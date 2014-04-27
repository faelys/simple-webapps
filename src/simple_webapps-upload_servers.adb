------------------------------------------------------------------------------
-- Copyright (c) 2014, Natacha Porté                                        --
--                                                                          --
-- Permission to use, copy, modify, and distribute this software for any    --
-- purpose with or without fee is hereby granted, provided that the above   --
-- copyright notice and this permission notice appear in all copies.        --
--                                                                          --
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES --
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF         --
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR  --
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES   --
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN    --
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF  --
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.           --
------------------------------------------------------------------------------

with Ada.Calendar.Formatting;
with Ada.Directories;

with AWS.Attachments;
with AWS.Messages;
with AWS.Parameters;

with Natools.S_Expressions.File_Readers;

package body Simple_Webapps.Upload_Servers is

   package body Backend is separate;

   function Report (File : Backend.File) return AWS.Response.Data;
      --  Create a report page for the given file

   function Upload_Form (DB : Backend.Database) return AWS.Response.Data;
      --  Create the main upload form



   ------------------------
   -- Request Dispatcher --
   ------------------------

   overriding function Dispatch
     (Dispatcher : Handler;
      Request : AWS.Status.Data)
     return AWS.Response.Data is
   begin
      case AWS.Status.Method (Request) is
         when AWS.Status.POST =>
            if AWS.Status.URI (Request) = "/purge" then
               Dispatcher.DB.Update.Data.Purge_Expired;
               return AWS.Response.URL ("/");
            end if;

            declare
               Attachments : constant AWS.Attachments.List
                 := AWS.Status.Attachments (Request);
               File_Att : constant AWS.Attachments.Element
                 := AWS.Attachments.Get (Attachments, 1);
               Parameters : constant AWS.Parameters.List
                 := AWS.Status.Parameters (Request);
               Report : URI_Key;
            begin
               pragma Assert
                 (AWS.Attachments.Count (Attachments) = 1
                  and then AWS.Parameters.Get (Parameters, "file")
                    = AWS.Attachments.Local_Filename (File_Att)
                  and then AWS.Parameters.Get (Parameters, "file", 2)
                    = AWS.Attachments.Filename (File_Att));

               Dispatcher.DB.Update.Data.Add_File
                 (AWS.Parameters.Get (Parameters, "file"),
                  AWS.Parameters.Get (Parameters, "file", 2),
                  AWS.Parameters.Get (Parameters, "comment"),
                  AWS.Attachments.Content_Type (File_Att),
                  Ada.Calendar."+" (Ada.Calendar.Clock, 3600.0),
                  Report);
               return AWS.Response.URL ('/' & Report);
            end;

         when AWS.Status.GET =>
            null;  --  processed below

         when others =>
            return AWS.Response.Acknowledge (AWS.Messages.S405);
      end case;

      declare
         URI : constant String := AWS.Status.URI (Request);
         Key : URI_Key;
         File : Backend.File;
      begin
         pragma Assert (URI (URI'First) = '/');

         if URI = "/" then
            return Upload_Form (Dispatcher.DB.Query.Data.all);
         end if;

         if URI'Length < Key'Length + 1 then
            return AWS.Response.Acknowledge (AWS.Messages.S404);
         end if;

         Key := URI (URI'First + 1 .. URI'First + Key'Length);

         if URI'Length = Key'Length + 1 then
            File := Dispatcher.DB.Query.Data.Report (Key);
            if not File.Is_Empty then
               return Report (File);
            end if;
         elsif URI (URI'First + Key'Length + 1) /= '/' then
            return AWS.Response.Acknowledge (AWS.Messages.S404);
         end if;

         File := Dispatcher.DB.Query.Data.Download (Key);

         if not File.Is_Empty then
            return AWS.Response.File (File.MIME_Type, File.Path);
         else
            return AWS.Response.Acknowledge (AWS.Messages.S404);
         end if;
      end;
   end Dispatch;


   overriding function Clone (Dispatcher : Handler) return Handler is
   begin
      return Dispatcher;
   end Clone;


   procedure Reset
     (Dispatcher : in out Handler;
      Config_File : in String)
   is
      function Create return Backend.Database;

      Reader : S_Expressions.File_Readers.S_Reader
        := S_Expressions.File_Readers.Reader (Config_File);

      function Create return Backend.Database is
      begin
         return DB : Backend.Database do
            DB.Reset (Reader);
         end return;
      end Create;
   begin
      Dispatcher.DB.Replace (Create'Access);
   end Reset;



   ----------------------------
   -- HTML Page Constructors --
   ----------------------------

   function Report (File : Backend.File) return AWS.Response.Data is
      DL_URI : constant String := '/' & File.Download & '/' & File.Name;
   begin
      return AWS.Response.Build
        ("text/html",
         "<html><head><title>File Report</title></head>"
         & "<body><h1>File Report</h1>"
         & "<ul>"
         & "<li>Uploaded as " & File.Name & "</li>"
         & "<li>File type: " & File.MIME_Type & "</li>"
         & "<li>" & File.Hash_Type & " digest: " & File.Hex_Digest & "</li>"
         & "<li>Upload date: "
         & Ada.Calendar.Formatting.Image (File.Upload) & "</li>"
         & "<li>Expiration date: "
         & Ada.Calendar.Formatting.Image (File.Expiration) & "</li>"
         & "<li>Download link: <a href=""" & DL_URI & """>"
         & DL_URI & "</a></li>"
         & "<li>Comment: " & HTML_Escape (File.Comment) & "</li>"
         & "</ul>"
         & "<p><a href=""/"">Back to upload page</a></p>"
         & "</body></html>");
   end Report;


   function Upload_Form (DB : Backend.Database) return AWS.Response.Data is
      pragma Unreferenced (DB);
   begin
      return AWS.Response.Build
        ("text/html",
         "<html><head><title>File Upload</title></head>"
         & "<body><h1>File Upload</h1>"
         & "<form enctype=""multipart/form-data"" action=""/post"""
         & " method=""post"">"
         & "<p><input name=""file"" type=""file""></p>"
         & "<p><textarea name=""comment"" cols=""80"" rows=""10""></textarea>"
         & "<p><input name=""submit"" value=""Send"""
         & " type=""submit""></p>"
         & "</form>"
         & "<h2>Maintenance</h2>"
         & "<form action=""/purge"" method=""post"">"
         & "<p><input name=""submit"" value=""Purge"" type=""submit""></p>"
         & "</form></body></html>");
   end Upload_Form;

end Simple_Webapps.Upload_Servers;
