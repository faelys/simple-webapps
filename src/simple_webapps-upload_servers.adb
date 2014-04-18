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

with Ada.Directories;
with Ada.Streams.Stream_IO;

with AWS.Messages;
with AWS.Parameters;

with Natools.S_Expressions;
with Natools.S_Expressions.Encodings;

package body Simple_Webapps.Upload_Servers is

   package S_Expressions renames Natools.S_Expressions;

   protected body Database is separate;

   function Report
     (F : File;
      Hash_Name, Digest : String)
     return AWS.Response.Data;
      --  Create a report page for the given file

   function Upload_Form (DB : Database) return AWS.Response.Data;
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
            declare
               Report : URI_Key;
            begin
               Dispatcher.DB.Update.Data.Post_File (Request, Report);
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
         Ref : File_Refs.Immutable_Reference;
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
            Ref := Dispatcher.DB.Query.Data.Report (Key);
            if not Ref.Is_Empty then
               return Report
                 (Ref.Query.Data.all,
                  Dispatcher.DB.Query.Data.Hash_Name (Ref),
                  Dispatcher.DB.Query.Data.Hex_Digest (Ref));
            end if;
         elsif URI (URI'First + Key'Length + 1) /= '/' then
            return AWS.Response.Acknowledge (AWS.Messages.S404);
         end if;

         Ref := Dispatcher.DB.Query.Data.Download (Key);

         if not Ref.Is_Empty then
            return AWS.Response.File
              ("application/octet-stream",
               Dispatcher.DB.Query.Data.Path (Ref));
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
      Directory : in String;
      HMAC_Key : in String)
   is
      function Create return Database;

      function Create return Database is
      begin
         return DB : Database do
            DB.Reset (Directory, HMAC_Key);
         end return;
      end Create;
   begin
      Dispatcher.DB.Replace (Create'Access);
   end Reset;



   ----------------------------
   -- HTML Page Constructors --
   ----------------------------

   function Report
     (F : File;
      Hash_Name, Digest : String)
     return AWS.Response.Data
   is
      DL_URI : constant String
        := '/' & F.Download & '/' & To_String (F.Name);
   begin
      return AWS.Response.Build
        ("text/html",
         "<html><head><title>File Report</title></head>"
         & "<body><h1>File Report</h1>"
         & "<ul>"
         & "<li>Uploaded as " & To_String (F.Name) & "</li>"
         & "<li>" & Hash_Name & " digest: " & Digest & "</li>"
         & "<li>Download link: <a href=""" & DL_URI & """>"
         & DL_URI & "</a></li>"
         & "<p><a href=""/"">Back to upload page</a></p>"
         & "</body></html>");
   end Report;


   function Upload_Form (DB : Database) return AWS.Response.Data is
      pragma Unreferenced (DB);
   begin
      return AWS.Response.Build
        ("text/html",
         "<html><head><title>File Upload</title></head>"
         & "<body><h1>File Upload</h1>"
         & "<form enctype=""multipart/form-data"" action=""/post"""
         & " method=""post"">"
         & "<p><input name=""file"" type=""file""></p>"
         & "<p><input name=""submit"" value=""Send"""
         & " type=""submit""></p>"
         & "</form></body></html>");
   end Upload_Form;

end Simple_Webapps.Upload_Servers;
