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

   function File_List (DB : Backend.Database) return AWS.Response.Data;
      --  Create a list of all files

   function Report (File : Backend.File) return AWS.Response.Data;
      --  Create a report page for the given file

   function Server_Log (DB : Backend.Database) return AWS.Response.Data;
      --  Create a list of recent server event log entries

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
               Expiration : Ada.Calendar.Time := Ada.Calendar.Clock;
            begin
               pragma Assert
                 (AWS.Attachments.Count (Attachments) = 1
                  and then AWS.Parameters.Get (Parameters, "file")
                    = AWS.Attachments.Local_Filename (File_Att)
                  and then AWS.Parameters.Get (Parameters, "file", 2)
                    = AWS.Attachments.Filename (File_Att));

               Compute_Expiration :
               declare
                  use type Ada.Calendar.Time;

                  Exp_Str : constant String
                    := AWS.Parameters.Get (Parameters, "expire");
                  Unit_Str : constant String
                    := AWS.Parameters.Get (Parameters, "expire_unit");
                  Exp_Delay : Natural;
               begin
                  Exp_Delay := Natural'Value (Exp_Str);

                  if Unit_Str = "minutes" then
                     Exp_Delay := Exp_Delay * 60;
                  elsif Unit_Str = "hours" then
                     Exp_Delay := Exp_Delay * 3600;
                  elsif Unit_Str = "days" then
                     Exp_Delay := Exp_Delay * 86_400;
                  elsif Unit_Str = "weeks" then
                     Exp_Delay := Exp_Delay * 604_800;
                  end if;

                  Expiration := Expiration + Duration (Exp_Delay);
               exception
                  when Constraint_Error =>
                     Expiration := Ada.Calendar.Clock + 300.0;
               end Compute_Expiration;

               Dispatcher.DB.Update.Data.Add_File
                 (AWS.Parameters.Get (Parameters, "file"),
                  AWS.Parameters.Get (Parameters, "file", 2),
                  AWS.Parameters.Get (Parameters, "comment"),
                  AWS.Attachments.Content_Type (File_Att),
                  Expiration,
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
         elsif URI = "/list" then
            return File_List (Dispatcher.DB.Query.Data.all);
         elsif URI = "/log" then
            return Server_Log (Dispatcher.DB.Query.Data.all);
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

   function File_List (DB : Backend.Database) return AWS.Response.Data is
      Table : Ada.Strings.Unbounded.Unbounded_String;

      procedure Process (File : in Backend.File);

      procedure Process (File : in Backend.File) is
      begin
         Ada.Strings.Unbounded.Append
           (Table,
            "<tr>"
            & "<td><a href=""/" & File.Report & """>"
            & File.Report & "</a></td>"
            & "<td>" & HTML_Escape (File.Name) & "</td>"
            & "<td>" & Ada.Calendar.Formatting.Image
                         (File.Expiration) & "</td>"
            & "</tr>");
      end Process;

      Result : Boolean;
      pragma Unreferenced (Result);
   begin
      Result := DB.Iterate (Process'Access);

      return AWS.Response.Build
        ("text/html",
         "<html><head><title>File List</title></head>"
         & "<body><h1>File List</h1>"
         & "<table><tr>"
         & "<th>Link</th>"
         & "<th>File name</th>"
         & "<th>Expiration</th>"
         & "</tr>"
         & Ada.Strings.Unbounded.To_String (Table)
         & "</table>"
         & "<p><a href=""/"">Back to upload page</a></p>"
         & "</body></html>");
   end File_List;


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


   function Server_Log (DB : Backend.Database) return AWS.Response.Data is
      Table : Ada.Strings.Unbounded.Unbounded_String;

      procedure Process (Time : in Ada.Calendar.Time; Message : in String);

      procedure Process (Time : in Ada.Calendar.Time; Message : in String) is
      begin
         Ada.Strings.Unbounded.Append
           (Table,
            "<tr>"
            & "<td>" & Ada.Calendar.Formatting.Image (Time) & "</td>"
            & "<td>" & HTML_Escape (Message) & "</td>"
            & "</tr>");
      end Process;

      Result : Boolean;
      pragma Unreferenced (Result);
   begin
      Result := DB.Iterate_Logs (Process'Access);

      return AWS.Response.Build
        ("text/html",
         "<html><head><title>Server Log</title></head>"
         & "<body><h1>Server Log</h1>"
         & "<table><tr>"
         & "<th>Time</th>"
         & "<th>Message</th>"
         & "</tr>"
         & Ada.Strings.Unbounded.To_String (Table)
         & "</table>"
         & "<p><a href=""/"">Back to upload page</a></p>"
         & "</body></html>");
   end Server_Log;


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
         & "<p><label>Expires in "
         & "<input name=""expire"" value=""1""></label>"
         & "<select name=""expire_unit"">"
         & "<option>seconds</options>"
         & "<option>minutes</options>"
         & "<option selected>hours</options>"
         & "<option>days</options>"
         & "<option>weeks</options>"
         & "</select></p>"
         & "<p><label>Comment:<br>"
         & "<textarea name=""comment"" cols=""80"" rows=""10""></textarea>"
         & "</label><?p>"
         & "<p><input name=""submit"" value=""Send"""
         & " type=""submit""></p>"
         & "</form>"
         & "<h2>Maintenance</h2>"
         & "<form action=""/purge"" method=""post"">"
         & "<p><input name=""submit"" value=""Purge"" type=""submit""></p>"
         & "</form></body></html>");
   end Upload_Form;

end Simple_Webapps.Upload_Servers;
