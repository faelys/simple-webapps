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

with Ada.Calendar.Arithmetic;
with Ada.Calendar.Formatting;
with Ada.Strings.Fixed;

with AWS.Attachments;
with AWS.Messages;
with AWS.Parameters;

with Templates_Parser;

with Natools.S_Expressions.File_Readers;

package body Simple_Webapps.Upload_Servers is

   package body Backend is separate;

   function Dump_Form (Request : AWS.Status.Data) return AWS.Response.Data;

   function File_List (DB : Backend.Database) return AWS.Response.Data;
      --  Create a list of all files

   function Report
     (File : Backend.File;
      Debug : Boolean;
      Template : String := "")
     return AWS.Response.Data;
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
--  GNAT bugbox generator:
--          elsif Dispatcher.DB.Query.Data.Debug_Activated
--            and then AWS.Status.URI (Request) = "/dump-form"
--          then
--  using the almost equivalent variant:
            elsif AWS.Status.URI (Request) = "/dump-form"
              and then Dispatcher.DB.Query.Data.Debug_Activated
            then
               return Dump_Form (Request);
            end if;

            declare
               function Compute_Expiration (Path : String)
                 return Ada.Calendar.Time;

               Attachments : constant AWS.Attachments.List
                 := AWS.Status.Attachments (Request);
               Parameters : constant AWS.Parameters.List
                 := AWS.Status.Parameters (Request);

               function Compute_Expiration (Path : String)
                 return Ada.Calendar.Time is
               begin
                  return Expiration
                    (Dispatcher.DB.Query.Data.all,
                     Natural'Value (AWS.Parameters.Get (Parameters, "expire")),
                     AWS.Parameters.Get (Parameters, "expire_unit"),
                     Ada.Directories.Size (Path));
               exception
                  when Constraint_Error =>
                     return Ada.Calendar."+" (Ada.Calendar.Clock, 300.0);
               end Compute_Expiration;

               Report : URI_Key;
            begin
               if AWS.Attachments.Count (Attachments) = 1
                 and then AWS.Parameters.Get (Parameters, "file")
                   = AWS.Attachments.Local_Filename
                      (AWS.Attachments.Get (Attachments, 1))
                 and then AWS.Parameters.Get (Parameters, "file", 2)
                   = AWS.Attachments.Filename
                      (AWS.Attachments.Get (Attachments, 1))
               then
                  --  Direct file upload through AWS

                  Dispatcher.DB.Update.Data.Add_File
                    (AWS.Parameters.Get (Parameters, "file"),
                     AWS.Parameters.Get (Parameters, "file", 2),
                     AWS.Parameters.Get (Parameters, "comment"),
                     AWS.Attachments.Content_Type
                       (AWS.Attachments.Get (Attachments, 1)),
                     Compute_Expiration
                       (AWS.Parameters.Get (Parameters, "file")),
                     Report);
                  return AWS.Response.URL ('/' & Report);

               elsif AWS.Parameters.Exist (Parameters, "file.path")
                 and then Ada.Directories.Exists
                    (AWS.Parameters.Get (Parameters, "file.path"))
               then
                  --  File upload through nginx upload module

                  Dispatcher.DB.Update.Data.Add_File
                    (AWS.Parameters.Get (Parameters, "file.path"),
                     AWS.Parameters.Get (Parameters, "file.name"),
                     AWS.Parameters.Get (Parameters, "comment"),
                     AWS.Parameters.Get (Parameters, "file.content_type"),
                     Compute_Expiration
                       (AWS.Parameters.Get (Parameters, "file.path")),
                     Report);
                  return AWS.Response.URL ('/' & Report);

               else
                  return AWS.Response.Build
                    ("text/html",
                     "<html><head><title>Bad Post</title></head>"
                     & "<body><h1>Bad Post</h1>"
                     & "<p>Unable to process form fields.</p>"
                     & "<p><a href=""/"">Back</a></p>"
                     & "</body></html>");
               end if;
            end;

         when AWS.Status.GET =>
            null;  --  processed below

         when others =>
            return AWS.Response.Acknowledge (AWS.Messages.S405);
      end case;

      declare
         DB_Accessor : constant Database_Refs.Accessor := Dispatcher.DB.Query;
         URI : constant String := AWS.Status.URI (Request);
         Key : URI_Key;
         File : Backend.File;
      begin
         pragma Assert (URI (URI'First) = '/');

         if URI = "/" then
            return Upload_Form (DB_Accessor.Data.all);
         elsif DB_Accessor.Data.Debug_Activated then
            if URI = "/list" then
               return File_List (DB_Accessor.Data.all);
            end if;
         end if;

         if URI'Length < Key'Length + 1 then
            return AWS.Response.Acknowledge (AWS.Messages.S404);
         end if;

         Key := URI (URI'First + 1 .. URI'First + Key'Length);

         if URI'Length = Key'Length + 1 then
            File := DB_Accessor.Data.Report (Key);
            if not File.Is_Empty then
               return Report
                 (File,
                  DB_Accessor.Data.Debug_Activated,
                  DB_Accessor.Data.Report_Template);
            end if;
         elsif URI (URI'First + Key'Length + 1) /= '/' then
            return AWS.Response.Acknowledge (AWS.Messages.S404);
         end if;

         File := DB_Accessor.Data.Download (Key);

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
      Config_File : in String;
      Debug : in Boolean := False)
   is
      function Create_DB return Backend.Database;
      function Create_Timer return Natools.Cron.Cron_Entry;

      Reader : S_Expressions.File_Readers.S_Reader
        := S_Expressions.File_Readers.Reader (Config_File);

      function Create_DB return Backend.Database is
      begin
         return DB : Backend.Database do
            DB.Reset (Reader, Debug);
         end return;
      end Create_DB;

      function Create_Timer return Natools.Cron.Cron_Entry is
      begin
         return Natools.Cron.Create
           (900.0,
            Purge_Callback'(DB => Dispatcher.DB));
      end Create_Timer;
   begin
      Dispatcher.DB.Replace (Create_DB'Access);
      Dispatcher.Timer.Replace (Create_Timer'Access);
   end Reset;


   function Expiration
     (DB : Backend.Database;
      Request_Number : Natural;
      Request_Unit : String;
      Size : Ada.Directories.File_Size)
     return Ada.Calendar.Time
   is
      use type Ada.Calendar.Time;

      Req_Delay, Max_Delay : Size_Time;
   begin
      Req_Delay := Size_Time (Request_Number);

      if Request_Unit = "minutes" then
         Req_Delay := Req_Delay * 60;
      elsif Request_Unit = "hours" then
         Req_Delay := Req_Delay * 3600;
      elsif Request_Unit = "days" then
         Req_Delay := Req_Delay * 86_400;
      elsif Request_Unit = "weeks" then
         Req_Delay := Req_Delay * 604_800;
      end if;

      Max_Delay := DB.Max_Expiration / Size_Time (Size);

      if Req_Delay > Max_Delay then
         Req_Delay := Max_Delay;
      end if;

      return Ada.Calendar.Arithmetic."+"
        (Ada.Calendar.Clock,
         Ada.Calendar.Arithmetic.Day_Count (Req_Delay / 86_400))
        + Duration (Req_Delay mod 86_400);
   end Expiration;


   overriding procedure Run (Self : in out Purge_Callback) is
   begin
      Self.DB.Update.Data.Purge_Expired;
   end Run;



   ----------------------------
   -- HTML Page Constructors --
   ----------------------------

   function Dump_Form (Request : AWS.Status.Data) return AWS.Response.Data is
      Page : Ada.Strings.Unbounded.Unbounded_String;

      Attachments : constant AWS.Attachments.List
        := AWS.Status.Attachments (Request);
      Attachment : AWS.Attachments.Element;
      Parameters : constant AWS.Parameters.List
        := AWS.Status.Parameters (Request);
   begin
      Ada.Strings.Unbounded.Append (Page,
        "<html><head><title>Form Dump</title></head>"
        & "<body><h1>Form Dump</h1><h2>Parameters</h2><ol>");

      for I in 1 .. AWS.Parameters.Count (Parameters) loop
         Ada.Strings.Unbounded.Append (Page,
            "<li>" & AWS.Parameters.Get_Name (Parameters, I)
            & ": " & AWS.Parameters.Get_Value (Parameters, I) & "</li>");
      end loop;

      Ada.Strings.Unbounded.Append (Page, "</ol><h2>Attachments</h2><ol>");

      for I in 1 .. AWS.Attachments.Count (Attachments) loop
         Attachment := AWS.Attachments.Get (Attachments, I);
         Ada.Strings.Unbounded.Append (Page,
            "<li>" & AWS.Attachments.Filename (Attachment) & ": "
            & AWS.Attachments.Content_Type (Attachment) & " at "
            & AWS.Attachments.Local_Filename (Attachment) & "</li>");
      end loop;

      Ada.Strings.Unbounded.Append (Page, "</ol></body></html>");

      return AWS.Response.Build ("test/html", To_String (Page));
   end Dump_Form;


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


   function Report
     (File : Backend.File;
      Debug : Boolean;
      Template : String := "")
     return AWS.Response.Data
   is
      function Image_Diff (Future, Now : Ada.Calendar.Time) return String;

      function Image_Diff (Future, Now : Ada.Calendar.Time) return String is
         use Ada.Calendar.Arithmetic;
         use Ada.Calendar.Formatting;

         Days : Day_Count;
         Seconds : Duration;
         Leap_Seconds : Leap_Seconds_Count;
         Sub_Sec : Second_Duration;
         Sec : Second_Number;
         Min : Minute_Number;
         Hour : Hour_Number;
      begin
         Difference (Future, Now, Days, Seconds, Leap_Seconds);
         Split (Seconds + Duration (Leap_Seconds), Hour, Min, Sec, Sub_Sec);

         if Days >= 10 then
            if Hour >= 12 then
               Days := Days + 1;
            end if;
            return Day_Count'Image (Days) & 'd';

         elsif Days > 0 then
            if Min >= 30 then
               if Hour = Hour_Number'Last then
                  return Day_Count'Image (Days + 1) & 'd';
               else
                  Hour := Hour + 1;
               end if;
            end if;
            return Day_Count'Image (Days) & 'd'
              & Hour_Number'Image (Hour) & 'h';

         elsif Hour > 0 then
            if Sec >= 30 then
               if Min = Minute_Number'Last then
                  return Hour_Number'Image (Hour + 1) & 'h';
               else
                  Min := Min + 1;
               end if;
            end if;
            return Hour_Number'Image (Hour) & 'h'
              & Minute_Number'Image (Min) & 'm';

         else
            return Minute_Number'Image (Min) & 'm'
              & Second_Number'Image (Sec) & 's';
         end if;
      end Image_Diff;

      DL : String_Holder;
   begin
      if Template = "" then
         if Debug then
            declare
               DL_URI : constant String
                 := '/' & File.Download & '/' & File.Name;
            begin
               DL := Hold ("<li>Download link: <a href=""" & DL_URI & """>"
                 & DL_URI & "</a></li>");
            end;
         end if;

         return AWS.Response.Build
           ("text/html",
            "<html><head><title>File Report</title></head>"
            & "<body><h1>File Report</h1>"
            & "<ul>"
            & "<li>Uploaded as " & HTML_Escape (File.Name) & "</li>"
            & "<li>File type: " & HTML_Escape (File.MIME_Type) & "</li>"
            & "<li>" & File.Hash_Type & " digest: " & File.Hex_Digest & "</li>"
            & "<li>Upload date: "
            & Ada.Calendar.Formatting.Image (File.Upload) & "</li>"
            & "<li>Expiration date: "
            & Ada.Calendar.Formatting.Image (File.Expiration)
            & " (in " & Image_Diff (File.Expiration, Ada.Calendar.Clock)
            & ")</li>"
            & To_String (DL)
            & "<li>Comment: " & HTML_Escape (File.Comment) & "</li>"
            & "</ul>"
            & "<p><a href=""/"">Back to upload page</a></p>"
            & "</body></html>");
      else
         declare
            use Templates_Parser;
            Debug_Assoc : constant Translate_Table
              := (Assoc ("DEBUG", True),
                  Assoc ("DOWNLOAD_KEY", File.Download),
                  Assoc ("DOWNLOAD_PATH",
                     '/' & File.Download & '/' & File.Name));
            Core_Assoc : constant Translate_Table
              := (Assoc ("NAME", File.Name),
                  Assoc ("MIME_TYPE", File.MIME_Type),
                  Assoc ("DIGEST_TYPE", File.Hash_Type),
                  Assoc ("DIGEST", File.Hex_Digest),
                  Assoc ("UPLOAD",
                     Ada.Calendar.Formatting.Image (File.Upload)),
                  Assoc ("EXPIRATION",
                     Ada.Calendar.Formatting.Image (File.Expiration)),
                  Assoc ("EXPIRATION_DELAY",
                     Image_Diff (File.Expiration, Ada.Calendar.Clock)),
                  Assoc ("COMMENT", File.Comment));
         begin
            if Debug then
               return AWS.Response.Build
                 ("text/html",
                  String'(Parse (Template, Core_Assoc & Debug_Assoc)));
            else
               return AWS.Response.Build
                 ("text/html",
                  String'(Parse (Template, Core_Assoc
                    & Assoc ("DEBUG", False))));
            end if;
         end;
      end if;
   end Report;


   function Upload_Form (DB : Backend.Database) return AWS.Response.Data is
      Template : constant String := DB.Index_Template;
      Raw_Max_Exp : constant Size_Time := DB.Max_Expiration;
      Max_Exp : Size_Time := Raw_Max_Exp;
      Time_Suffix : Character := 's';
      Size_Prefix : Character := ' ';
   begin
      if Max_Exp mod 60 = 0 then
         Max_Exp := Max_Exp / 60;
         Time_Suffix := 'm';

         if Max_Exp mod 60 = 0 then
            Max_Exp := Max_Exp / 60;
            Time_Suffix := 'h';

            if Max_Exp mod 24 = 0 then
               Max_Exp := Max_Exp / 24;
               Time_Suffix := 'd';

               if Max_Exp mod 7 = 0 then
                  Max_Exp := Max_Exp / 7;
                  Time_Suffix := 'w';
               end if;
            end if;
         end if;
      end if;

      if Max_Exp mod 1024 = 0 then
         Max_Exp := Max_Exp / 1024;
         Size_Prefix := 'k';

         if Max_Exp mod 1024 = 0 then
            Max_Exp := Max_Exp / 1024;
            Size_Prefix := 'M';

            if Max_Exp mod 1024 = 0 then
               Max_Exp := Max_Exp / 1024;
               Size_Prefix := 'G';

               if Max_Exp mod 1024 = 0 then
                  Max_Exp := Max_Exp / 1024;
                  Size_Prefix := 'T';
               end if;
            end if;
         end if;
      end if;

      if Template = "" then
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
            & "<p>Maximum expiration is" & Size_Time'Image (Max_Exp)
            & ' ' & Size_Prefix & "B." & Time_Suffix & " / (file size)</p>"
            & "<p><label>Comment:<br>"
            & "<textarea name=""comment"" cols=""80"" rows=""10""></textarea>"
            & "</label></p>"
            & "<p><input name=""submit"" value=""Send"""
            & " type=""submit""></p>"
            & "</form>"
            & "<h2>Maintenance</h2>"
            & "<form action=""/purge"" method=""post"">"
            & "<p><input name=""submit"" value=""Purge"" type=""submit""></p>"
            & "</form></body></html>");
      else
         return AWS.Response.Build
           ("text/html",
            String'(Templates_Parser.Parse (Template,
              (Templates_Parser.Assoc ("MAX_EXPIRATION_BYTE_SECONDS",
                  Ada.Strings.Fixed.Trim
                    (Size_Time'Image (Raw_Max_Exp),
                     Ada.Strings.Left)),
               Templates_Parser.Assoc ("MAX_EXPIRATION",
                  Ada.Strings.Fixed.Trim
                    (Size_Time'Image (Max_Exp),
                     Ada.Strings.Left)),
               Templates_Parser.Assoc ("MAX_EXPIRATION_UNIT",
                  Ada.Strings.Fixed.Trim
                    (Size_Prefix & "B." & Time_Suffix,
                     Ada.Strings.Left)),
               Templates_Parser.Assoc ("MAX_EXPIRATION_PREFIX",
                 (1 => Size_Prefix)),
               Templates_Parser.Assoc ("MAX_EXPIRATION_SUFFIX",
                 (1 => Time_Suffix))))));
      end if;
   end Upload_Form;

end Simple_Webapps.Upload_Servers;
