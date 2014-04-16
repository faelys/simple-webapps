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

with GNAT.SHA1;
with Natools.GNAT_HMAC;
with Natools.GNAT_HMAC.SHA1;
with Natools.S_Expressions;
with Natools.S_Expressions.Encodings;

package body Simple_Webapps.Upload_Servers is

   package S_Expressions renames Natools.S_Expressions;

   package Hash renames GNAT.SHA1;
   package HMAC renames Natools.GNAT_HMAC.SHA1;
   Hash_Name : constant String := "SHA-1";

   Digit_62 : constant Ada.Streams.Stream_Element := Character'Pos ('-');
   Digit_63 : constant Ada.Streams.Stream_Element := Character'Pos ('_');
      --  Special digits for base-64 URI (RFC 4648)


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
               Post_File (Dispatcher.DB.Update.Data.all, Request, Report);
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
         Cursor : File_Maps.Cursor;
      begin
         pragma Assert (URI (URI'First) = '/');

         if URI = "/" then
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
         end if;

         if URI'Length < Key'Length + 1 then
            return AWS.Response.Acknowledge (AWS.Messages.S404);
         end if;

         Key := URI (URI'First + 1 .. URI'First + Key'Length);

         if URI'Length = Key'Length + 1 then
            Cursor := Dispatcher.DB.Query.Data.Reports.Find (Key);
            if File_Maps.Has_Element (Cursor) then
               return Response (File_Maps.Element (Cursor).Query.Data.all);
            end if;
         elsif URI (URI'First + Key'Length + 1) /= '/' then
            return AWS.Response.Acknowledge (AWS.Messages.S404);
         end if;

         Cursor := Dispatcher.DB.Query.Data.Download.Find (Key);

         if File_Maps.Has_Element (Cursor) then
            return AWS.Response.File
              ("application/octet-stream",
               Ada.Directories.Compose
                 (To_String (Dispatcher.DB.Query.Data.Directory),
                  File_Maps.Element (Cursor).Query.Data.Report,
                  ""));
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
         return
           (Directory => Hold (Directory),
            HMAC_Key => Hold (HMAC_Key),
            Reports => File_Maps.Empty_Map,
            Download => File_Maps.Empty_Map);
      end Create;
   begin
      Dispatcher.DB.Replace (Create'Access);
   end Reset;



   ------------------
   -- File Entries --
   ------------------

   function Response (F : File) return AWS.Response.Data is
      Digest : constant String
        := S_Expressions.To_String
           (S_Expressions.Encodings.Encode_Hex
              (S_Expressions.Encodings.Decode_Base64
                 (S_Expressions.To_Atom (F.Report), Digit_62, Digit_63),
               S_Expressions.Encodings.Lower));
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
   end Response;



   -------------------
   -- File Database --
   -------------------

   procedure Post_File
     (Self : in out Database;
      Request : in AWS.Status.Data;
      Report : out URI_Key)
   is
      Parameters : constant AWS.Parameters.List
        := AWS.Status.Parameters (Request);
      Local_Path : constant String := AWS.Parameters.Get (Parameters, "file");
      Name : constant String := AWS.Parameters.Get (Parameters, "file", 2);
      Download : URI_Key;

      function Create return File;

      function Create return File is
      begin
         return
           (Name => Hold (Name),
            Report => Report,
            Download => Download);
      end Create;
   begin
      Compute_Hash :
      declare
         package Stream_IO renames Ada.Streams.Stream_IO;

         Context : Hash.Context := Hash.Initial_Context;
         Block : Ada.Streams.Stream_Element_Array (1 .. 1024);
         Last : Ada.Streams.Stream_Element_Offset;
         Input : Stream_IO.File_Type;
      begin
         Stream_IO.Open (Input, Stream_IO.In_File, Local_Path);
         loop
            Stream_IO.Read (Input, Block, Last);
            exit when Last not in Block'Range;
            Hash.Update (Context, Block (Block'First .. Last));
         end loop;
         Stream_IO.Close (Input);

         Report := S_Expressions.To_String
           (S_Expressions.Encodings.Encode_Base64
              (Natools.GNAT_HMAC.Digest (Context), Digit_62, Digit_63));
      end Compute_Hash;

      Save_File :
      declare
         Target_Path : constant String
           := Ada.Directories.Compose (To_String (Self.Directory), Report, "");
      begin
         Ada.Directories.Copy_File (Local_Path, Target_Path);
      end Save_File;

      Download := S_Expressions.To_String
        (S_Expressions.Encodings.Encode_Base64
           (HMAC.Digest
              (To_String (Self.HMAC_Key),
               S_Expressions.To_Atom (Report)),
            Digit_62,
            Digit_63));

      Insert_Ref :
      declare
         Ref : constant File_Refs.Reference
           := File_Refs.Create (Create'Access);
      begin
         Self.Reports.Insert (Report, Ref);
         Self.Download.Insert (Download, Ref);
      end Insert_Ref;
   end Post_File;

end Simple_Webapps.Upload_Servers;
