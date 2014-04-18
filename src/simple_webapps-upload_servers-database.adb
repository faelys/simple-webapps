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

with GNAT.SHA1;
with Natools.GNAT_HMAC;
with Natools.GNAT_HMAC.SHA1;

separate (Simple_Webapps.Upload_Servers)
protected body Database is



   function Report (Key : URI_Key) return File_Refs.Immutable_Reference is
      Cursor : constant File_Maps.Cursor := Reports.Find (Key);
   begin
      if File_Maps.Has_Element (Cursor) then
         return File_Maps.Element (Cursor);
      else
         return File_Refs.Null_Immutable_Reference;
      end if;
   end Report;


   function Download (Key : URI_Key) return File_Refs.Immutable_Reference is
      Cursor : constant File_Maps.Cursor := Downloads.Find (Key);
   begin
      if File_Maps.Has_Element (Cursor) then
         return File_Maps.Element (Cursor);
      else
         return File_Refs.Null_Immutable_Reference;
      end if;
   end Download;


   function Path (Report : URI_Key) return String is
   begin
      return Ada.Directories.Compose (To_String (Directory), Report, "");
   end Path;


   function Path (Ref : File_Refs.Immutable_Reference) return String is
   begin
      return Path (Ref.Query.Data.Report);
   end Path;


   function Hex_Digest (Ref : File_Refs.Immutable_Reference) return String is
   begin
      return S_Expressions.To_String
           (S_Expressions.Encodings.Encode_Hex
              (S_Expressions.Encodings.Decode_Base64
                 (S_Expressions.To_Atom (Ref.Query.Data.Report),
                  Digit_62, Digit_63),
               S_Expressions.Encodings.Lower));
   end Hex_Digest;


   function Hash_Name (Ref : File_Refs.Immutable_Reference) return String is
      pragma Unreferenced (Ref);
   begin
      return "SHA-1";
   end Hash_Name;


   procedure Post_File
     (Request : in AWS.Status.Data;
      Report : out URI_Key)
   is
      package Hash renames GNAT.SHA1;
      package HMAC renames Natools.GNAT_HMAC.SHA1;

      Parameters : constant AWS.Parameters.List
        := AWS.Status.Parameters (Request);
      Local_Path : constant String
        := AWS.Parameters.Get (Parameters, "file");
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
         Target_Path : constant String := Path (Report);
      begin
         Ada.Directories.Copy_File (Local_Path, Target_Path);
      end Save_File;

      Download := S_Expressions.To_String
        (S_Expressions.Encodings.Encode_Base64
           (HMAC.Digest
              (To_String (HMAC_Key),
               S_Expressions.To_Atom (Report)),
            Digit_62,
            Digit_63));

      Insert_Ref :
      declare
         Ref : constant File_Refs.Immutable_Reference
           := File_Refs.Create (Create'Access);
      begin
         Reports.Insert (Report, Ref);
         Downloads.Insert (Download, Ref);
      end Insert_Ref;
   end Post_File;


   procedure Reset
     (New_Directory : in String;
      New_HMAC_Key : in String) is
   begin
      Directory := Hold (New_Directory);
      HMAC_Key := Hold (New_HMAC_Key);
      Reports.Clear;
      Downloads.Clear;
   end Reset;

end Database;
