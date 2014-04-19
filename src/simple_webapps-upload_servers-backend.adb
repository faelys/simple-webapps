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

with Ada.Streams.Stream_IO;

with GNAT.SHA1;

with Natools.GNAT_HMAC;
with Natools.GNAT_HMAC.SHA1;
with Natools.S_Expressions.Encodings;

separate (Simple_Webapps.Upload_Servers)
package body Backend is

   package Hash renames GNAT.SHA1;
   package HMAC renames Natools.GNAT_HMAC.SHA1;
   Hash_Name : constant String := "SHA-1";

   Digit_62 : constant Ada.Streams.Stream_Element := Character'Pos ('-');
   Digit_63 : constant Ada.Streams.Stream_Element := Character'Pos ('_');
      --  Special digits for base-64 URI (RFC 4648)


   --------------------
   -- File Accessors --
   --------------------

   function Is_Empty (Self : File) return Boolean is
   begin
      return Self.Ref.Is_Empty;
   end Is_Empty;


   function Name (Self : File) return String is
   begin
      return To_String (Self.Ref.Query.Data.Name);
   end Name;


   function Path
     (Directory : Atom_Refs.Immutable_Reference;
      Report : URI_Key)
     return String is
   begin
      return Ada.Directories.Compose
        (S_Expressions.To_String (Directory.Query.Data.all),
         Report,
         "");
   end Path;


   function Path (Self : File) return String is
   begin
      return Path
        (Self.Ref.Query.Data.Directory,
         Self.Ref.Query.Data.Report);
   end Path;


   function Report (Self : File) return URI_Key is
   begin
      return Self.Ref.Query.Data.Report;
   end Report;


   function Download (Self : File) return URI_Key is
   begin
      return Self.Ref.Query.Data.Download;
   end Download;


   function Hash_Type (Self : File) return String is
      pragma Unreferenced (Self);
   begin
      return Hash_Name;
   end Hash_Type;


   function Hex_Digest (Self : File) return String is
   begin
      return S_Expressions.To_String
           (S_Expressions.Encodings.Encode_Hex
              (S_Expressions.Encodings.Decode_Base64
                 (S_Expressions.To_Atom (Self.Ref.Query.Data.Report),
                  Digit_62, Digit_63),
               S_Expressions.Encodings.Lower));
   end Hex_Digest;


   protected body Database is

      function Report (Key : URI_Key) return File is
         Cursor : constant File_Maps.Cursor := Files.Reports.Find (Key);
      begin
         if File_Maps.Has_Element (Cursor) then
            return File_Maps.Element (Cursor);
         else
            return (Ref => File_Refs.Null_Immutable_Reference);
         end if;
      end Report;


      function Download (Key : URI_Key) return File is
         Cursor : constant File_Maps.Cursor := Files.Downloads.Find (Key);
      begin
         if File_Maps.Has_Element (Cursor) then
            return File_Maps.Element (Cursor);
         else
            return (Ref => File_Refs.Null_Immutable_Reference);
         end if;
      end Download;


      procedure Post_File
        (Request : in AWS.Status.Data;
         Report : out URI_Key)
      is
         Parameters : constant AWS.Parameters.List
           := AWS.Status.Parameters (Request);
         Local_Path : constant String
           := AWS.Parameters.Get (Parameters, "file");
         Name : constant String := AWS.Parameters.Get (Parameters, "file", 2);
         Download : URI_Key;

         function Create return File_Data;

         function Create return File_Data is
         begin
            return
              (Name => Hold (Name),
               Report => Report,
               Download => Download,
               Directory => Directory);
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
            Target_Path : constant String := Path (Directory, Report);
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
            F : constant File := (Ref => File_Refs.Create (Create'Access));
         begin
            Files.Reports.Insert (Report, F);
            Files.Downloads.Insert (Download, F);
         end Insert_Ref;
      end Post_File;


      procedure Reset
        (New_Directory : in String;
         New_HMAC_Key : in String)
      is
         function Create return S_Expressions.Atom;

         function Create return S_Expressions.Atom is
         begin
            return S_Expressions.To_Atom (New_Directory);
         end Create;
      begin
         Directory := Atom_Refs.Create (Create'Access);
         HMAC_Key := Hold (New_HMAC_Key);
         Files.Reports.Clear;
         Files.Downloads.Clear;
      end Reset;

   end Database;

end Backend;
