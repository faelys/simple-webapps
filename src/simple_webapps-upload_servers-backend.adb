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

with Ada.Streams;

with GNAT.SHA1;

with Natools.GNAT_HMAC;
with Natools.GNAT_HMAC.SHA1;
with Natools.S_Expressions.Encodings;
with Natools.S_Expressions.File_Readers;
with Natools.S_Expressions.File_Writers;
with Natools.S_Expressions.Interpreter_Loop;
with Natools.S_Expressions.Printers.Pretty.Config;

with Simple_Webapps.Commands.Upload_Servers;

separate (Simple_Webapps.Upload_Servers)
package body Backend is

   package Commands renames Simple_Webapps.Commands.Upload_Servers;

   package Hash renames GNAT.SHA1;
   package HMAC renames Natools.GNAT_HMAC.SHA1;
   Hash_Name : constant String := "SHA-1";

   Digit_62 : constant Ada.Streams.Stream_Element := Character'Pos ('-');
   Digit_63 : constant Ada.Streams.Stream_Element := Character'Pos ('_');
      --  Special digits for base-64 URI (RFC 4648)

   procedure Execute
     (State : in out Config_Data;
      Context : in Natools.Meaningless_Type;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class);
      --  Execute S-expression commands for configuration data interpreter

   procedure Execute
     (State : in out File_Data;
      Context : in Natools.Meaningless_Type;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class);
      --  Execute S-expression commands for file data interpreter

   procedure Set_Max_Expiration
     (Max_Expiration : in out Size_Time;
      Expression : in out S_Expressions.Lockable.Descriptor'Class);
      --  Process Expression and update Max_Expiration with parsed value

   procedure Write
     (Self : in File_Data;
      Output : in out S_Expressions.Printers.Printer'Class);
      --  Serialize the given file data into Output


   --------------------
   -- File Entry I/O --
   --------------------

   procedure Execute
     (State : in out File_Data;
      Context : in Natools.Meaningless_Type;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class)
   is
      pragma Unreferenced (Context);
      use type S_Expressions.Events.Event;
   begin
      if Arguments.Current_Event /= S_Expressions.Events.Add_Atom then
         return;
      end if;

      declare
         Value : constant String
           := S_Expressions.To_String (Arguments.Current_Atom);
      begin
         case Commands.To_File_Command (S_Expressions.To_String (Name)) is
            when Commands.File_Error =>
               null;

            when Commands.Set_Name =>
               State.Name := Hold (Value);

            when Commands.Set_Comment =>
               State.Comment := Hold (Value);

            when Commands.Set_Download =>
               if State.Download'Length = Value'Length then
                  State.Download := Value;
               end if;

            when Commands.Set_Expiration =>
               State.Expiration := Ada.Calendar.Formatting.Value (Value);

            when Commands.Set_MIME_Type =>
               State.MIME_Type := Hold (Value);

            when Commands.Set_Upload =>
               State.Upload := Ada.Calendar.Formatting.Value (Value);
         end case;
      end;
   end Execute;


   procedure Interpreter is new S_Expressions.Interpreter_Loop
     (File_Data, Natools.Meaningless_Type, Execute);


   procedure Write
     (Self : in File_Data;
      Output : in out S_Expressions.Printers.Printer'Class) is
   begin
      Output.Open_List;
      Output.Append_Atom (S_Expressions.To_Atom (Self.Report));
      Output.Open_List;
      Output.Append_Atom (S_Expressions.To_Atom ("name"));
      Output.Append_Atom (S_Expressions.To_Atom (To_String (Self.Name)));
      Output.Close_List;
      Output.Open_List;
      Output.Append_Atom (S_Expressions.To_Atom ("comment"));
      Output.Append_Atom (S_Expressions.To_Atom (To_String (Self.Comment)));
      Output.Close_List;
      Output.Open_List;
      Output.Append_Atom (S_Expressions.To_Atom ("download-key"));
      Output.Append_Atom (S_Expressions.To_Atom (Self.Download));
      Output.Close_List;
      Output.Open_List;
      Output.Append_Atom (S_Expressions.To_Atom ("expire"));
      Output.Append_Atom (S_Expressions.To_Atom
        (Ada.Calendar.Formatting.Image (Self.Expiration)));
      Output.Close_List;
      Output.Open_List;
      Output.Append_Atom (S_Expressions.To_Atom ("mime-type"));
      Output.Append_Atom (S_Expressions.To_Atom (To_String (Self.MIME_Type)));
      Output.Close_List;
      Output.Open_List;
      Output.Append_Atom (S_Expressions.To_Atom ("upload"));
      Output.Append_Atom (S_Expressions.To_Atom
        (Ada.Calendar.Formatting.Image (Self.Upload)));
      Output.Close_List;
      Output.Close_List;
   end Write;



   -------------------------
   -- Database Config I/O --
   -------------------------

   procedure Set_Max_Expiration
     (Max_Expiration : in out Size_Time;
      Expression : in out S_Expressions.Lockable.Descriptor'Class)
   is
      use type S_Expressions.Events.Event;
      Event : S_Expressions.Events.Event;
      New_Value : Size_Time := 0;
   begin
      Parse_Number :
      declare
         use type S_Expressions.Atom;
         use type S_Expressions.Count;
         use type S_Expressions.Octet;

         Number : constant S_Expressions.Atom := Expression.Current_Atom;
         I : S_Expressions.Offset := Number'First;
      begin
         while I in Number'Range and then Number (I) = 32 loop
            I := I + 1;
         end loop;

         while I in Number'Range and then Number (I) in 48 .. 57 loop
            New_Value := New_Value * 10 + Size_Time (Number (I) - 48);
            I := I + 1;
         end loop;

         if I in Number'Range or New_Value = 0 then
            return;
         end if;
      end Parse_Number;

      Max_Expiration := New_Value;

      Expression.Next (Event);
      if Event /= S_Expressions.Events.Add_Atom then
         return;
      end if;

      Parse_Unit :
      declare
         Unit_Str : constant String
           := S_Expressions.To_String (Expression.Current_Atom);
         I : Positive := Unit_Str'First;
      begin
         if Unit_Str (I) = 'k' then
            New_Value := New_Value * 1024;
            I := I + 1;
         elsif Unit_Str (I) = 'M' then
            New_Value := New_Value * 1024 ** 2;
            I := I + 1;
         elsif Unit_Str (I) = 'G' then
            New_Value := New_Value * 1024 ** 3;
            I := I + 1;
         elsif Unit_Str (I) = 'T' then
            New_Value := New_Value * 1024 ** 4;
            I := I + 1;
         end if;

         if Unit_Str (I) = 'b' then
            New_Value := New_Value / 8;
            I := I + 1;
         elsif Unit_Str (I) = 'B' then
            I := I + 1;
         else
            return;
         end if;

         if Unit_Str (I) = '.' then
            I := I + 1;
         else
            return;
         end if;

         if Unit_Str (I) = 's' then
            I := I + 1;
         elsif Unit_Str (I) = 'm' then
            New_Value := New_Value * 60;
            I := I + 1;
         elsif Unit_Str (I) = 'h' then
            New_Value := New_Value * 3600;
            I := I + 1;
         elsif Unit_Str (I) = 'd' then
            New_Value := New_Value * 86_400;
            I := I + 1;
         elsif Unit_Str (I) = 'w' then
            New_Value := New_Value * 604_800;
            I := I + 1;
         else
            return;
         end if;
      end Parse_Unit;

      Max_Expiration := New_Value;
   end Set_Max_Expiration;


   procedure Execute
     (State : in out Config_Data;
      Context : in Natools.Meaningless_Type;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class)
   is
      pragma Unreferenced (Context);
      use type S_Expressions.Events.Event;

      function Current_Atom return S_Expressions.Atom;

      function Current_Atom return S_Expressions.Atom is
      begin
         return Arguments.Current_Atom;
      end Current_Atom;
   begin
      if Arguments.Current_Event /= S_Expressions.Events.Add_Atom then
         return;
      end if;

      declare
         Value : constant String
           := S_Expressions.To_String (Arguments.Current_Atom);
      begin
         case Commands.To_Config_Command (S_Expressions.To_String (Name)) is
            when Commands.Config_Error =>
               null;

            when Commands.Set_Storage_File =>
               State.Storage_File := Hold (Value);

               declare
                  Event : S_Expressions.Events.Event;
               begin
                  Arguments.Next (Event);
                  case Event is
                     when S_Expressions.Events.Add_Atom
                       | S_Expressions.Events.Open_List
                     =>
                        S_Expressions.Printers.Pretty.Config.Update
                          (State.Printer_Param,
                           Arguments);

                     when S_Expressions.Events.Close_List
                       | S_Expressions.Events.End_Of_Input
                       | S_Expressions.Events.Error
                     =>
                        null;
                  end case;
               end;

            when Commands.Set_Directory =>
               State.Directory := Atom_Refs.Create (Current_Atom'Access);

            when Commands.Set_HMAC_Key =>
               State.HMAC_Key := Hold (Value);

            when Commands.Set_Index_Template =>
               State.Index_Template := Hold (Value);

            when Commands.Set_Input_Dir =>
               State.Input_Dir := Hold (Value);

            when Commands.Set_Max_Expiration =>
               Set_Max_Expiration (State.Max_Expiration, Arguments);

            when Commands.Set_Static_Dir =>
               State.Static_Dir := Hold (Value);

            when Commands.Set_Report_Template =>
               State.Report_Template := Hold (Value);
         end case;
      end;
   end Execute;


   procedure Interpreter is new S_Expressions.Interpreter_Loop
      (Config_Data, Natools.Meaningless_Type, Execute);



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


   function Comment (Self : File) return String is
   begin
      return To_String (Self.Ref.Query.Data.Comment);
   end Comment;


   function MIME_Type (Self : File) return String is
   begin
      return To_String (Self.Ref.Query.Data.MIME_Type);
   end MIME_Type;


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


   function Upload (Self : File) return Ada.Calendar.Time is
   begin
      return Self.Ref.Query.Data.Upload;
   end Upload;


   function Expiration (Self : File) return Ada.Calendar.Time is
   begin
      return Self.Ref.Query.Data.Expiration;
   end Expiration;


   function Expire_Before (Left, Right : File) return Boolean is
      use type Ada.Calendar.Time;
   begin
      return Left.Expiration < Right.Expiration
        or else (Left.Expiration = Right.Expiration
          and then Left.Report < Right.Report);
   end Expire_Before;



   ----------------------
   -- S-Expression I/O --
   ----------------------

   procedure Read
     (Self : out File_Set;
      Input : in out S_Expressions.Lockable.Descriptor'Class;
      Directory : in Atom_Refs.Immutable_Reference)
   is
      use type S_Expressions.Events.Event;
      use type Ada.Calendar.Time;

      Event : S_Expressions.Events.Event := Input.Current_Event;
      Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;

      Empty_Data : constant File_Data
        := (Name | Comment | MIME_Type => Hold (""),
            Report | Download => (others => ' '),
            Upload | Expiration => Now,
            Directory => Directory);
      Data : File_Data;
      F : File;

      function Create return File_Data;

      function Create return File_Data is
      begin
         return Data;
      end Create;

      Lock : S_Expressions.Lockable.Lock_State;
   begin
      Self.Reports.Clear;
      Self.Downloads.Clear;
      Self.Expires.Clear;

      loop
         case Event is
            when S_Expressions.Events.Open_List =>
               Input.Next (Event);
               if Event = S_Expressions.Events.Add_Atom
                 and then Input.Current_Atom'Length = Data.Report'Length
               then
                  Data := Empty_Data;
                  Data.Report := S_Expressions.To_String (Input.Current_Atom);
                  Input.Lock (Lock);
                  Input.Next;
                  Interpreter (Input, Data, Natools.Meaningless_Value);
                  Input.Unlock (Lock);

                  if Data.Download /= Empty_Data.Download
                    and then not Self.Reports.Contains (Data.Report)
                    and then not Self.Downloads.Contains (Data.Download)
                  then
                     if Data.Expiration > Now then
                        F := (Ref => File_Refs.Create (Create'Access));
                        Self.Reports.Insert (Data.Report, F);
                        Self.Downloads.Insert (Data.Download, F);
                        Self.Expires.Insert (F);
                     elsif Ada.Directories.Exists
                       (Path (Directory, Data.Report))
                     then
                        Ada.Directories.Delete_File
                          (Path (Directory, Data.Report));
                        Log ("Purged stale file " & Data.Report);
                     end if;
                  end if;
               end if;

            when S_Expressions.Events.Close_List
              | S_Expressions.Events.Add_Atom =>
               null;

            when S_Expressions.Events.End_Of_Input
              | S_Expressions.Events.Error =>
               exit;
         end case;
         Input.Next (Event);
      end loop;
   end Read;



   -----------------------------
   -- Database Implementation --
   -----------------------------

   protected body Database is

      function Debug_Activated return Boolean is
      begin
         return Debug;
      end Debug_Activated;

      function Report (Key : URI_Key) return File is
         use type Ada.Calendar.Time;

         Cursor : constant File_Maps.Cursor := Files.Reports.Find (Key);
         Result : File := (Ref => File_Refs.Null_Immutable_Reference);
      begin
         if File_Maps.Has_Element (Cursor) then
            Result := File_Maps.Element (Cursor);

            if Result.Expiration < Ada.Calendar.Clock then
               Result := (Ref => File_Refs.Null_Immutable_Reference);
            end if;
         end if;

         return Result;
      end Report;


      function Download (Key : URI_Key) return File is
         use type Ada.Calendar.Time;

         Cursor : constant File_Maps.Cursor := Files.Downloads.Find (Key);
         Result : File := (Ref => File_Refs.Null_Immutable_Reference);
      begin
         if File_Maps.Has_Element (Cursor) then
            Result := File_Maps.Element (Cursor);

            if Result.Expiration < Ada.Calendar.Clock then
               Result := (Ref => File_Refs.Null_Immutable_Reference);
            end if;
         end if;

         return Result;
      end Download;


      function Index_Template return String is
      begin
         return To_String (Config.Index_Template);
      end Index_Template;


      function Max_Expiration return Size_Time is
      begin
         return Config.Max_Expiration;
      end Max_Expiration;


      function Static_Resource_Dir return String is
      begin
         return To_String (Config.Static_Dir);
      end Static_Resource_Dir;


      function Report_Template return String is
      begin
         return To_String (Config.Report_Template);
      end Report_Template;


      function Iterate
        (Process : not null access procedure (F : in File))
         return Boolean
      is
         procedure Local_Process (Position : in Time_Sets.Cursor);

         procedure Local_Process (Position : in Time_Sets.Cursor) is
         begin
            Process.all (Time_Sets.Element (Position));
         end Local_Process;
      begin
         Files.Expires.Iterate (Local_Process'Access);
         return True;
      end Iterate;


      procedure Add_File
        (Local_Path : in String;
         Name : in String;
         Comment : in String;
         MIME_Type : in String;
         Expiration : in Ada.Calendar.Time;
         Report : out URI_Key)
      is
         Download : URI_Key;
         Input_Dir : constant String := To_String (Config.Input_Dir);

         function Create return File_Data;

         function Create return File_Data is
         begin
            return
              (Name => Hold (Name),
               Comment => Hold (Comment),
               MIME_Type => Hold (MIME_Type),
               Report => Report,
               Download => Download,
               Expiration => Expiration,
               Upload => Ada.Calendar.Clock,
               Directory => Config.Directory);
         end Create;
      begin
         Purge_Expired;

         if Input_Dir /= ""
           and then (Input_Dir'Length > Local_Path'Length
              or else Local_Path (Local_Path'First
                        .. Local_Path'First + Input_Dir'Length - 1)
                 /= Input_Dir)
         then
            --  Local_Path outside of Input_Dir, something nasty might be
            --  going on, so drop the request.

            Report := (others => ' ');
            return;
         end if;

         Compute_Hash :
         declare
            procedure Process (Block : in S_Expressions.Atom);

            Context : Hash.Context := Hash.Initial_Context;

            procedure Process (Block : in S_Expressions.Atom) is
            begin
               Hash.Update (Context, Block);
            end Process;
         begin
            S_Expressions.File_Readers.Block_Query
              (S_Expressions.File_Readers.Reader (Local_Path),
               4096,
               Process'Access);

            Report := S_Expressions.To_String
              (S_Expressions.Encodings.Encode_Base64
                 (Natools.GNAT_HMAC.Digest (Context), Digit_62, Digit_63));
         end Compute_Hash;

         Save_File :
         declare
            Target_Path : constant String := Path (Config.Directory, Report);
         begin
            Ada.Directories.Copy_File (Local_Path, Target_Path);
         end Save_File;

         Remove_Source :
         begin
            Ada.Directories.Delete_File (Local_Path);
         end Remove_Source;

         Download := S_Expressions.To_String
           (S_Expressions.Encodings.Encode_Base64
              (HMAC.Digest
                 (To_String (Config.HMAC_Key),
                  S_Expressions.To_Atom (Report)),
               Digit_62,
               Digit_63));

         Write_DB :
         declare
            Printer : S_Expressions.File_Writers.Writer;
         begin
            Printer.Open (To_String (Config.Storage_File));
            Printer.Set_Parameters (Config.Printer_Param);
            Write (Create, Printer);

            if Config.Printer_Param.Newline_At
              (S_Expressions.Printers.Pretty.Closing,
               S_Expressions.Printers.Pretty.Opening)
            then
               Printer.Newline;
            end if;
         end Write_DB;


         Insert_Ref :
         declare
            F : constant File := (Ref => File_Refs.Create (Create'Access));
         begin
            Files.Reports.Insert (Report, F);
            Files.Downloads.Insert (Download, F);
            Files.Expires.Insert (F);
         end Insert_Ref;

         Log ("File " & Report & " received, expires on "
           & Ada.Calendar.Formatting.Image (Expiration));
      end Add_File;


      procedure Purge_Expired is
         use type Ada.Calendar.Time;
         Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
         First : File;
      begin
         loop
            exit when Files.Expires.Is_Empty;
            First := Files.Expires.First_Element;
            exit when First.Expiration > Now;
            Files.Reports.Delete (First.Report);
            Files.Downloads.Delete (First.Download);
            Files.Expires.Delete (First);
            Ada.Directories.Delete_File (First.Path);
            Log ("File " & First.Report & " purged");
         end loop;
      end Purge_Expired;


      procedure Reset
        (New_Config : in out S_Expressions.Lockable.Descriptor'Class;
         New_Debug : in Boolean := False)
      is
         use type Ada.Directories.File_Kind;

         New_Data : Config_Data
           := (Storage_File => Hold ("/"),
               Directory => Atom_Refs.Null_Immutable_Reference,
               HMAC_Key | Input_Dir | Index_Template | Report_Template
                 | Static_Dir
                 => Hold (""),
               Max_Expiration => <>,
               Printer_Param => S_Expressions.Printers.Pretty.Canonical);
      begin
         Interpreter (New_Config, New_Data, Natools.Meaningless_Value);

         if New_Data.Directory.Is_Empty
           or else To_String (New_Data.HMAC_Key) = ""
           or else not Ada.Directories.Exists
              (To_String (New_Data.Storage_File))
           or else Ada.Directories.Kind (To_String (New_Data.Storage_File))
              /= Ada.Directories.Ordinary_File
         then
            Log ("Failed reset");
            return;
         end if;

         declare
            Storage : S_Expressions.File_Readers.S_Reader
              := S_Expressions.File_Readers.Reader
                 (To_String (New_Data.Storage_File));
         begin
            Read (Files, Storage, New_Data.Directory);
         end;

         Config := New_Data;
         Debug := New_Debug;

         Log ("Database successfully reset with"
           & Ada.Containers.Count_Type'Image (Files.Expires.Length)
           & " active files");
      end Reset;

   end Database;

end Backend;
