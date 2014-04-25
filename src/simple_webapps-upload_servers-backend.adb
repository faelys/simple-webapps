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
with Natools.S_Expressions.File_Readers;
with Natools.S_Expressions.Interpreters;
with Natools.S_Expressions.Printers;

separate (Simple_Webapps.Upload_Servers)
package body Backend is

   package Hash renames GNAT.SHA1;
   package HMAC renames Natools.GNAT_HMAC.SHA1;
   Hash_Name : constant String := "SHA-1";

   Digit_62 : constant Ada.Streams.Stream_Element := Character'Pos ('-');
   Digit_63 : constant Ada.Streams.Stream_Element := Character'Pos ('_');
      --  Special digits for base-64 URI (RFC 4648)

   procedure Write
     (Self : in File_Data;
      Output : in out S_Expressions.Printers.Printer'Class);
      --  Serialize the given file data into Output


   --------------------
   -- File Entry I/O --
   --------------------

   package File_Interpreters is new Natools.S_Expressions.Interpreters
     (Shared_State => File_Data,
      Shared_Context => Boolean);

   function File_Interpreter return File_Interpreters.Interpreter;


   package File_Commands is
      type Single_Value is abstract new File_Interpreters.Command
        with null record;

      not overriding procedure Simple_Execute
        (Self : in out Single_Value;
         State : in out File_Data;
         Context : in Boolean;
         Value : in String)
        is abstract;

      overriding procedure Execute
        (Self : in out Single_Value;
         State : in out File_Data;
         Context : in Boolean;
         Cmd : in out S_Expressions.Lockable.Descriptor'Class);

      type Set_Name is new Single_Value with null record;

      overriding procedure Simple_Execute
        (Self : in out Set_Name;
         State : in out File_Data;
         Context : in Boolean;
         Value : in String);

      type Set_Comment is new Single_Value with null record;

      overriding procedure Simple_Execute
        (Self : in out Set_Comment;
         State : in out File_Data;
         Context : in Boolean;
         Value : in String);

      type Set_Download is new Single_Value with null record;

      overriding procedure Simple_Execute
        (Self : in out Set_Download;
         State : in out File_Data;
         Context : in Boolean;
         Value : in String);
   end File_Commands;


   package body File_Commands is

      overriding procedure Execute
        (Self : in out Single_Value;
         State : in out File_Data;
         Context : in Boolean;
         Cmd : in out S_Expressions.Lockable.Descriptor'Class)
      is
         use type S_Expressions.Events.Event;

         Event : S_Expressions.Events.Event;
      begin
         Cmd.Next (Event);

         if Event /= S_Expressions.Events.Add_Atom then
            return;
         end if;

         Simple_Execute
           (Single_Value'Class (Self), State, Context,
            S_Expressions.To_String (Cmd.Current_Atom));
      end Execute;


      overriding procedure Simple_Execute
        (Self : in out Set_Name;
         State : in out File_Data;
         Context : in Boolean;
         Value : in String)
      is
         pragma Unreferenced (Self, Context);
      begin
         State.Name := Hold (Value);
      end Simple_Execute;


      overriding procedure Simple_Execute
        (Self : in out Set_Comment;
         State : in out File_Data;
         Context : in Boolean;
         Value : in String)
      is
         pragma Unreferenced (Self, Context);
      begin
         State.Comment := Hold (Value);
      end Simple_Execute;


      overriding procedure Simple_Execute
        (Self : in out Set_Download;
         State : in out File_Data;
         Context : in Boolean;
         Value : in String)
      is
         pragma Unreferenced (Self, Context);
      begin
         if State.Download'Length = Value'Length then
            State.Download := Value;
         end if;
      end Simple_Execute;
   end File_Commands;


   function File_Interpreter return File_Interpreters.Interpreter is
      Result : File_Interpreters.Interpreter;
   begin
      Result.Add_Command
        (S_Expressions.To_Atom ("name"),
         File_Commands.Set_Name'(null record));
      Result.Add_Command
        (S_Expressions.To_Atom ("comment"),
         File_Commands.Set_Comment'(null record));
      Result.Add_Command
        (S_Expressions.To_Atom ("download-key"),
         File_Commands.Set_Download'(null record));
      Result.Add_Command
        (S_Expressions.To_Atom ("error"),
         File_Interpreters.Do_Nothing);

      Result.Set_Fallback (S_Expressions.To_Atom ("error"));
      return Result;
   end File_Interpreter;


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
      Output.Close_List;
   end Write;



   -------------------------
   -- Database Config I/O --
   -------------------------

   package Config_Interpreters is new Natools.S_Expressions.Interpreters
     (Shared_State => Config_Data,
      Shared_Context => Boolean);

   function Config_Interpreter return Config_Interpreters.Interpreter;


   package Config_Commands is
      type Single_Value is abstract new Config_Interpreters.Command
        with null record;

      not overriding procedure Simple_Execute
        (Self : in out Single_Value;
         State : in out Config_Data;
         Context : in Boolean;
         Value : in String)
        is abstract;

      overriding procedure Execute
        (Self : in out Single_Value;
         State : in out Config_Data;
         Context : in Boolean;
         Cmd : in out S_Expressions.Lockable.Descriptor'Class);

      type Set_Storage_File is new Single_Value with null record;

      overriding procedure Simple_Execute
        (Self : in out Set_Storage_File;
         State : in out Config_Data;
         Context : in Boolean;
         Value : in String);

      type Set_Directory is new Single_Value with null record;

      overriding procedure Simple_Execute
        (Self : in out Set_Directory;
         State : in out Config_Data;
         Context : in Boolean;
         Value : in String);

      type Set_HMAC_Key is new Single_Value with null record;

      overriding procedure Simple_Execute
        (Self : in out Set_HMAC_Key;
         State : in out Config_Data;
         Context : in Boolean;
         Value : in String);
   end Config_Commands;


   package body Config_Commands is

      overriding procedure Execute
        (Self : in out Single_Value;
         State : in out Config_Data;
         Context : in Boolean;
         Cmd : in out S_Expressions.Lockable.Descriptor'Class)
      is
         use type S_Expressions.Events.Event;

         Event : S_Expressions.Events.Event;
      begin
         Cmd.Next (Event);

         if Event /= S_Expressions.Events.Add_Atom then
            return;
         end if;

         Simple_Execute
           (Single_Value'Class (Self), State, Context,
            S_Expressions.To_String (Cmd.Current_Atom));
      end Execute;


      overriding procedure Simple_Execute
        (Self : in out Set_Storage_File;
         State : in out Config_Data;
         Context : in Boolean;
         Value : in String)
      is
         pragma Unreferenced (Self, Context);
      begin
         State.Storage_File := Hold (Value);
      end Simple_Execute;


      overriding procedure Simple_Execute
        (Self : in out Set_Directory;
         State : in out Config_Data;
         Context : in Boolean;
         Value : in String)
      is
         pragma Unreferenced (Self, Context);

         function Create return S_Expressions.Atom;

         function Create return S_Expressions.Atom is
         begin
            return S_Expressions.To_Atom (Value);
         end Create;
      begin
         State.Directory := Atom_Refs.Create (Create'Access);
      end Simple_Execute;


      overriding procedure Simple_Execute
        (Self : in out Set_HMAC_Key;
         State : in out Config_Data;
         Context : in Boolean;
         Value : in String)
      is
         pragma Unreferenced (Self, Context);
      begin
         State.HMAC_Key := Hold (Value);
      end Simple_Execute;

   end Config_Commands;


   function Config_Interpreter return Config_Interpreters.Interpreter is
      Result : Config_Interpreters.Interpreter;
   begin
      Result.Add_Command
        (S_Expressions.To_Atom ("backend"),
         Config_Commands.Set_Storage_File'(null record));
      Result.Add_Command
        (S_Expressions.To_Atom ("directory"),
         Config_Commands.Set_Directory'(null record));
      Result.Add_Command
        (S_Expressions.To_Atom ("hmac-key"),
         Config_Commands.Set_HMAC_Key'(null record));
      Result.Add_Command
        (S_Expressions.To_Atom ("error"),
         Config_Interpreters.Do_Nothing);

      Result.Set_Fallback (S_Expressions.To_Atom ("error"));
      return Result;
   end Config_Interpreter;



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



   ----------------------
   -- S-Expression I/O --
   ----------------------

   procedure Read
     (Self : out File_Set;
      Input : in out S_Expressions.Lockable.Descriptor'Class;
      Directory : in Atom_Refs.Immutable_Reference)
   is
      use type S_Expressions.Events.Event;
      Event : S_Expressions.Events.Event := Input.Current_Event;

      Empty_Data : constant File_Data
        := (Name | Comment => Hold (""),
            Report | Download => (others => ' '),
            Directory => Directory);
      Data : File_Data;
      F : File;

      function Create return File_Data;

      function Create return File_Data is
      begin
         return Data;
      end Create;

      Interpreter : File_Interpreters.Interpreter := File_Interpreter;
      Lock : S_Expressions.Lockable.Lock_State;
   begin
      Self.Reports.Clear;
      Self.Downloads.Clear;

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
                  Interpreter.Execute (Input, Data, True);
                  Input.Unlock (Lock);

                  if Data.Download /= Empty_Data.Download
                    and then not Self.Reports.Contains (Data.Report)
                    and then not Self.Downloads.Contains (Data.Download)
                  then
                     F := (Ref => File_Refs.Create (Create'Access));
                     Self.Reports.Insert (Data.Report, F);
                     Self.Downloads.Insert (Data.Download, F);
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


      procedure Add_File
        (Local_Path : in String;
         Name : in String;
         Comment : in String;
         Report : out URI_Key)
      is
         Download : URI_Key;

         function Create return File_Data;

         function Create return File_Data is
         begin
            return
              (Name => Hold (Name),
               Comment => Hold (Comment),
               Report => Report,
               Download => Download,
               Directory => Config.Directory);
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
            Target_Path : constant String := Path (Config.Directory, Report);
         begin
            Ada.Directories.Copy_File (Local_Path, Target_Path);
         end Save_File;

         Download := S_Expressions.To_String
           (S_Expressions.Encodings.Encode_Base64
              (HMAC.Digest
                 (To_String (Config.HMAC_Key),
                  S_Expressions.To_Atom (Report)),
               Digit_62,
               Digit_63));

         Write_DB :
         declare
            package Stream_IO renames Ada.Streams.Stream_IO;
            Output : Stream_IO.File_Type;
         begin
            Stream_IO.Open
              (Output,
               Stream_IO.Append_File,
               To_String (Config.Storage_File));

            Printer_Block :
            declare
               Printer : S_Expressions.Printers.Canonical
                 (Stream_IO.Stream (Output));
            begin
               Write (Create, Printer);
            end Printer_Block;

            Stream_IO.Close (Output);
         end Write_DB;


         Insert_Ref :
         declare
            F : constant File := (Ref => File_Refs.Create (Create'Access));
         begin
            Files.Reports.Insert (Report, F);
            Files.Downloads.Insert (Download, F);
         end Insert_Ref;
      end Add_File;


      procedure Reset
        (New_Config : in out S_Expressions.Lockable.Descriptor'Class)
      is
         use type Ada.Directories.File_Kind;

         New_Data : Config_Data
           := (Storage_File => Hold ("/"),
               Directory => Atom_Refs.Null_Immutable_Reference,
               HMAC_Key => Hold (""));
         Interpreter : Config_Interpreters.Interpreter := Config_Interpreter;
      begin
         Interpreter.Execute (New_Config, New_Data, True);

         if New_Data.Directory.Is_Empty
           or else To_String (New_Data.HMAC_Key) = ""
           or else not Ada.Directories.Exists
              (To_String (New_Data.Storage_File))
           or else Ada.Directories.Kind (To_String (New_Data.Storage_File))
              /= Ada.Directories.Ordinary_File
         then
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
      end Reset;

   end Database;

end Backend;
