------------------------------------------------------------------------------
-- Copyright (c) 2016, Natacha Porté                                        --
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

with Ada.Calendar;
with Ada.Streams.Stream_IO;
with Natools.File_Streams;
with Natools.GNAT_HMAC.SHA256;
with Natools.S_Expressions.Atom_Ref_Constructors;
with Natools.S_Expressions.File_Writers;
with Natools.S_Expressions.Interpreter_Loop;
with Natools.S_Expressions.Lockable;
with Natools.Time_IO.RFC_3339;
with Simple_Webapps.Commands.Append_Servers;

package body Simple_Webapps.Append_Servers is

   package Commands renames Simple_Webapps.Commands.Append_Servers;
   package Constructors renames Natools.S_Expressions.Atom_Ref_Constructors;
   package HMAC renames Natools.GNAT_HMAC.SHA256;


   procedure Set
     (State : in out Endpoint;
      Context : in String;
      Name : in Sx.Atom;
      Arguments : in out Sx.Lockable.Descriptor'Class);


   procedure Interpreter is new Sx.Interpreter_Loop
     (Endpoint, String, Set) with Unreferenced;



   ------------------------
   -- Execution Endpoint --
   ------------------------

   procedure Append_Data
     (Self : in Endpoint;
      Data : in Sx.Atom)
   is
      File : Natools.File_Streams.File_Stream
        := Natools.File_Streams.Open
           (Ada.Streams.Stream_IO.Append_File,
            To_String (Self.Data_Path));
   begin
      File.Write (Data);
   end Append_Data;


   procedure Log_Invalid
     (Self : in Endpoint;
      Data : in Sx.Atom;
      Given_Signature : in Sx.Atom;
      Expected_Signature : in Sx.Atom) is
   begin
      if To_String (Self.Invalid_Log) = "" then
         Log ("Invalid signature for data posted to "
           & To_String (Self.Data_Path));
         return;
      end if;

      declare
         Log_File : Sx.File_Writers.Writer
           := Sx.File_Writers.Open (To_String (Self.Invalid_Log));
      begin
         Log_File.Open_List;
         Log_File.Append_String (Natools.Time_IO.RFC_3339.Image
           (Ada.Calendar.Clock, Subsecond_Digits => 3));

         Log_File.Open_List;
         Log_File.Append_String ("endpoint");
         Log_File.Append_String (To_String (Self.Data_Path));
         Log_File.Close_List;

         Log_File.Open_List;
         Log_File.Append_String ("data");
         Log_File.Append_Atom (Data);
         Log_File.Close_List;

         Log_File.Open_List;
         Log_File.Append_String ("expected-signature");
         Log_File.Append_Atom (Expected_Signature);
         Log_File.Close_List;

         Log_File.Open_List;
         Log_File.Append_String ("given-signature");
         Log_File.Append_Atom (Given_Signature);
         Log_File.Close_List;

         Log_File.Close_List;
      end;
   end Log_Invalid;


   function Execute
     (Self : in Endpoint;
      Data : in Sx.Atom;
      Signature : in Sx.Atom)
     return Execution_Results.Data
   is
      use type Sx.Atom;
   begin
      Signature_Check :
      declare
         Expected_Signature : constant Sx.Atom
           := HMAC.Digest (Self.Key.Query, Data);
      begin
         if Expected_Signature /= Signature then
            Log_Invalid (Self, Data, Signature, Expected_Signature);
            return (State => Execution_Results.Invalid_Signature);
         end if;
      end Signature_Check;

      case Self.Separator.Action is
         when No_Separator =>
            Append_Data (Self, Data);
         when Force_Separator =>
            Append_Data (Self, Data & Self.Separator.Data.Query);
         when Separator_If_Needed =>
            declare
               use type Sx.Offset;
               Sep : constant Sx.Atom := Self.Separator.Data.Query;
            begin
               if Data'Length >= Sep'Length
                 and then Data (Data'Last - Sep'Length + 1 .. Data'Last) = Sep
               then
                  Append_Data (Self, Data);
               else
                  Append_Data (Self, Data & Sep);
               end if;
            end;
      end case;

      return (State => Execution_Results.OK, Redirect => Self.Redirect);
   end Execute;


   procedure Set
     (State : in out Endpoint;
      Context : in String;
      Name : in Sx.Atom;
      Arguments : in out Sx.Lockable.Descriptor'Class)
   is
      use type Sx.Events.Event;
   begin
      case Commands.To_Endpoint_Command (Sx.To_String (Name)) is
         when Commands.Endpoint_Error =>
            Log ("Unknown command """ & Sx.To_String (Name)
              & """ for endpoint """ & Context & '"');

         when Commands.Data_Path =>
            if Arguments.Current_Event = Sx.Events.Add_Atom then
               State.Data_Path := Hold (Sx.To_String (Arguments.Current_Atom));
            end if;

         when Commands.Force_Separator =>
            if Arguments.Current_Event = Sx.Events.Add_Atom then
               State.Separator
                 := (Action => Force_Separator,
                     Data => Constructors.Create (Arguments.Current_Atom));
            end if;

         when Commands.Invalid_Log =>
            if Arguments.Current_Event = Sx.Events.Add_Atom then
               State.Invalid_Log
                 := Hold (Sx.To_String (Arguments.Current_Atom));
            end if;

         when Commands.Key =>
            if Arguments.Current_Event = Sx.Events.Add_Atom then
               State.Key := Constructors.Create (Arguments.Current_Atom);
            end if;

         when Commands.No_Separator =>
            State.Separator := (Action => No_Separator);

         when Commands.Redirect =>
            if Arguments.Current_Event = Sx.Events.Add_Atom then
               State.Redirect := Hold (Sx.To_String (Arguments.Current_Atom));
            end if;

         when Commands.Separator_If_Needed =>
            if Arguments.Current_Event = Sx.Events.Add_Atom then
               State.Separator
                 := (Action => Separator_If_Needed,
                     Data => Constructors.Create (Arguments.Current_Atom));
            end if;
      end case;
   end Set;

end Simple_Webapps.Append_Servers;
