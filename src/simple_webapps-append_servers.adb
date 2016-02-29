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
with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;
with Ada.IO_Exceptions;
with AWS.Messages;
with AWS.MIME;
with AWS.Parameters;
with AWS.Response.Set;
with Natools.File_Streams;
with Natools.GNAT_HMAC.SHA256;
with Natools.S_Expressions.Atom_Ref_Constructors;
with Natools.S_Expressions.Encodings;
with Natools.S_Expressions.File_Readers;
with Natools.S_Expressions.File_Writers;
with Natools.S_Expressions.Interpreter_Loop;
with Natools.Time_IO.RFC_3339;
with Simple_Webapps.Commands.Append_Servers;
with Templates_Parser;

package body Simple_Webapps.Append_Servers is

   package Commands renames Simple_Webapps.Commands.Append_Servers;
   package Constructors renames Natools.S_Expressions.Atom_Ref_Constructors;
   package HMAC renames Natools.GNAT_HMAC.SHA256;


   function Simple_Response
     (Code : AWS.Messages.Status_Code;
      Template : String := "";
      Location : String := "";
      Allow : String := "")
     return AWS.Response.Data;

   procedure Set
     (State : in out Endpoint;
      Context : in String;
      Name : in Sx.Atom;
      Arguments : in out Sx.Lockable.Descriptor'Class);

   procedure Set
     (State : in out Endpoint_Maps.Unsafe_Maps.Map;
      Context : in Natools.Meaningless_Type;
      Name : in Sx.Atom;
      Arguments : in out Sx.Lockable.Descriptor'Class);

   procedure Set
     (State : in out Server_Data;
      Context : in String;
      Name : in Sx.Atom;
      Arguments : in out Sx.Lockable.Descriptor'Class);


   procedure Interpreter is new Sx.Interpreter_Loop
     (Endpoint, String, Set);

   procedure Interpreter is new Sx.Interpreter_Loop
     (Endpoint_Maps.Unsafe_Maps.Map, Natools.Meaningless_Type, Set);

   procedure Interpreter is new Sx.Interpreter_Loop
     (Server_Data, String, Set);



   ------------------------
   -- Execution Endpoint --
   ------------------------

   procedure Append_Data
     (Self : in Endpoint;
      Data : in Sx.Atom)
   is
      function Open_File (Name : String)
        return Natools.File_Streams.File_Stream;


      function Open_File (Name : String)
        return Natools.File_Streams.File_Stream is
      begin
         return Natools.File_Streams.Open
           (Ada.Streams.Stream_IO.Append_File, Name);
      exception
         when Ada.IO_Exceptions.Name_Error =>
            return Natools.File_Streams.Create
              (Ada.Streams.Stream_IO.Append_File, Name);
      end Open_File;


      File : Natools.File_Streams.File_Stream
        := Open_File (To_String (Self.Data_Path));
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


   -----------------------------
   -- Server-Wide Subprograms --
   -----------------------------

   procedure Set
     (State : in out Endpoint_Maps.Unsafe_Maps.Map;
      Context : in Natools.Meaningless_Type;
      Name : in Sx.Atom;
      Arguments : in out Sx.Lockable.Descriptor'Class)
   is
      use type Sx.Events.Event;
      pragma Unreferenced (Context);
      S_Name : constant String := Sx.To_String (Name);
      Item : Endpoint;
   begin
      Interpreter (Arguments, Item, S_Name);

      if Item.Key.Is_Empty or else To_String (Item.Data_Path) = "" then
         return;
      end if;

      State.Include (S_Name, Item);
   end Set;


   procedure Set
     (State : in out Server_Data;
      Context : in String;
      Name : in Sx.Atom;
      Arguments : in out Sx.Lockable.Descriptor'Class)
   is
      use type Sx.Events.Event;
   begin
      case Commands.To_Server_Command (Sx.To_String (Name)) is
         when Commands.Server_Error =>
            Log ("Unknown command """ & Sx.To_String (Name)
              & """ for server data in """ & Context & '"');

         when Commands.Endpoints =>
            declare
               New_Map : Endpoint_Maps.Unsafe_Maps.Map;
            begin
               Interpreter (Arguments, New_Map, Natools.Meaningless_Value);
               State.Endpoints := Endpoint_Maps.Create (New_Map);
            end;

         when Commands.Static_Path =>
            if Arguments.Current_Event = Sx.Events.Add_Atom then
               State.Static_Path
                 := Hold (Sx.To_String (Arguments.Current_Atom));
            end if;

         when Commands.Template =>
            if Arguments.Current_Event = Sx.Events.Add_Atom then
               State.Template := Hold (Sx.To_String (Arguments.Current_Atom));
            end if;
      end case;
   end Set;


   -----------------
   -- AWS Handler --
   -----------------

   overriding function Dispatch
     (Dispatcher : in Handler;
      Request : in AWS.Status.Data)
     return AWS.Response.Data
   is
      Accessor : constant Server_Refs.Accessor := Dispatcher.Ref.Query;
      URI : constant String := AWS.Status.URI (Request);
      Cursor : constant Endpoint_Maps.Cursor := Accessor.Endpoints.Find (URI);

      use type AWS.Status.Request_Method;
   begin
      if not Endpoint_Maps.Has_Element (Cursor) then
         Dispatch_Static_File :
         declare
            Path : constant String := To_String (Accessor.Static_Path) & URI;
         begin
            if Ada.Strings.Fixed.Index (URI, "/.") /= 0
              or else not Ada.Directories.Exists (Path)
            then
               return Simple_Response
                 (AWS.Messages.S404, To_String (Accessor.Template));
            elsif AWS.Status.Method (Request) /= AWS.Status.GET then
               return Simple_Response
                 (AWS.Messages.S405,
                  To_String (Accessor.Template),
                  Allow => "GET");
            else
               return AWS.Response.File (AWS.MIME.Content_Type (Path), Path);
            end if;
         end Dispatch_Static_File;
      end if;

      if AWS.Status.Method (Request) /= AWS.Status.POST then
         return Simple_Response
           (AWS.Messages.S405, To_String (Accessor.Template), Allow => "POST");
      end if;

      declare
         Parameters : constant AWS.Parameters.List
           := AWS.Status.Parameters (Request);
         Data : constant Sx.Atom
           := Sx.To_Atom (AWS.Parameters.Get (Parameters, "data"));
         Signature : constant Sx.Atom
           := Sx.To_Atom (AWS.Parameters.Get (Parameters, "signature"));
         Result : constant Execution_Results.Data := Execute
           (Accessor.Endpoints.Constant_Reference (Cursor),
            Data,
            Sx.Encodings.Decode_Hex (Signature));
      begin
         case Result.State is
            when Execution_Results.OK =>
               if To_String (Result.Redirect) /= "" then
                  return Simple_Response
                    (AWS.Messages.S303, To_String (Accessor.Template),
                     Location => To_String (Result.Redirect));
               else
                  return Simple_Response
                    (AWS.Messages.S200, To_String (Accessor.Template));
               end if;
            when Execution_Results.Invalid_Signature =>
               return Simple_Response
                 (AWS.Messages.S403, To_String (Accessor.Template));
            when Execution_Results.File_Error =>
               return Simple_Response
                 (AWS.Messages.S500, To_String (Accessor.Template));
         end case;
      end;
   end Dispatch;


   overriding function Clone (Dispatcher : in Handler) return Handler is
   begin
      return Dispatcher;
   end Clone;


   not overriding procedure Reset
     (Dispatcher : in out Handler;
      Config_File : in String)
   is
      Reader : Sx.File_Readers.S_Reader
        := Sx.File_Readers.Reader (Config_File);
   begin
      Reset (Dispatcher, Reader, Config_File);
   end Reset;


   not overriding procedure Reset
     (Dispatcher : in out Handler;
      Config : in out Natools.S_Expressions.Lockable.Descriptor'Class;
      File_Name : in String)
   is
      New_Server : constant Server_Refs.Data_Access := new Server_Data;
      New_Ref : constant Server_Refs.Immutable_Reference
        := Server_Refs.Create (New_Server);
   begin
      Interpreter (Config, New_Server.all, File_Name);
      Dispatcher.Ref := New_Ref;
   end Reset;


   function Simple_Response
     (Code : AWS.Messages.Status_Code;
      Template : String := "";
      Location : String := "";
      Allow : String := "")
     return AWS.Response.Data
   is
      function Response_Body return String;

      Image : constant String := AWS.Messages.Image (Code);
      Message : constant String := AWS.Messages.Reason_Phrase (Code);

      function Response_Body return String is
      begin
         if Template = "" then
            return "<html><head><title>" & Image & ' ' & Message
              & "</title></head>"
              & "<body><h1>" & Image & ' ' & Message & "</h1></body></html>";
         else
            return Templates_Parser.Parse (Template,
              (Templates_Parser.Assoc ("CODE", Image),
               Templates_Parser.Assoc ("MESSAGE", Message)));
         end if;
      end Response_Body;

      Result : AWS.Response.Data := AWS.Response.Build
        ("text/html", Response_Body, Code);
   begin
      if Allow /= "" then
         AWS.Response.Set.Add_Header
           (Result,
            AWS.Messages.Allow_Token,
            Allow);
      end if;

      if Location /= "" then
         AWS.Response.Set.Add_Header
           (Result,
            AWS.Messages.Location_Token,
            Location);
      end if;

      return Result;
   end Simple_Response;

end Simple_Webapps.Append_Servers;
