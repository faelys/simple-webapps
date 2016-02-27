------------------------------------------------------------------------------
-- Copyright (c) 2014-2016, Natacha Porté                                   --
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

with Ada.Command_Line;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Text_IO;

with AWS.Config;
with AWS.Dispatchers;
with AWS.Server;
with AWS.Services.Dispatchers.Virtual_Host;

with Natools.S_Expressions.File_Readers;
with Natools.S_Expressions.Interpreter_Loop;
with Natools.S_Expressions.Lockable;

with Syslog.Guess.App_Name;
with Syslog.Guess.Hostname;
with Syslog.Transport.Send_Task;
with Syslog.Transport.UDP;

with Simple_Webapps.Append_Servers;
with Simple_Webapps.Upload_Servers;

procedure All_Servers is
   package String_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists
     (String);

   procedure Register
     (Dispatcher : in out AWS.Services.Dispatchers.Virtual_Host.Handler;
      Host_List : in String_Lists.List;
      Action : in AWS.Dispatchers.Handler'Class);

   procedure Execute
     (State : in out AWS.Services.Dispatchers.Virtual_Host.Handler;
      Context : in Boolean;
      Name : in Natools.S_Expressions.Atom;
      Arguments : in out Natools.S_Expressions.Lockable.Descriptor'Class);

   procedure Interpreter is new Natools.S_Expressions.Interpreter_Loop
     (AWS.Services.Dispatchers.Virtual_Host.Handler, Boolean, Execute);


   procedure Execute
     (State : in out AWS.Services.Dispatchers.Virtual_Host.Handler;
      Context : in Boolean;
      Name : in Natools.S_Expressions.Atom;
      Arguments : in out Natools.S_Expressions.Lockable.Descriptor'Class)
   is
      package Sx renames Natools.S_Expressions;
      use type Natools.S_Expressions.Events.Event;
      S_Name : constant String := Sx.To_String (Name);
      Hosts : String_Lists.List;
      Event : Sx.Events.Event := Arguments.Current_Event;
   begin
      while Event = Sx.Events.Add_Atom loop
         String_Lists.Append (Hosts, Sx.To_String (Arguments.Current_Atom));
         Arguments.Next (Event);
      end loop;

      if String_Lists.Is_Empty (Hosts) then
         return;
      end if;

      if S_Name = "upload" or S_Name = "upload-server" then
         declare
            Handler : Simple_Webapps.Upload_Servers.Handler;
         begin
            Handler.Reset (Arguments, Context);
            Register (State, Hosts, Handler);
         end;
      elsif S_Name = "append" or S_Name = "append-server" then
         declare
            Handler : Simple_Webapps.Append_Servers.Handler;
         begin
            Handler.Reset (Arguments, Ada.Command_Line.Argument (1));
            Register (State, Hosts, Handler);
         end;
      end if;
   end Execute;

   procedure Register
     (Dispatcher : in out AWS.Services.Dispatchers.Virtual_Host.Handler;
      Host_List : in String_Lists.List;
      Action : in AWS.Dispatchers.Handler'Class)
   is
      Cursor : String_Lists.Cursor := Host_List.First;
   begin
      while String_Lists.Has_Element (Cursor) loop
         if String_Lists.Element (Cursor) = "" then
            Dispatcher.Register_Default_Callback (Action);
         else
            Dispatcher.Register (String_Lists.Element (Cursor), Action);
         end if;

         String_Lists.Next (Cursor);
      end loop;
   end Register;


   WS : AWS.Server.HTTP;
   Handler : AWS.Services.Dispatchers.Virtual_Host.Handler;
   Debug : constant Boolean := Ada.Command_Line.Argument_Count >= 2;
begin
   if Ada.Command_Line.Argument_Count < 1 then
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Current_Error,
         "Usage: " & Ada.Command_Line.Command_Name & " servers.sx");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

   if Debug then
      Simple_Webapps.Upload_Servers.Log := Ada.Text_IO.Put_Line'Access;
   else
      Syslog.Guess.App_Name;
      Syslog.Guess.Hostname;
      Syslog.Transport.UDP.Connect ("127.0.0.1");
      Syslog.Transport.Send_Task.Set_Backend (Syslog.Transport.UDP.Transport);
      Syslog.Set_Transport (Syslog.Transport.Send_Task.Transport);
      Syslog.Set_Default_Facility (Syslog.Facilities.Daemon);
      Syslog.Set_Default_Severity (Syslog.Severities.Notice);
      Simple_Webapps.Upload_Servers.Log := Syslog.Log'Access;
   end if;

   Read_Config :
   declare
      Reader : Natools.S_Expressions.File_Readers.S_Reader;
   begin
      Reader.Set_Filename (Ada.Command_Line.Argument (1));
      Interpreter (Reader, Handler, Debug);
   end Read_Config;

   AWS.Server.Start (WS, Handler, AWS.Config.Get_Current);

   if Debug then
      Ada.Text_IO.Put_Line ("Websever started");
      AWS.Server.Wait (AWS.Server.Q_Key_Pressed);
   else
      AWS.Server.Wait;
   end if;

   AWS.Server.Shutdown (WS);
end All_Servers;
