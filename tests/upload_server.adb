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

with Ada.Command_Line;
with Ada.Text_IO;

with AWS.Config;
with AWS.Server;

with Syslog.Guess.App_Name;
with Syslog.Guess.Hostname;
with Syslog.Transport.Send_Task;
with Syslog.Transport.UDP;

with Simple_Webapps.Upload_Servers;

procedure Upload_Server is
   WS : AWS.Server.HTTP;
   Handler : Simple_Webapps.Upload_Servers.Handler;
   Debug : constant Boolean := Ada.Command_Line.Argument_Count >= 2;
begin
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

   if Ada.Command_Line.Argument_Count >= 1 then
      Handler.Reset (Ada.Command_Line.Argument (1), Debug);
   else
      Handler.Reset ("upload.sx", Debug);
   end if;

   AWS.Server.Start (WS, Handler, AWS.Config.Get_Current);

   if Debug then
      Ada.Text_IO.Put_Line ("Websever started");
      AWS.Server.Wait (AWS.Server.Q_Key_Pressed);
   else
      AWS.Server.Wait;
   end if;

   AWS.Server.Shutdown (WS);
end Upload_Server;
