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

with Simple_Webapps.Upload_Servers;

procedure Upload_Server is
   WS : AWS.Server.HTTP;
   Handler : Simple_Webapps.Upload_Servers.Handler;
   Debug : constant Boolean := Ada.Command_Line.Argument_Count >= 2;
begin
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
