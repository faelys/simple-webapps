--  Generated at 2014-06-11 19:00:27 +0000 by Natools.Static_Hash_Maps
--  from src/simple_webapps-upload_servers-commands.sx

with Simple_Webapps.Commands.Upload_Servers.Config;
with Simple_Webapps.Commands.Upload_Servers.File;

package body Simple_Webapps.Commands.Upload_Servers is

   function To_Config_Command (Key : String) return Config_Command is
      N : constant Natural
        := Simple_Webapps.Commands.Upload_Servers.Config.Hash (Key);
   begin
      if Map_1_Keys (N).all = Key then
         return Map_1_Elements (N);
      else
         return Config_Error;
      end if;
   end To_Config_Command;


   function To_File_Command (Key : String) return File_Command is
      N : constant Natural
        := Simple_Webapps.Commands.Upload_Servers.File.Hash (Key);
   begin
      if Map_2_Keys (N).all = Key then
         return Map_2_Elements (N);
      else
         return File_Error;
      end if;
   end To_File_Command;

end Simple_Webapps.Commands.Upload_Servers;
