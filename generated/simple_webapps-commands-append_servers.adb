--  Generated at 2016-02-17 19:37:34 +0000 by Natools.Static_Hash_Maps
--  from src/simple_webapps-append_servers-maps.sx

with Simple_Webapps.Commands.Append_Servers.Endpoint_Hash;

package body Simple_Webapps.Commands.Append_Servers is

   function To_Endpoint_Command (Key : String) return Endpoint_Command is
      N : constant Natural
        := Simple_Webapps.Commands.Append_Servers.Endpoint_Hash.Hash (Key);
   begin
      if Map_1_Keys (N).all = Key then
         return Map_1_Elements (N);
      else
         return Endpoint_Error;
      end if;
   end To_Endpoint_Command;

end Simple_Webapps.Commands.Append_Servers;
