--  Generated at 2016-02-17 19:37:34 +0000 by Natools.Static_Hash_Maps
--  from src/simple_webapps-append_servers-maps.sx

package Simple_Webapps.Commands.Append_Servers is
   pragma Pure;

   type Endpoint_Command is
     (Endpoint_Error,
      Data_Path,
      Force_Separator,
      Invalid_Log,
      Key,
      No_Separator,
      Redirect,
      Separator_If_Needed);

   function To_Endpoint_Command (Key : String) return Endpoint_Command;

private

   Map_1_Key_0 : aliased constant String := "data-path";
   Map_1_Key_1 : aliased constant String := "path";
   Map_1_Key_2 : aliased constant String := "file";
   Map_1_Key_3 : aliased constant String := "file-path";
   Map_1_Key_4 : aliased constant String := "force-separator";
   Map_1_Key_5 : aliased constant String := "invalid-log";
   Map_1_Key_6 : aliased constant String := "key";
   Map_1_Key_7 : aliased constant String := "no-separator";
   Map_1_Key_8 : aliased constant String := "redirect";
   Map_1_Key_9 : aliased constant String := "location";
   Map_1_Key_10 : aliased constant String := "separator-if-needed";
   Map_1_Keys : constant array (0 .. 10) of access constant String
     := (Map_1_Key_0'Access,
         Map_1_Key_1'Access,
         Map_1_Key_2'Access,
         Map_1_Key_3'Access,
         Map_1_Key_4'Access,
         Map_1_Key_5'Access,
         Map_1_Key_6'Access,
         Map_1_Key_7'Access,
         Map_1_Key_8'Access,
         Map_1_Key_9'Access,
         Map_1_Key_10'Access);
   Map_1_Elements : constant array (0 .. 10) of Endpoint_Command
     := (Data_Path,
         Data_Path,
         Data_Path,
         Data_Path,
         Force_Separator,
         Invalid_Log,
         Key,
         No_Separator,
         Redirect,
         Redirect,
         Separator_If_Needed);

end Simple_Webapps.Commands.Append_Servers;
