--  Generated at 2014-07-02 17:53:59 +0000 by Natools.Static_Hash_Maps
--  from src/simple_webapps-upload_servers-commands.sx

package Simple_Webapps.Commands.Upload_Servers is
   pragma Pure;

   type Config_Command is
     (Config_Error,
      Set_Storage_File,
      Set_Directory,
      Set_Error_Template,
      Set_HMAC_Key,
      Set_Index_Template,
      Set_Input_Dir,
      Set_Max_Expiration,
      Set_Report_Template,
      Set_Static_Dir);

   type File_Command is
     (File_Error,
      Set_Name,
      Set_Comment,
      Set_Download,
      Set_Expiration,
      Set_MIME_Type,
      Set_Upload);

   function To_Config_Command (Key : String) return Config_Command;
   function To_File_Command (Key : String) return File_Command;

private

   Map_1_Key_0 : aliased constant String := "backend";
   Map_1_Key_1 : aliased constant String := "directory";
   Map_1_Key_2 : aliased constant String := "error-template";
   Map_1_Key_3 : aliased constant String := "hmac-key";
   Map_1_Key_4 : aliased constant String := "index-template";
   Map_1_Key_5 : aliased constant String := "input-directory";
   Map_1_Key_6 : aliased constant String := "max-expiration";
   Map_1_Key_7 : aliased constant String := "report-template";
   Map_1_Key_8 : aliased constant String := "static";
   Map_1_Key_9 : aliased constant String := "static-dir";
   Map_1_Key_10 : aliased constant String := "static-resources";
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
   Map_1_Elements : constant array (0 .. 10) of Config_Command
     := (Set_Storage_File,
         Set_Directory,
         Set_Error_Template,
         Set_HMAC_Key,
         Set_Index_Template,
         Set_Input_Dir,
         Set_Max_Expiration,
         Set_Report_Template,
         Set_Static_Dir,
         Set_Static_Dir,
         Set_Static_Dir);

   Map_2_Key_0 : aliased constant String := "name";
   Map_2_Key_1 : aliased constant String := "comment";
   Map_2_Key_2 : aliased constant String := "download-key";
   Map_2_Key_3 : aliased constant String := "expire";
   Map_2_Key_4 : aliased constant String := "mime-type";
   Map_2_Key_5 : aliased constant String := "upload";
   Map_2_Keys : constant array (0 .. 5) of access constant String
     := (Map_2_Key_0'Access,
         Map_2_Key_1'Access,
         Map_2_Key_2'Access,
         Map_2_Key_3'Access,
         Map_2_Key_4'Access,
         Map_2_Key_5'Access);
   Map_2_Elements : constant array (0 .. 5) of File_Command
     := (Set_Name,
         Set_Comment,
         Set_Download,
         Set_Expiration,
         Set_MIME_Type,
         Set_Upload);

end Simple_Webapps.Commands.Upload_Servers;
