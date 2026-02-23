with Ada.Command_Line;
with Ada.Directories;

package body Test_Directories is

   function Program_Abspath return String is
      Command_Name : constant String := Ada.Command_Line.Command_Name;
   begin
      return Ada.Directories.Full_Name (Command_Name);
   end Program_Abspath;

   function Test_Dir return String is
   begin
      return
     Ada.Directories.Containing_Directory
       (Ada.Directories.Containing_Directory (Program_Abspath));
   end Test_Dir;

end Test_Directories;
