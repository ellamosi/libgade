with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;
with GNAT.OS_Lib;      use GNAT.OS_Lib;

procedure Run_Tests is
   Python_Exec : String_Access := Locate_Exec_On_Path ("python3");
   Args        : Argument_List_Access;
   Status      : Integer;
begin
   if Python_Exec = null then
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error,
         "error: could not find 'python3' in PATH");
      Set_Exit_Status (Failure);
      return;
   end if;

   Args := new Argument_List (1 .. Argument_Count + 1);
   Args (1) := new String'("run.py");
   for I in 1 .. Argument_Count loop
      Args (I + 1) := new String'(Argument (I));
   end loop;

   Status := Spawn (Program_Name => Python_Exec.all, Args => Args.all);
   if Status = 0 then
      Set_Exit_Status (Success);
   else
      Set_Exit_Status (Failure);
   end if;

   for I in Args'Range loop
      Free (Args (I));
   end loop;
   Free (Args);
   Free (Python_Exec);
end Run_Tests;
