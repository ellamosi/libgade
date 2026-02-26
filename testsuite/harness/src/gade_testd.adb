with Ada.Exceptions;
with Ada.Text_IO;

with Testd_Session;

procedure Gade_Testd is

   use Ada.Exceptions;
   use Ada.Text_IO;

   S : Testd_Session.Session;

begin
   Testd_Session.Initialize (S);

   while not End_Of_File loop
      declare
         Raw_Line : constant String := Get_Line;
      begin
         exit when Testd_Session.Process_Line (S, Raw_Line);
      exception
         when End_Error =>
            exit;
      end;
   end loop;

   Testd_Session.Finalize (S);

exception
   when E : others =>
      Testd_Session.Reply_Fatal (Exception_Message (E));
end Gade_Testd;
