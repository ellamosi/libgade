with Ada.Exceptions;
with Ada.Text_IO;

with Testd.Sessions;

procedure Gade_Testd is

   use Ada.Exceptions;
   use Ada.Text_IO;

   S : Testd.Sessions.Session;

begin
   Testd.Sessions.Initialize (S);

   while not End_Of_File loop
      declare
         Raw_Line : constant String := Get_Line;
      begin
         exit when Testd.Sessions.Process_Line (S, Raw_Line);
      exception
         when End_Error =>
            exit;
      end;
   end loop;

   Testd.Sessions.Finalize (S);

exception
   when E : others =>
      Testd.Sessions.Reply_Fatal (Exception_Message (E));
end Gade_Testd;
