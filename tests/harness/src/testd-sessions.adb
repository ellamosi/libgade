with Ada.Characters.Handling;
with Ada.Exceptions;
with Ada.Strings.Fixed;

with Testd.Commands.Dispatch;
with Testd.Protocol;

package body Testd.Sessions is

   use Ada.Characters.Handling;
   use Ada.Exceptions;
   use Ada.Strings;
   use Ada.Strings.Fixed;
   use Ada.Text_IO;

   procedure Reply_OK (Payload : String := "") is
   begin
      if Payload'Length = 0 then
         Put_Line (Standard_Output, "OK");
      else
         Put_Line (Standard_Output, "OK " & Payload);
      end if;
      Flush (Standard_Output);
   end Reply_OK;

   procedure Reply_ERR (Code : String; Message : String) is
   begin
      Put_Line (Standard_Output, "ERR " & Code & " " & Message);
      Flush (Standard_Output);
   end Reply_ERR;

   procedure Initialize (S : in out Session) is
   begin
      Create (S.Dev_Null, Out_File, "/dev/null");
      Set_Output (S.Dev_Null);
   end Initialize;

   procedure Finalize (S : in out Session) is
   begin
      if Is_Open (S.Dev_Null) then
         Close (S.Dev_Null);
      end if;
   end Finalize;

   function Process_Line (S : in out Session; Raw_Line : String) return Boolean
   is
      Line    : constant String := Trim (Raw_Line, Both);
      Pos     : Positive := 1;
      Command : constant String :=
        To_Upper (Testd.Protocol.Next_Token (Line, Pos));
   begin
      if Command = "" then
         Reply_ERR ("BAD_CMD", "Empty command");
         return False;
      end if;

      return Testd.Commands.Dispatch.Dispatch_Command (S, Command, Line, Pos);
   exception
      when E : others =>
         Reply_ERR ("INTERNAL", Exception_Message (E));
         return False;
   end Process_Line;

   procedure Reply_Fatal (Message : String) is
   begin
      Put_Line (Standard_Output, "ERR FATAL " & Message);
   exception
      when others =>
         null;
   end Reply_Fatal;

end Testd.Sessions;
