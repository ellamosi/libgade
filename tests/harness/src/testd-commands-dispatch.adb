with Ada.Strings.Fixed;

with Testd.Commands.Handlers;

package body Testd.Commands.Dispatch is

   use Ada.Strings;
   use Ada.Strings.Fixed;

   function Dispatch_Command
     (S       : in out Session;
      Command : String;
      Line    : String;
      Pos     : in out Positive) return Boolean is
   begin
      if Command = "PING" then
         Testd.Sessions.Reply_OK;

      elsif Command = "QUIT" then
         Testd.Sessions.Reply_OK;
         return True;

      elsif Command = "RESET" then
         Testd.Commands.Handlers.Reset (S);

      elsif Command = "LOAD" then
         Testd.Commands.Handlers.Load (S, Line, Pos);

      elsif Command = "PRESS" then
         Testd.Commands.Handlers.Press_Release
           (S, Line, Pos, Command, Pressed => True);

      elsif Command = "RELEASE" then
         Testd.Commands.Handlers.Press_Release
           (S, Line, Pos, Command, Pressed => False);

      elsif Command = "SET_INPUT" then
         Testd.Commands.Handlers.Set_Input (S, Line, Pos);

      elsif Command = "RUN" then
         Testd.Commands.Handlers.Run (S, Line, Pos);

      elsif Command = "FRAME_INDEX" then
         Testd.Sessions.Reply_OK (Trim (Natural'Image (S.Frame_Count), Both));

      elsif Command = "STATE" then
         Testd.Commands.Handlers.State (S);

      elsif Command = "SAVE_FRAME" then
         Testd.Commands.Handlers.Save_Frame (S, Line, Pos);

      elsif Command = "MATCH_FRAME" then
         Testd.Commands.Handlers.Match_Frame (S, Line, Pos);

      elsif Command = "FIND_MATCH" then
         Testd.Commands.Handlers.Find_Match (S, Line, Pos);

      else
         Testd.Sessions.Reply_ERR ("BAD_CMD", "Unknown command: " & Command);
      end if;

      return False;
   end Dispatch_Command;

end Testd.Commands.Dispatch;
