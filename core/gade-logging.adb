package body Gade.Logging is

   type Null_Logger is new Logger_Interface with null record;

   overriding
   procedure Log
     (Logger  : in out Null_Logger;
      Level   : Log_Level;
      Message : String) is
      pragma Unreferenced (Logger, Level, Message);
   begin
      null;
   end Log;

   Null_Logger_Instance : aliased Null_Logger;
   Current_Logger       : Logger_Access := Null_Logger_Instance'Access;

   procedure Write
     (Level   : Log_Level;
      Message : String);

   procedure Set_Logger (Logger : Logger_Access) is
   begin
      if Logger = null then
         Current_Logger := Null_Logger_Instance'Access;
      else
         Current_Logger := Logger;
      end if;
   end Set_Logger;

   procedure Write
     (Level   : Log_Level;
      Message : String) is
   begin
      Current_Logger.Log (Level, Message);
   end Write;

   procedure Debug (Message : String) is
   begin
      Write (Debug, Message);
   end Debug;

   procedure Info (Message : String) is
   begin
      Write (Info, Message);
   end Info;

   procedure Warn (Message : String) is
   begin
      Write (Warn, Message);
   end Warn;

   procedure Error (Message : String) is
   begin
      Write (Error, Message);
   end Error;

end Gade.Logging;
