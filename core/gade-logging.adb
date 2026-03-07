package body Gade.Logging is

   type Null_Logger is new Logger_Interface with null record;

   overriding
   procedure Log
     (Logger  : Null_Logger;
      Level   : Log_Level;
      Message : String) is
      pragma Unreferenced (Logger, Level, Message);
   begin
      null;
   end Log;

   Null_Logger_Instance : aliased Null_Logger;
   Default : constant Logger_Access := Null_Logger_Instance'Access;

   function Default_Logger return Logger_Access is
   begin
      return Default;
   end Default_Logger;

   procedure Log
     (Logger  : Logger_Access;
      Level   : Log_Level;
      Message : String) is
      Effective_Logger : constant Logger_Access :=
        (if Logger = null then Default_Logger else Logger);
   begin
      Effective_Logger.Log (Level, Message);
   end Log;

   procedure Debug (Logger : Logger_Access; Message : String) is
   begin
      Log (Logger, Debug, Message);
   end Debug;

   procedure Info (Logger : Logger_Access; Message : String) is
   begin
      Log (Logger, Info, Message);
   end Info;

   procedure Warn (Logger : Logger_Access; Message : String) is
   begin
      Log (Logger, Warn, Message);
   end Warn;

   procedure Error (Logger : Logger_Access; Message : String) is
   begin
      Log (Logger, Error, Message);
   end Error;

end Gade.Logging;
