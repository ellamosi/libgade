package Gade.Logging is

   type Log_Level is (Debug, Info, Warn, Error);

   type Logger_Interface is interface;
   procedure Log
     (Logger  : in out Logger_Interface;
      Level   : Log_Level;
      Message : String) is abstract;

   type Logger_Access is access all Logger_Interface'Class;

   procedure Set_Logger (Logger : Logger_Access);

   procedure Debug (Message : String);
   procedure Info (Message : String);
   procedure Warn (Message : String);
   procedure Error (Message : String);

end Gade.Logging;
