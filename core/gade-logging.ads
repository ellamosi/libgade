package Gade.Logging is

   type Log_Level is (Debug, Info, Warn, Error);

   type Logger_Interface is interface;
   procedure Log (Logger : Logger_Interface; Level : Log_Level; Message : String)
   is abstract;

   type Logger_Access is access all Logger_Interface'Class;

   function Default_Logger return Logger_Access;

   procedure Log (Logger : Logger_Access; Level : Log_Level; Message : String);
   procedure Debug (Logger : Logger_Access; Message : String);
   procedure Info (Logger : Logger_Access; Message : String);
   procedure Warn (Logger : Logger_Access; Message : String);
   procedure Error (Logger : Logger_Access; Message : String);

end Gade.Logging;
