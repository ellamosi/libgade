package Testd_Session.Commands is

   function Dispatch_Command
     (S       : in out Session;
      Command : String;
      Line    : String;
      Pos     : in out Positive) return Boolean;

end Testd_Session.Commands;
