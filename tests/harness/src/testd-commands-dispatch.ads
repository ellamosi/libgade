with Testd.Sessions; use Testd.Sessions;

package Testd.Commands.Dispatch is

   function Dispatch_Command
     (S       : in out Session;
      Command : String;
      Line    : String;
      Pos     : in out Positive) return Boolean;

end Testd.Commands.Dispatch;
