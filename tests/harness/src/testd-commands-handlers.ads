with Testd.Sessions; use Testd.Sessions;

package Testd.Commands.Handlers is

   procedure Reset (S : in out Session);
   procedure Load (S : in out Session; Line : String; Pos : in out Positive);
   procedure Press_Release
     (S       : in out Session;
      Line    : String;
      Pos     : in out Positive;
      Command : String;
      Pressed : Boolean);
   procedure Set_Input
     (S : in out Session; Line : String; Pos : in out Positive);
   procedure Run (S : in out Session; Line : String; Pos : in out Positive);
   procedure Save_Frame
     (S : in out Session; Line : String; Pos : in out Positive);
   procedure Match_Frame
     (S : in out Session; Line : String; Pos : in out Positive);
   procedure Find_Match
     (S : in out Session; Line : String; Pos : in out Positive);

end Testd.Commands.Handlers;
