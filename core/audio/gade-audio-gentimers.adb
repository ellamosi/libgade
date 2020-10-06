package body Gade.Audio.GenTimers is

   procedure Setup (T : out Timer) is
   begin
      T.Remaining := 1;
      Stop (T);
   end Setup;

   procedure Start (T : in out Timer'Class; Ticks : Positive) is
   begin
      T.Remaining := Ticks;
      T.Tick_Step := 1;
   end Start;

   procedure Stop (T : in out Timer) is
   begin
      T.Tick_Step := 0;
   end Stop;

   procedure Tick (T : in out Timer) is
   begin
      T.Remaining := T.Remaining - T.Tick_Step;
   end Tick;

   procedure Tick (T : in out Timer; Observer : in out Observer_Type) is
   begin
      Tick (T);
      if Has_Finished (T) then Finished (Observer); end if;
   end Tick;

   --  TODO: Consistency in boolean naming
   function Has_Finished (T : Timer) return Boolean is
   begin
      return T.Remaining = 0;
   end Has_Finished;

   function Enabled (T : Timer) return Boolean is
   begin
      return T.Tick_Step /= 0;
   end Enabled;

   overriding
   procedure Setup (T : out Repeatable_Timer) is
   begin
      Setup (T, 1);
   end Setup;

   procedure Setup (T : out Repeatable_Timer; Ticks : Positive) is
   begin
      T.Initial := Ticks;
      Stop (T);
   end Setup;

   procedure Start (T : in out Repeatable_Timer) is
   begin
      T.Remaining := T.Initial;
      T.Tick_Step := 1;
   end Start;

   procedure Reset (T : in out Repeatable_Timer) is
   begin
      T.Remaining := T.Initial;
   end Reset;

   overriding
   procedure Stop (T : in out Repeatable_Timer) is
   begin
      Timer (T).Stop;
      --  Reset timer, that way we can transparently tick a stopped timer that
      --  had finished.
      Reset (T);
   end Stop;

end Gade.Audio.GenTimers;
