package body Gade.Audio.Timers is

   procedure Setup (T : out Timer) is
   begin
      T.Remaining := 1;
      Stop (T);
   end Setup;

   procedure Start (T : in out Timer'Class; Ticks : Positive) is
   begin
      T.Remaining := Ticks;
      T.Step := 1;
   end Start;

   procedure Stop (T : in out Timer) is
   begin
      T.Step := 0;
   end Stop;

   procedure Pause (T : in out Timer) is
   begin
      T.Step := 0;
   end Pause;

   procedure Resume (T : in out Timer) is
   begin
      T.Step := 1;
   end Resume;

   procedure Tick (T : in out Timer) is
   begin
      T.Remaining := T.Remaining - T.Step;
   end Tick;

   procedure Tick_Notify (T : in out Timer; Observer : in out Observer_Type) is
   begin
      Tick (T);
      if Has_Finished (T) and Enabled (T) then Finished (Observer); end if;
   end Tick_Notify;

   --  TODO: Consistency in boolean naming
   function Has_Finished (T : Timer) return Boolean is
   begin
      return T.Remaining = 0;
   end Has_Finished;

   function Enabled (T : Timer) return Boolean is
   begin
      return T.Step /= 0;
   end Enabled;

   function Ticks_Remaining (T : Timer) return Natural is
   begin
      return T.Remaining;
   end Ticks_Remaining;

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
      T.Step := 1;
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

   procedure Tick_Notify_Repeatable
     (T        : in out Repeatable_Timer;
      Observer : in out Observer_Type)
   is
      procedure Tick_Notify_Non_Repeatable is new Tick_Notify
        (Observer_Type, Finished);
   begin
      --  This method could very well be inherited, but there are some
      --  limitations when using generic procedures that prevent that.
      Tick_Notify_Non_Repeatable (Timer (T), Observer);
   end Tick_Notify_Repeatable;

end Gade.Audio.Timers;
