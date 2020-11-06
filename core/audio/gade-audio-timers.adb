package body Gade.Audio.Timers is

   procedure Setup (T : out Timer_Type) is
   begin
      Setup (T, 1);
   end Setup;

   procedure Setup (T : out Timer_Type; Ticks : Positive) is
   begin
      T.Remaining := Ticks;
      Stop (T);
   end Setup;

   procedure Reload (T : in out Timer_Type; Ticks : Positive) is
   begin
      T.Remaining := Ticks;
   end Reload;

   procedure Start (T : in out Timer_Type'Class; Ticks : Positive) is
   begin
      T.Remaining := Ticks;
      T.Step := 1;
   end Start;

   procedure Stop (T : in out Timer_Type) is
      --  TODO: Remove in lieu of Pause
   begin
      T.Step := 0;
   end Stop;

   procedure Pause (T : in out Timer_Type) is
   begin
      T.Step := 0;
   end Pause;

   procedure Resume (T : in out Timer_Type) is
   begin
      T.Step := 1;
   end Resume;

   procedure Tick (T : in out Timer_Type) is
   begin
      T.Remaining := T.Remaining - T.Step;
   end Tick;

   procedure Tick_Notify
     (T : in out Timer_Type;
      Observer : in out Observer_Type)
   is
   begin
      Tick (T);
      if Has_Finished (T) and Enabled (T) then
         Pause (T);
         Notify (Observer);
      end if;
   end Tick_Notify;

   --  TODO: Consistency in boolean naming
   function Has_Finished (T : Timer_Type) return Boolean is
   begin
      return T.Remaining = 0;
   end Has_Finished;

   function Enabled (T : Timer_Type) return Boolean is
   begin
      return T.Step /= 0;
   end Enabled;

   function Ticks_Remaining (T : Timer_Type) return Natural is
   begin
      return T.Remaining;
   end Ticks_Remaining;

end Gade.Audio.Timers;
