package body Gade.Audio.Timers is

   procedure Setup (T : out Timer_Type) is
   begin
      T.Reload (1, False);
   end Setup;

   procedure Reload (T : in out Timer_Type; Ticks : Positive) is
   begin
      T.Remaining := Ticks;
   end Reload;

   procedure Reload (T : in out Timer_Type; Ticks : Positive; Enable : Boolean) is
   begin
      T.Reload (Ticks);
      T.Enable (Enable);
   end Reload;

   procedure Start (T : in out Timer_Type'Class; Ticks : Positive) is
   begin
      T.Reload (Ticks, True);
   end Start;

   procedure Enable (T : in out Timer_Type; Enable : Boolean := True) is
   begin
      T.Step := (if Enable then 1 else 0);
   end Enable;

   procedure Disable (T : in out Timer_Type) is
   begin
      T.Enable (False);
   end Disable;

   procedure Tick_Notify
     (T : in out Timer_Type;
      Observer : in out Observer_Type)
   is
   begin
      T.Remaining := T.Remaining - T.Step;
      if T.Has_Finished and T.Is_Enabled then
         T.Disable;
         Notify (Observer);
      end if;
   end Tick_Notify;

   function Has_Finished (T : Timer_Type) return Boolean is
   begin
      return T.Remaining = 0;
   end Has_Finished;

   function Is_Enabled (T : Timer_Type) return Boolean is
   begin
      return T.Step /= 0;
   end Is_Enabled;

   function Ticks_Remaining (T : Timer_Type) return Natural is
   begin
      return T.Remaining;
   end Ticks_Remaining;

end Gade.Audio.Timers;
