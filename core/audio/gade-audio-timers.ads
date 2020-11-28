package Gade.Audio.Timers is

   type Timer_Type is tagged private;

   --  Start timer for a number of ticks.
   procedure Start (T : in out Timer_Type'Class; Ticks : Positive);

   --  If Enable = True => Continue ticking the timer from where it was left.
   --  Otherwise => Disables the timer (see below).
   procedure Enable (T : in out Timer_Type; Enable : Boolean := True);

   --  Disable the timer, ticking it will have no effects, remaining ticks
   --  will be preserved until re-enabled or reloaded.
   procedure Disable (T : in out Timer_Type);

   --  Reload the timer, while preserving enabled state.
   procedure Reload (T : in out Timer_Type; Ticks : Positive);

   --  Reload the timer, while setting enabled state.
   procedure Reload (T : in out Timer_Type; Ticks : Positive; Enable : Boolean);

   --  Ticks the timer, triggering Notify if the last tick is carried out and
   --  disabling the timer. No ticings will be carried out nor Notify will be
   --  triggered if the timer was disabled.
   generic
      type Observer_Type is abstract tagged limited private;
      with procedure Notify (Observer : in out Observer_Type);
   procedure Tick_Notify
     (T        : in out Timer_Type;
      Observer : in out Observer_Type);

   function Has_Finished (T : Timer_Type) return Boolean;

   function Is_Enabled (T : Timer_Type) return Boolean;

   function Ticks_Remaining (T : Timer_Type) return Natural;

private

   subtype Tick_Step_Type is Natural range 0 .. 1;

   type Timer_Type is tagged record
      Remaining : Natural        := 0;
      Step      : Tick_Step_Type := 0;
   end record;

end Gade.Audio.Timers;
