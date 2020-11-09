package Gade.Audio.Timers is

   type Timer_Type is tagged private; -- Audio Timer?

   procedure Setup (T : out Timer_Type); -- Used for initialization, have something clearer?

   procedure Start (T : in out Timer_Type'Class; Ticks : Positive);

   procedure Enable (T : in out Timer_Type; Enable : Boolean := True);

   procedure Disable (T : in out Timer_Type);

   --  Reload the timer, while preserving enabled state.
   procedure Reload (T : in out Timer_Type; Ticks : Positive);

   --  Reload the timer, while setting enabled state.
   procedure Reload (T : in out Timer_Type; Ticks : Positive; Enable : Boolean);

   generic
      type Observer_Type is abstract tagged limited private;
      with procedure Notify (Observer : in out Observer_Type);
   procedure Tick_Notify (T        : in out Timer_Type;
                          Observer : in out Observer_Type);

   function Has_Finished (T : Timer_Type) return Boolean;

   function Is_Enabled (T : Timer_Type) return Boolean;

   function Ticks_Remaining (T : Timer_Type) return Natural;

private

   subtype Tick_Step_Type is Natural range 0 .. 1;

   type Timer_Type is tagged record
      Remaining : Natural;
      Step      : Tick_Step_Type;
   end record;

end Gade.Audio.Timers;
