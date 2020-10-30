package Gade.Audio.Timers is

   --  TODO: Cleanup this interface

   type Timer_Type is tagged private;

   procedure Setup (T : out Timer_Type);

   procedure Setup (T : out Timer_Type; Ticks : Positive);

   procedure Reload (T : in out Timer_Type; Ticks : Positive);

   procedure Start (T : in out Timer_Type'Class; Ticks : Positive);

   procedure Stop (T : in out Timer_Type);

   procedure Pause (T : in out Timer_Type);

   procedure Resume (T : in out Timer_Type);

   procedure Tick (T : in out Timer_Type);

   generic
      type Observer_Type is abstract tagged private;
      with procedure Finished (Observer : in out Observer_Type);
   procedure Tick_Notify (T : in out Timer_Type; Observer : in out Observer_Type);

   function Has_Finished (T : Timer_Type) return Boolean;

   function Enabled (T : Timer_Type) return Boolean;

   function Ticks_Remaining (T : Timer_Type) return Natural;

private

   subtype Tick_Step_Type is Natural range 0 .. 1;

   type Timer_Type is tagged record
      Remaining : Natural;
      Step      : Tick_Step_Type;
   end record;

end Gade.Audio.Timers;
