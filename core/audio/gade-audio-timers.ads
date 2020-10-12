package Gade.Audio.Timers is

   --  TODO: Cleanup this interface

   type Timer is tagged private;

   procedure Setup (T : out Timer);

   procedure Setup (T : out Timer; Ticks : Positive);

   procedure Start (T : in out Timer'Class; Ticks : Positive);

   procedure Stop (T : in out Timer);

   procedure Pause (T : in out Timer);

   procedure Resume (T : in out Timer);

   procedure Tick (T : in out Timer);

   generic
      type Observer_Type is abstract tagged private;
      with procedure Finished (Observer : in out Observer_Type);
   procedure Tick_Notify (T : in out Timer; Observer : in out Observer_Type);

   function Has_Finished (T : Timer) return Boolean;

   function Enabled (T : Timer) return Boolean;

   function Ticks_Remaining (T : Timer) return Natural;


   type Repeatable_Timer is new Timer with private;

   overriding
   procedure Setup (T : out Repeatable_Timer);

   overriding
   procedure Setup (T : out Repeatable_Timer; Ticks : Positive);

   procedure Start (T : in out Repeatable_Timer);

   procedure Reset (T : in out Repeatable_Timer);

   overriding
   procedure Stop (T : in out Repeatable_Timer);

   generic
      type Observer_Type is abstract tagged private;
      with procedure Finished (Observer : in out Observer_Type);
   procedure Tick_Notify_Repeatable
     (T        : in out Repeatable_Timer;
      Observer : in out Observer_Type);

private

   subtype Tick_Step_Type is Natural range 0 .. 1;

   type Timer is tagged record
      Remaining : Natural;
      Step      : Tick_Step_Type;
   end record;

   type Repeatable_Timer is new Timer with record
      Initial : Positive;
   end record;

end Gade.Audio.Timers;
