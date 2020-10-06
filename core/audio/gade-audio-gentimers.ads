generic
   type Observer_Type is abstract tagged private;
   with procedure Finished (Observer : in out Observer_Type);
package Gade.Audio.GenTimers is

   type Timer is tagged private;

   procedure Setup (T : out Timer);

   procedure Start (T : in out Timer'Class; Ticks : Positive);

   procedure Stop (T : in out Timer);

   procedure Tick (T : in out Timer);

   procedure Tick (T : in out Timer; Observer : in out Observer_Type);

   function Has_Finished (T : Timer) return Boolean;

   function Enabled (T : Timer) return Boolean;


   type Repeatable_Timer is new Timer with private;

   overriding
   procedure Setup (T : out Repeatable_Timer);

   procedure Setup (T : out Repeatable_Timer; Ticks : Positive);

   procedure Start (T : in out Repeatable_Timer);

   procedure Reset (T : in out Repeatable_Timer);

   overriding
   procedure Stop (T : in out Repeatable_Timer);

private

   subtype Tick_Step_Type is Natural range 0 .. 1;

   type Timer is tagged record
      Remaining : Natural;
      Tick_Step : Tick_Step_Type;
   end record;

   type Repeatable_Timer is new Timer with record
      Initial : Positive;
   end record;

end Gade.Audio.GenTimers;
