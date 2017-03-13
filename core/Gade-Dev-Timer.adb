with Ada.Text_IO; use Ada.Text_IO;

with Gade.GB;             use Gade.GB;
with Gade.Dev.Interrupts; use Gade.Dev.Interrupts;

package body Gade.Dev.Timer is

   procedure Reset (Timer : in out Timer_Type) is
   begin
      Timer.Ticks := 0;
      Timer.Map.Space := (others => 0);
      Timer.Modulo_Ticks :=
        TIMA_Clocks(Timer.Map.Timer_Control.Input_Clock_Select);
   end Reset;

   procedure Read
     (Timer   : in out Timer_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Value   : out Byte) is
   begin
      if Address = DIV then
         Value := Byte(Timer.Ticks mod 256);
      else
         Value := Timer.Map.Space(Address);
      end if;
   end Read;

   procedure Write
     (Timer   : in out Timer_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Value   : Byte) is
   begin
      if Address = TAC then
         -- Might need to reset ticks too, needs research
         Timer.Modulo_Ticks :=
           TIMA_Clocks(Timer.Map.Timer_Control.Input_Clock_Select);
      end if;
      Timer.Map.Space(Address) := Value;
   end Write;

   function Is_Running (Timer : Timer_Type) return Boolean is
   begin
      return Timer.Map.Timer_Control.Timer_Stop = Start;
   end Is_Running;

   procedure Report_Cycle (Timer : in out Timer_Type;
                           GB    : in out Gade.GB.GB_Type) is
      New_Ticks : Natural;
   begin
      New_Ticks := (Timer.Ticks + 1) mod Timer.Modulo_Ticks;
      if Is_Running(Timer) and New_Ticks < Timer.Ticks then
         Timer.Map.Timer_Counter := Timer.Map.Timer_Counter + 1;
         if Timer.Map.Timer_Counter = 0 then
            Timer.Map.Timer_Counter := Timer.Map.Timer_Modulo;
            Set_Interrupt(GB, Timer_Interrupt);
         end if;
      end if;
      Timer.Ticks := New_Ticks;
   end Report_Cycle;

end Gade.Dev.Timer;
