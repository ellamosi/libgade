with Ada.Text_IO; use Ada.Text_IO;

with Gade.GB;             use Gade.GB;
with Gade.Dev.Interrupts; use Gade.Dev.Interrupts;

package body Gade.Dev.Timer is

   procedure Reset (Timer : in out Timer_Type) is
   begin
      Timer.Clocks := 0;
      Timer.Map.Space := (others => 0);
   end Reset;

   procedure Read
     (Timer   : in out Timer_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Value   : out Byte) is
   begin
      if Address = DIV then
         Value := Byte(Timer.Clocks mod 256);
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
      Timer.Map.Space(Address) := Value;
   end Write;

   procedure Report_Cycle (Timer : in out Timer_Type;
                           GB    : in out Gade.GB.GB_Type) is
   begin
      Timer.Clocks := Timer.Clocks + 1; -- TODO: Types need to be modular
      if Timer.Map.Timer_Control.Timer_Stop = Start then
         -- Timer.Clocks := Timer.Clocks + 1;
         -- Put_Line("Clocks " & Timer.Clocks'Img & " Counter " & Timer.Timer_Counter'Img);
         if Timer.Clocks >= TIMA_Clocks(Timer.Map.Timer_Control.Input_Clock_Select)
         then
            Timer.Clocks := 0;
            Timer.Map.Timer_Counter := Timer.Map.Timer_Counter + 1;
            if Timer.Map.Timer_Counter = 0 then
               Timer.Map.Timer_Counter := Timer.Map.Timer_Modulo;
               Set_Interrupt(GB, Timer_Interrupt);
            end if;
         end if;
      end if;
   end Report_Cycle;

end Gade.Dev.Timer;
