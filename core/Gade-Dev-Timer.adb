with Gade.GB;             use Gade.GB;
with Gade.Dev.Interrupts; use Gade.Dev.Interrupts;

package body Gade.Dev.Timer is

   procedure Reset (Timer : in out Timer_Type) is
   begin
      Timer.Map.Space := (others => 0);
      Timer.Ticks := 0;
      Timer.Modulo_Ticks :=
        TIMA_Clocks(Timer.Map.Timer_Control.Input_Clock_Select);
      Timer.Div_Ticks := 0;
   end Reset;

   procedure Read
     (Timer   : in out Timer_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Value   : out Byte) is
   begin
      if Address = DIV then
         Value := Byte(Timer.DIV_Ticks / 256);
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
      if Address = TAC then
         -- Might need to reset internal ticks too, needs research
         Timer.Modulo_Ticks :=
           TIMA_Clocks(Timer.Map.Timer_Control.Input_Clock_Select);
      elsif Address = DIV then
         -- This will preserve the internal DIV timing
         Timer.DIV_Ticks := Timer.DIV_Ticks mod 256;
      end if;
   end Write;

   function Is_Running (Timer : Timer_Type) return Boolean is
   begin
      return Timer.Map.Timer_Control.Timer_Stop = Start;
   end Is_Running;

   procedure Report_Cycles
     (Timer  : in out Timer_Type;
      GB     : in out Gade.GB.GB_Type;
      Cycles : Positive)
   is
      New_Ticks, New_Counter, Counter_Increment : Integer;
   begin
      if Is_Running(Timer) then
         New_Ticks := (Timer.Ticks + Cycles);
         Timer.Ticks := New_Ticks mod Timer.Modulo_Ticks;
         if New_Ticks >= Timer.Modulo_Ticks then
            --Put_Line("Counter" & Timer.Map.Timer_Counter'Img);
            Counter_Increment := New_Ticks / Timer.Modulo_Ticks;
            New_Counter := Integer(Timer.Map.Timer_Counter) + Counter_Increment;
            if New_Counter >= 256 then
               Timer.Map.Timer_Counter := Timer.Map.Timer_Modulo;
               Set_Interrupt(GB, Timer_Interrupt);
            end if;
            Timer.Map.Timer_Counter := Byte(New_Counter mod 256);
         end if;
      end if;
      Timer.Div_Ticks := (Timer.DIV_Ticks + Cycles) mod 256*256;
      -- Put_Line("Ticks" & Timer.Ticks'Img & " Counter" & Timer.Map.Timer_Counter'Img & " Modulo" & Timer.Modulo_Ticks'Img);
   end Report_Cycles;

end Gade.Dev.Timer;
