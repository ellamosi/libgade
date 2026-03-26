with Gade.GB;             use Gade.GB;
with Gade.Dev.Interrupts; use Gade.Dev.Interrupts;

package body Gade.Dev.Timer is

   overriding
   procedure Reset (Timer : in out Timer_Type) is
   begin
      Timer.Map.Space := [others => 0];
      Timer.Ticks := 0;
      Timer.Modulo_Ticks := TIMA_Clocks (Timer.Map.Timer_Control.Input_Clock_Select);
      Timer.DIV_Ticks := 0;
   end Reset;

   overriding
   procedure Read
     (Timer   : in out Timer_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Value   : out Byte)
   is
      pragma Unreferenced (GB);
   begin
      if Address = DIV then
         Value := Byte (Timer.DIV_Ticks / 256);
      else
         Value := Timer.Map.Space (Address);
      end if;
   end Read;

   overriding
   procedure Write
     (Timer   : in out Timer_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Value   : Byte)
   is
      pragma Unreferenced (GB);
   begin
      Timer.Map.Space (Address) := Value;
      if Address = TAC then
         --  Might need to reset internal ticks too, needs research
         Timer.Modulo_Ticks := TIMA_Clocks (Timer.Map.Timer_Control.Input_Clock_Select);
      elsif Address = DIV then
         --  This will preserve the internal DIV timing
         Timer.DIV_Ticks := Timer.DIV_Ticks mod 256;
      end if;
   end Write;

   function Is_Running (Timer : Timer_Type) return Boolean is
   begin
      return Timer.Map.Timer_Control.Timer_Stop = Start;
   end Is_Running;

   overriding
   procedure Report_Cycles
     (Timer : in out Timer_Type; GB : in out Gade.GB.GB_Type; Cycles : M_Cycle_Count)
   is
      --  Timer edge detection remains T-cycle based even though the emulator
      --  scheduler now reports time in M-cycles.
      T_Cycles                       : constant T_Cycle_Count := To_T_Cycles (Cycles);
      New_Ticks                      : T_Cycle_Count;
      New_Counter, Counter_Increment : Integer;
   begin
      if Is_Running (Timer) then
         New_Ticks := Timer.Ticks + T_Cycles;
         Timer.Ticks := New_Ticks mod Timer.Modulo_Ticks;
         if New_Ticks >= Timer.Modulo_Ticks then
            Counter_Increment := Integer (New_Ticks / Timer.Modulo_Ticks);
            New_Counter := Integer (Timer.Map.Timer_Counter);
            for I in 1 .. Counter_Increment loop
               pragma Unreferenced (I);
               if New_Counter = 255 then
                  --  Reload TMA on overflow instead of wrapping to 0. Games
                  --  that program periodic timer IRQs depend on that shorter
                  --  post-overflow period.
                  New_Counter := Integer (Timer.Map.Timer_Modulo);
                  Set_Interrupt (GB, Timer_Interrupt);
               else
                  New_Counter := New_Counter + 1;
               end if;
            end loop;
            Timer.Map.Timer_Counter := Byte (New_Counter);
         end if;
      end if;
      --  DIV exposes the upper byte of a free-running 16-bit divider.
      Timer.DIV_Ticks := (Timer.DIV_Ticks + T_Cycles) mod DIV_Counter_Modulus;
   --  Put_Line("Ticks" & Timer.Ticks'Img & " Counter" &
   --  Timer.Map.Timer_Counter'Img & " Modulo" & Timer.Modulo_Ticks'Img);
   end Report_Cycles;

end Gade.Dev.Timer;
