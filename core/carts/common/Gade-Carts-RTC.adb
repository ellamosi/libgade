package body Gade.Carts.RTC is

   procedure Reset (Clk : in out Clock) is
   begin
      --  An actual reset of the GB would not result in the RTC resetting.
      null;
   end Reset;

   procedure Read
     (Clk   : in out Clock;
      Reg   : Register;
      Value : out Byte) is
   begin
      Value := Clk.Registers.Indexed (Reg);
   end Read;

   procedure Write
     (Clk   : in out Clock;
      Reg   : Register;
      Value : Byte)
   is
      Regs : Counter_Registers;
   begin
      Regs.Indexed (Reg) := Value;
      To_Counter (Regs, Clk.Elapsed);
   end Write;

   procedure Latch (Clk : in out Clock) is
   begin
      Clk.Latched := Clk.Elapsed;
      To_Registers (Clk.Latched, Clk.Registers);
   end Latch;

   procedure Report_Cycles
     (Clk    : in out Clock;
      Cycles : Positive)
   is
      New_Cycles, Seconds : Natural;
   begin
      New_Cycles := Clk.Cycles + Cycles;
      Clk.Cycles := New_Cycles mod Cycles_Per_Second;
      if New_Cycles > Cycles_Per_Second and not Clk.Elapsed.Halted then
         Seconds := New_Cycles / Cycles_Per_Second;
         Increase_Counter (Clk.Elapsed, Duration (Seconds));
      end if;
   end Report_Cycles;

   procedure Increase_Counter
     (C    : in out Counter;
      Span : Duration)
   is
   begin
      C.Span := C.Span + Span;
      while C.Span >= Max_Span loop
         C.Carry := True;
         C.Span := Duration'Max (C.Span - Max_Span, 0.0);
      end loop;
   end Increase_Counter;

   function Elapsed (Regs : Counter_Registers) return Duration is
      Seconds : Natural;
   begin
      Seconds :=
        Natural (Regs.Seconds) +
        Natural (Regs.Minutes) * 60 +
        Natural (Regs.Hours)   * 60 * 60 +
        Natural (Regs.Days)    * 24 * 60 * 60;
      return Duration (Seconds);
   end Elapsed;

   procedure To_Counter
     (C_Regs : Counter_Registers;
      C      : out Counter)
   is
   begin
      C.Span   := Elapsed (C_Regs);
      C.Halted := C_Regs.Halted;
      C.Carry  := C_Regs.Carry;
   end To_Counter;

   procedure To_Registers
     (C      : Counter;
      C_Regs : out Counter_Registers)
   is
      Span_Seconds : constant Natural := Natural (C.Span);
      Span_Minutes : constant Natural := Span_Seconds / 60;
      Span_Hours   : constant Natural := Span_Minutes / 60;
      Span_Days    : constant Natural := Span_Hours / 24;
   begin
      C_Regs.Seconds := Byte (Span_Seconds mod 60);
      C_Regs.Minutes := Byte (Span_Minutes mod 60);
      C_Regs.Hours   := Byte (Span_Hours   mod 24);

      --  Clear unused bits (clear value not really known)
      C_Regs.Indexed (Days_High) := 0;

      C_Regs.Days   := Day_Count (Span_Days mod Day_Count_Cardinality);
      C_Regs.Carry  := C.Carry;
      C_Regs.Halted := C.Halted;
   end To_Registers;

end Gade.Carts.RTC;
