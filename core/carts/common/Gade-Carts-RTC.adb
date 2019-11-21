with Ada.Calendar.Formatting; use Ada.Calendar.Formatting;
with Ada.Text_IO;

package body Gade.Carts.RTC is

   function Days (D : Duration) return Natural is
   begin
      return Natural (D / Seconds_Day);
   end Days;

   function Truncate (D : Duration) return Day_Duration is
      Day_Count : constant Natural := Days (D);
   begin
      return Maximum (D - Day_Count * Seconds_Day, 0.0);
   end Truncate;

   function Maximum (Left, Right : Duration) return Duration is
   begin
      return (if Left > Right then Left else Right);
   end Maximum;

   function Duration_Image (D : Duration) return String is
      Days_Img : constant String := Days (D)'Img;
   begin
      return
        Days_Img (Days_Img'First + 1 .. Days_Img'Last) &
        "d " & Image (Truncate (D));
   end Duration_Image;

   function Counter_Image (C : Counter) return String is
      Halted    : String := "H: ";
      Overflown : String := "O: ";
   begin
      Halted (Halted'Last)       := (if C.Halted then 't' else 'f');
      Overflown (Overflown'Last) := (if C.Carry then 't' else 'f');
      return Duration_Image (C.Span) & ' ' & Overflown & ' ' & Halted;
   end Counter_Image;

   procedure Print (Clk : Clock) is
   begin
      Ada.Text_IO.Put_Line ("  Elapsed:    " & Counter_Image (Clk.Elapsed));
      Ada.Text_IO.Put_Line ("  Latched:    " & Counter_Image (Clk.Latched));
   end Print;

   procedure Reset (Clk : in out Clock) is
   begin
      --  An actual reset of the GB would not result in the RTC actually
      --  resetting.
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
      Ada.Text_IO.Put_Line ("Latched");
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