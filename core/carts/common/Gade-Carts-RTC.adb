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
      Ada.Text_IO.Put_Line ("  Updated_At: " & Image (Clk.Updated_At));
   end Print;

   procedure Reset (Clk : in out Clock) is
   begin
      --  An actual reset of the GB would not result in the RTC actually
      --  resetting. Might need to add some logic to handle syncing the state
      --  with the rest of the system later on though.
      null;
   end Reset;

   procedure Refresh (Clk : in out Clock) is
      Now : constant Time := Ada.Calendar.Clock;
   begin
      if not Clk.Elapsed.Halted then
         Clk.Elapsed.Span := Now - Clk.Updated_At + Clk.Elapsed.Span;
         if Clk.Elapsed.Span >= Max_Span then
            Clk.Elapsed.Carry := True;
            Clk.Elapsed.Span := Clk.Elapsed.Span - Max_Span;
         end if;
      end if;
      Clk.Updated_At := Now;
   end Refresh;

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
      Refresh (Clk);
      To_Registers (Clk.Elapsed, Regs);
      Regs.Indexed (Reg) := Value;
      To_Counter (Regs, Clk.Elapsed);
   end Write;

   procedure Latch (Clk : in out Clock) is
   begin
      Ada.Text_IO.Put_Line ("Latched");
      Refresh (Clk);
      Clk.Latched := Clk.Elapsed;
      To_Registers (Clk.Latched, Clk.Registers);
   end Latch;

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
