with Ada.Calendar.Conversions;   use Ada.Calendar.Conversions;
with Ada.Exceptions;             use Ada.Exceptions;
with Ada.Text_IO;
with Interfaces.C;

package body Gade.Carts.RTC.File_IO is

   procedure To_Counter_Registers
     (C_Data    : Counter_Data;
      Registers : out Counter_Registers)
   is
      pragma Unsuppress (Overflow_Check);

      Days_Top : constant Unsigned_32 := Unsigned_32 (C_Data.Days_Top);
      Days : constant Unsigned_32 :=
        C_Data.Days_Low + Days_Top * Days_Low_Cardinality;
   begin
      Ada.Text_IO.Put_Line ("To_Counter_Registers.SS" & C_Data.Seconds'Img);
      Ada.Text_IO.Put_Line ("To_Counter_Registers.MM" & C_Data.Minutes'Img);
      Ada.Text_IO.Put_Line ("To_Counter_Registers.HH" & C_Data.Hours'Img);
      Registers.Seconds := Byte (C_Data.Seconds);
      Registers.Minutes := Byte (C_Data.Minutes);
      Registers.Hours   := Byte (C_Data.Hours);

      Registers.Indexed (Days_High) := 0;
      Registers.Days   := Day_Count (Days);
      Registers.Halted := C_Data.Halted;
      Registers.Carry  := C_Data.Carry;
   end To_Counter_Registers;

   procedure To_Counter
     (C_Data : Counter_Data;
      C      : out Counter)
   is
      Registers : Counter_Registers;
   begin
      To_Counter_Registers (C_Data, Registers);
      To_Counter (Registers, C);
   end To_Counter;

   procedure To_Counter_Data
     (Registers : Counter_Registers;
      C_Data    : out Counter_Data)
   is
   begin
      C_Data.Seconds  := Unsigned_32 (Registers.Seconds);
      C_Data.Minutes  := Unsigned_32 (Registers.Minutes);
      C_Data.Hours    := Unsigned_32 (Registers.Hours);
      C_Data.Days_Low := Unsigned_32 (Registers.Days mod Days_Low_Cardinality);
      C_Data.Days_Top := Days_Top_Type (Registers.Days / Days_Low_Cardinality);
      C_Data.Halted   := Registers.Halted;
      C_Data.Carry    := Registers.Carry;
   end To_Counter_Data;

   procedure To_Counter_Data
     (C      : Counter;
      C_Data : out Counter_Data)
   is
      Registers : Counter_Registers;
   begin
      To_Registers (C, Registers);
      To_Counter_Data (Registers, C_Data);
   end To_Counter_Data;

   procedure To_Clock
     (Clk_Data  : Clock_Data;
      Loaded_At : Time;
      Clk       : out Clock)
   is
      Saved_At : Time;
      Elapsed  : Duration;
   begin
      To_Counter (Clk_Data.Time, Clk.Elapsed);
      To_Counter (Clk_Data.Latched, Clk.Latched);
      if not Clk.Elapsed.Halted then
         Saved_At := To_Ada_Time (Interfaces.C.long (Clk_Data.Saved_At));
         --  Ignore timestamps in the future by only allowing positive spans
         Elapsed := Duration'Max (Loaded_At - Saved_At, 0.0);
         Increase_Counter (Clk.Elapsed, Elapsed);
      end if;
   end To_Clock;

   procedure To_Clock_Data
     (Clk      : Clock;
      Saved_At : Time;
      Clk_Data : out Clock_Data)
   is
   begin
      Clk_Data.Content_64 := (others => 0);
      To_Counter_Data (Clk.Elapsed, Clk_Data.Time);
      To_Counter_Data (Clk.Latched, Clk_Data.Latched);
      Clk_Data.Saved_At := Integer_64 (To_Unix_Time (Saved_At));
   end To_Clock_Data;

   procedure Read (File : File_Type; Clk_Data : out Clock_Data) is
      Input_Stream : Stream_Access;
      File_Size    : Count;
      Tail_Size    : Count;
   begin
      File_Size := Size (File);
      Tail_Size := File_Size mod RAM_Bank_Size;
      Input_Stream := Ada.Streams.Stream_IO.Stream (File);
      Clk_Data.Saved_At := 0;
      if Tail_Size >= Clock_Data_64_Size then
         Clock_Data_64_Buffer'Read (Input_Stream, Clk_Data.Content_64);
      else
         Clock_Data_32_Buffer'Read (Input_Stream, Clk_Data.Content_32);
      end if;
   end Read;

   procedure Load
     (Clk  : out Clock;
      File : Ada.Streams.Stream_IO.File_Type)
   is
      Clk_Data : Clock_Data;
   begin
      Read (File, Clk_Data);
      To_Clock (Clk_Data, Ada.Calendar.Clock, Clk);
      Ada.Text_IO.Put_Line ("Loaded RTC:");
      Print (Clk);
   exception
      when Error : others =>
         Ada.Text_IO.Put_Line ("Unable to read RTC file");
         Ada.Text_IO.Put_Line (Exception_Information (Error));
   end Load;

   procedure Save
     (Clk  : Clock;
      File : Ada.Streams.Stream_IO.File_Type)
   is
      Output_Stream : Stream_Access;
      Clk_Data      : Clock_Data;
   begin
      Ada.Text_IO.Put_Line ("Saving RTC:");
      Print (Clk);
      To_Clock_Data (Clk, Ada.Calendar.Clock, Clk_Data);
      Output_Stream := Ada.Streams.Stream_IO.Stream (File);
      Clock_Data_64_Buffer'Write (Output_Stream, Clk_Data.Content_64);
   exception
      when Error : others =>
         Ada.Text_IO.Put_Line ("Unable to write RTC file");
         Ada.Text_IO.Put_Line (Exception_Information (Error));
   end Save;

end Gade.Carts.RTC.File_IO;
