private with Ada.Calendar;
private with System;

package Gade.Carts.RTC is

   type Register is (Seconds, Minutes, Hours, Days_Low, Days_High);

   type Clock is private;

   type Clock_Access is access Clock;

   subtype Clock_NN_Access is not null Clock_Access;

   procedure Reset (Clk : in out Clock);

   procedure Read
     (Clk   : in out Clock;
      Reg   : Register;
      Value : out Byte);

   procedure Write
     (Clk   : in out Clock;
      Reg   : Register;
      Value : Byte);

   procedure Latch (Clk : in out Clock);

   procedure Report_Cycles
     (Clk    : in out Clock;
      Cycles : Positive);

private
   use Ada.Calendar;

   type Counter is record
      Span   : Duration;
      Halted : Boolean;
      Carry  : Boolean;
   end record;

   type Plain_Counter_Registers is array (Register'Range) of Byte;

   Day_Count_Cardinality : constant := 512;

   Cycles_Per_Second : constant := 4_190_000;

   type Day_Count is mod Day_Count_Cardinality;

   type Registers_Access_Type is (Named, Indexable);
   type Counter_Registers (Access_Type : Registers_Access_Type := Named)
   is record
      case Access_Type is
         when Named =>
            Seconds : Byte;
            Minutes : Byte;
            Hours   : Byte;
            Days    : Day_Count;
            Carry   : Boolean;
            Halted  : Boolean;
         when Indexable =>
            Indexed : Plain_Counter_Registers;
      end case;
   end record with Unchecked_Union;
   for Counter_Registers use record
      Seconds at 0 range 0 .. 7;
      Minutes at 1 range 0 .. 7;
      Hours   at 2 range 0 .. 7;
      Days    at 3 range 0 .. 8;
      Halted  at 4 range 6 .. 6;
      Carry   at 4 range 7 .. 7;
      Indexed at 0 range 0 .. 39;
   end record;
   for Counter_Registers'Size use 5 * 8;
   for Counter_Registers'Bit_Order use System.Low_Order_First;

   type Clock is record
      Elapsed   : Counter;
      Latched   : Counter;
      Registers : Counter_Registers;
      Cycles    : Natural;
   end record;

   Max_Span : constant Duration :=
     Duration (Day_Count_Cardinality * 24 * 60 * 60);

   Seconds_Day : constant Duration := Duration (24 * 60 * 60);

   function Days (D : Duration) return Natural;
   function Truncate (D : Duration) return Day_Duration;
   function Maximum (Left, Right : Duration) return Duration;

   function Duration_Image (D : Duration) return String;
   function Counter_Image (C : Counter) return String;
   procedure Print (Clk : Clock);

   procedure Increase_Counter
     (C    : in out Counter;
      Span : Duration);

   function Elapsed (Regs : Counter_Registers) return Duration;

   procedure To_Counter
     (C_Regs : Counter_Registers;
      C      : out Counter);
   procedure To_Registers
     (C      : Counter;
      C_Regs : out Counter_Registers);

end Gade.Carts.RTC;
