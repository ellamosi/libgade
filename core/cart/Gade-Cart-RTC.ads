private with Ada.Calendar;
private with Ada.Streams.Stream_IO;
private with System;

package Gade.Cart.RTC is

   type Register is (Seconds, Minutes, Hours, Days_Low, Days_High);

   type Clock is private;

   procedure Reset (Clk : out Clock);

   --  procedure Refresh (Clk : in out Clock);

--     procedure Read
--       (Clk : Clock;
--        Seconds, Minutes, Hours, Days_Low, Days_High : out Byte);
--

   procedure Read
     (Clk   : in out Clock;
      Reg   : Register;
      Value : out Byte);

--     procedure Write
--       (Clk   : in out Clock;
--        Reg   : Register;
--        Value : Byte);
--
   procedure Latch (Clk : in out Clock);

   procedure Load
     (Path : String;
      Clk  : out Clock);

   procedure Save
     (Path : String;
      Clk  : Clock);

private
   use Ada.Calendar;
   use Ada.Streams.Stream_IO;

   type Unsigned_8 is mod 2 ** 8;
   type Unsigned_32 is mod 2 ** 32;

   type Days_Top_Type is mod 2;

   type File_Time_Info is record
      Seconds   : Unsigned_32;
      Minutes   : Unsigned_32;
      Hours     : Unsigned_32;
      Days_Low  : Unsigned_32;
      Days_Top  : Days_Top_Type;
      Carry     : Boolean;
      Halted    : Boolean;
   end record;
   for File_Time_Info use record
      Seconds   at  0 range 0 .. 31;
      Minutes   at  4 range 0 .. 31;
      Hours     at  8 range 0 .. 31;
      Days_Low  at 12 range 0 .. 31;
      Days_Top  at 16 range 0 .. 0;
      Halted    at 16 range 6 .. 6;
      Carry     at 16 range 7 .. 7;
   end record;

   type Timestamp_32 is mod 2 ** 32;
   type Timestamp_64 is mod 2 ** 64;

   File_Data_32_Size : constant := 44; -- Bytes
   File_Data_64_Size : constant := 48;

   type File_Data_Buffer_32 is array (0 .. File_Data_32_Size - 1) of Unsigned_8;
   type File_Data_Buffer_64 is array (0 .. File_Data_64_Size - 1) of Unsigned_8;

   type File_Data_Access_Type is (Named, Raw);
   type Timestamp_Size_Type is (TS_Size_32, TS_Size_64);

   --  Stream_IO would pack the records when reading/writing the file, so to
   --  enforce the rerpresentation clauses by treating it as just a byte array.
   type File_Data
     (Access_Type    : File_Data_Access_Type := Named;
      Timestamp_Size : Timestamp_Size_Type   := TS_Size_32)
   is record
      case Access_Type is
         when Named =>
            Time     : File_Time_Info;
            Latched  : File_Time_Info;
            Saved_At : Timestamp_64;
         when Raw =>
            case Timestamp_Size is
               when TS_Size_32 => Content_32 : File_Data_Buffer_32;
               when TS_Size_64 => Content_64 : File_Data_Buffer_64;
            end case;
      end case;
   end record with Unchecked_Union;
   for File_Data use record
      Time        at  0 range 0 .. 20 * 8 - 1;
      Latched     at 20 range 0 .. 20 * 8 - 1;
      Saved_At    at 40 range 0 .. 63;
      Content_32  at  0 range 0 .. File_Data_32_Size * 8 - 1;
      Content_64  at  0 range 0 .. File_Data_64_Size * 8 - 1;
   end record;
   for File_Data'Size use File_Data_64_Size * 8; -- 20 + 20 + 8 Bytes
   for File_Data'Bit_Order use System.Low_Order_First;

   type Counter is record
      Span      : Duration;
      Halted    : Boolean;
      Overflown : Boolean;
   end record;

   type Register_Values is array (Register'Range) of Byte;

   type Clock is record
      Elapsed    : Counter;
      Latched    : Counter;
      Registers  : Register_Values;
      Updated_At : Time;
   end record;

   Max_Span : constant := 512 * 24 * 60 * 60;

   Seconds_Day : constant Duration := Duration (24 * 60 * 60);

   procedure Read (File : File_Type; RTC_Data : out File_Data);
   procedure Sanitize (FD : in out File_Data);

   function To_Clock (FD : File_Data) return Clock;
   function To_File_Data (Clk : Clock) return File_Data;

   function Elapsed (Time : File_Time_Info) return Duration;
   function To_Counter (Time : File_Time_Info) return Counter;
   function To_File_Time (C : Counter) return File_Time_Info;

   function Days (D : Duration) return Natural;
   function Truncate (D : Duration) return Day_Duration;

   function Maximum (Left, Right : Duration) return Duration;

   function Duration_Image (D : Duration) return String;
   function Counter_Image (C : Counter) return String;

   procedure Refresh (Clk : in out Clock);

   procedure Materialize_Registers
     (Count : Counter;
      Regs  : out Register_Values);

end Gade.Cart.RTC;

