with Ada.Streams.Stream_IO;
private with Interfaces;
private with System;

package Gade.Carts.RTC.File_IO is

   procedure Load
     (Clk  : out Clock;
      File : Ada.Streams.Stream_IO.File_Type);

   procedure Save
     (Clk  : Clock;
      File : Ada.Streams.Stream_IO.File_Type);

private
   use Interfaces, Ada.Streams.Stream_IO;

   Days_Low_Cardinality : constant := 256;
   type Days_Top_Type is mod 2;

   Counter_Data_Size : constant := 20;

   type Counter_Data is record
      Seconds   : Unsigned_32;
      Minutes   : Unsigned_32;
      Hours     : Unsigned_32;
      Days_Low  : Unsigned_32;
      Days_Top  : Days_Top_Type;
      Carry     : Boolean;
      Halted    : Boolean;
   end record;
   for Counter_Data use record
      Seconds   at  0 range 0 .. 31;
      Minutes   at  4 range 0 .. 31;
      Hours     at  8 range 0 .. 31;
      Days_Low  at 12 range 0 .. 31;
      Days_Top  at 16 range 0 .. 0;
      Halted    at 16 range 6 .. 6;
      Carry     at 16 range 7 .. 7;
   end record;
   for Counter_Data'Size use Counter_Data_Size * 8;

   --  There are two versions for the VBA-M RTC save format, one using a 32 bit
   --  UNIX timestamp and another one using a 64 bit timestamp. The following
   --  declarations define a record type that can be used to represent either.
   --  This allows reading files in either format (but saving will always be
   --  done in the 64 bit timestamp format).
   --  The RTC data sizes for each type are 44 and 48 bytes respectively.
   Clock_Data_32_Size : constant := 2 * Counter_Data_Size + 4;
   Clock_Data_64_Size : constant := 2 * Counter_Data_Size + 8;

   type Clock_Data_32_Byte_Range is range 0 .. Clock_Data_32_Size - 1;
   type Clock_Data_64_Byte_Range is range 0 .. Clock_Data_64_Size - 1;

   type Clock_Data_32_Buffer is array (Clock_Data_32_Byte_Range) of Unsigned_8;
   type Clock_Data_64_Buffer is array (Clock_Data_64_Byte_Range) of Unsigned_8;

   type Clock_Data_Access_Type is (Named, Plain);
   type Timestamp_Size_Type is (TS_Size_32, TS_Size_64);

   --  Stream_IO would pack the records when reading/writing the file, so we
   --  enforce the rerpresentation clauses by treating it as just a byte array.
   type Clock_Data
     (Access_Type    : Clock_Data_Access_Type := Named;
      Timestamp_Size : Timestamp_Size_Type    := TS_Size_32)
   is record
      case Access_Type is
         when Named =>
            Time, Latched : Counter_Data;
            Saved_At      : Integer_64;
         when Plain =>
            case Timestamp_Size is
               when TS_Size_32 => Content_32 : Clock_Data_32_Buffer;
               when TS_Size_64 => Content_64 : Clock_Data_64_Buffer;
            end case;
      end case;
   end record with Unchecked_Union;
   for Clock_Data use record
      Time       at  0 range 0 .. 20 * 8 - 1;
      Latched    at 20 range 0 .. 20 * 8 - 1;
      Saved_At   at 40 range 0 .. 63;
      Content_32 at  0 range 0 .. Clock_Data_32_Size * 8 - 1;
      Content_64 at  0 range 0 .. Clock_Data_64_Size * 8 - 1;
   end record;
   for Clock_Data'Size use Clock_Data_64_Size * 8;
   for Clock_Data'Bit_Order use System.Low_Order_First;
   for Clock_Data'Scalar_Storage_Order use System.Low_Order_First;

   procedure Read (File : File_Type; Clk_Data : out Clock_Data);

   procedure To_Clock
     (Clk_Data  : Clock_Data;
      Loaded_At : Time;
      Clk       : out Clock);
   procedure To_Clock_Data
     (Clk      : Clock;
      Saved_At : Time;
      Clk_Data : out Clock_Data);

   procedure To_Counter_Data
     (Registers : Counter_Registers;
      C_Data    : out Counter_Data);
   procedure To_Counter_Data
     (C      : Counter;
      C_Data : out Counter_Data);

   procedure To_Counter_Registers
     (C_Data    : Counter_Data;
      Registers : out Counter_Registers);
   procedure To_Counter
     (C_Data : Counter_Data;
      C      : out Counter);

end Gade.Carts.RTC.File_IO;
