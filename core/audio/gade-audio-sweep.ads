with System;
with Gade.Audio.Square; use Gade.Audio.Square;

private package Gade.Audio.Sweep is

   type Sweep_Channel is new Square_Channel with private;

   overriding
   procedure Extended_Reset (Ch : in out Sweep_Channel);

   overriding
   procedure NRx0_Read
     (Ch    : in out Sweep_Channel;
      Value : out Byte);

   overriding
   procedure NRx0_Write
     (Ch    : in out Sweep_Channel;
      Value : Byte);

   overriding
   procedure Extended_Trigger
     (Ch        : in out Sweep_Channel;
      Frequency : Frequency_Type);

   procedure Frequency_Sweep_Step (Ch : in out Sweep_Channel);

private

   type Sweep_Period_Type is mod 2 ** 3;
   type Sweep_Shift_Type is mod 2 ** 3;

   Sweep_IO_Mask : constant Byte := 16#80#; --  Most significant bit is unused

   --  Just a single registar at NRx0 is used for frequency sweep control.
   --  A single bit is left unused.
   type Sweep_IO (Access_Type : Audio_Access_Type := Named) is record
      case Access_Type is
         when Named =>
            Period : Sweep_Period_Type;
            Negate : Boolean;
            Shift  : Sweep_Shift_Type;
         when Address =>
            Space  : Byte;
      end case;
   end record with Unchecked_Union;
   for Sweep_IO use record
      Period at 0 range 4 .. 6;
      Negate at 0 range 3 .. 3;
      Shift  at 0 range 0 .. 2;
      Space  at 0 range 0 .. 7;
   end record;
   for Sweep_IO'Scalar_Storage_Order use System.Low_Order_First;
   for Sweep_IO'Size use 8;

   type Sweep_Channel is new Square_Channel with record
      --  TODO: Revise names, sweep_ suffixing
      IO               : Sweep_IO;
      Sweep_Enabled    : Boolean;
      Shadow_Frequency : Frequency_Type;
      Sweep_Timer      : Timer;
      Sweep_Shift      : Sweep_Shift_Type;
      Sweep_Negate     : Boolean;
   end record;

   procedure Trigger_Frequency_Sweep
     (Ch        : in out Sweep_Channel;
      Frequency : Frequency_Type;
      Period    : Sweep_Period_Type;
      Negate    : Boolean;
      Shift     : Sweep_Shift_Type);
   procedure Frequency_Sweep_Shift (Ch : in out Sweep_Channel);



--     type A is tagged record
--        IO : Shared (A_IO);
--        BIO : Shared (B_IO) renames IO;
--     end record;




end Gade.Audio.Sweep;
