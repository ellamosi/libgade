with System;

private package Gade.Audio.Channels is

   type Audio_Channel is abstract tagged private;

private

   type Frequency_Type is mod 2 ** 11;

   type Channel_Volume_Type is mod 2 ** 4;
   type Sweep_Period_Type is mod 2 ** 3;
   type Sweep_Shift_Type is mod 2 ** 3;

   type Duty_Type is (Eighth, Quarter, Half, Three_Quarters);
   for Duty_Type use
     (Eighth         => 2#00#,
      Quarter        => 2#01#,
      Half           => 2#10#,
      Three_Quarters => 2#11#);
   type Length_Load_Type is mod 2 ** 6;

   type Envelope_Direction_Type is (Down, Up);
   for Envelope_Direction_Type use (Down => 0, Up => 1);
   type Period_Type is mod 2 ** 3;

   type Channel_IO_Space is array (Channel_Register'Range) of Byte;
   type Channel_IO (Access_Type : Audio_Access_Type := Named) is record
      case Access_Type is
         when Named =>
            --  NRx0 (unused on SQ2)
            Sweep_Period       : Sweep_Period_Type;
            Negate             : Boolean;
            Shift              : Sweep_Shift_Type;
            --  NRx1
            Duty               : Duty_Type;
            Length_Load        : Length_Load_Type;
            --  NRx2
            Volume             : Channel_Volume_Type;
            Envelope_Direction : Envelope_Direction_Type;
            Period             : Period_Type;
            --  NRx3 + NRx4
            Frequency          : Frequency_Type;
            Trigger            : Boolean;
            Length_Enable      : Boolean;
         when Address =>
            Space              : Channel_IO_Space;
      end case;
   end record with Unchecked_Union;
   for Channel_IO use record
      Sweep_Period       at 0 range 4 .. 6;
      Negate             at 0 range 3 .. 3;
      Shift              at 0 range 0 .. 2;
      Duty               at 1 range 6 .. 7;
      Length_Load        at 1 range 0 .. 5;
      Volume             at 2 range 4 .. 7;
      Envelope_Direction at 2 range 3 .. 3;
      Period             at 2 range 0 .. 2;
      Frequency          at 3 range 0 .. 10;
      Trigger            at 4 range 7 .. 7;
      Length_Enable      at 4 range 6 .. 6;
   end record;
   for Channel_IO'Scalar_Storage_Order use System.Low_Order_First;
   for Channel_IO'Size use 40;

   type Square_Channel is abstract tagged record
      IO : Channel_IO;
   end record;

end Gade.Audio.Channels;
