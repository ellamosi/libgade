with System;

package Gade.Audio.Channels.Pulse.Square is

   type Square_Channel is new Pulse_Channel with private;

   overriding
   procedure Reset (Channel : out Square_Channel);

private

   Next_Pulse_State : constant array (Pulse_State_Type) of Pulse_State_Type :=
     (Pulse_High, Pulse_Low);

   type Duty_Type is (Eighth, Quarter, Half, Three_Quarters);
   for Duty_Type use
     (Eighth         => 2#00#,
      Quarter        => 2#01#,
      Half           => 2#10#,
      Three_Quarters => 2#11#);

   type Pulse_Cycles_Type is array (Pulse_State_Type) of Positive;

   Hi_Duty_Sample_Multiplier : constant array (Duty_Type) of Natural :=
     (Eighth         => 1,
      Quarter        => 2,
      Half           => 4,
      Three_Quarters => 6);
   Lo_Duty_Sample_Multiplier : constant array (Duty_Type) of Natural :=
     (Eighth         => 7,
      Quarter        => 6,
      Half           => 4,
      Three_Quarters => 2);

   NRx1_Duty_Mask : constant Byte := 16#3F#;
   NRx1_Duty_Div : constant Byte := 2 ** 6;

   Max_Period : constant := 2 ** 11;
   type Frequency_Type is mod Max_Period;

   type Frequency_IO (Access_Type : Audio_Access_Type := Named) is record
      case Access_Type is
         when Named =>
            Frequency : Frequency_Type;
         when Address =>
            NRx3, NRx4 : Byte;
      end case;
   end record with Unchecked_Union;
   for Frequency_IO use record
      Frequency at 0 range 0 .. 10;
      NRx3      at 0 range 0 .. 7;
      NRx4      at 1 range 0 .. 7;
   end record;
   for Frequency_IO'Scalar_Storage_Order use System.Low_Order_First;
   for Frequency_IO'Size use Byte'Size * 2;


   type Square_Channel is new Pulse_Channel with record
      Pulse_Cycles : Pulse_Cycles_Type;
      Pulse_State  : Pulse_State_Type;
      Frequency_In : Frequency_IO;
      Duty         : Duty_Type;
      NRx1         : Byte;
   end record;

   overriding
   procedure Next_Sample_Level
     (Channel      : in out Square_Channel;
      Sample_Level : out Sample;
      Level_Cycles : out Positive);

   overriding
   function Read_NRx1 (Channel : Square_Channel) return Byte;

   overriding
   procedure Write_NRx1 (Channel : in out Square_Channel; Value : Byte);

   overriding
   procedure Write_NRx3 (Channel : in out Square_Channel; Value : Byte);

   overriding
   procedure Write_NRx4 (Channel : in out Square_Channel; Value : Byte);

   procedure Set_Frequency
     (Channel : in out Square_Channel;
      Freq    : Frequency_Type);

end Gade.Audio.Channels.Pulse.Square;
