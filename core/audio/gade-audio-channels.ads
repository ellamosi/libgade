with Ada.Unchecked_Conversion;
with Gade.Audio.Timers; use Gade.Audio.Timers;
with System;

package Gade.Audio.Channels is

   --  type Channel_Register is (NRx0, NRx1, NRx2, NRx3, NRx4);

   type Audio_Channel is abstract tagged private;

   procedure Reset (Ch : out Audio_Channel) is abstract;

   function Read
     (Channel  : Audio_Channel'Class;
      Register : Channel_Register)
      return Byte;

   procedure Write
     (Channel  : in out Audio_Channel'Class;
      Register : Channel_Register;
      Value    : Byte);

   procedure Step (Channel : in out Audio_Channel; S : out Sample) is abstract;

   procedure Length_Step (Channel : in out Audio_Channel) is abstract;

private

   --  Basically just more of an interface than anything
   type Audio_Channel is abstract tagged null record;

   Blank_Value : constant Byte := 16#FF#;

   procedure Trigger (Channel : in out Audio_Channel) is null;

   procedure Disable (Channel : in out Audio_Channel) is null;

   function Read_Blank (Channel : Audio_Channel) return Byte;

   function Read_NRx0 (Channel : Audio_Channel) return Byte renames Read_Blank;
   function Read_NRx1 (Channel : Audio_Channel) return Byte renames Read_Blank;
   function Read_NRx2 (Channel : Audio_Channel) return Byte renames Read_Blank;
   function Read_NRx3 (Channel : Audio_Channel) return Byte renames Read_Blank;
   function Read_NRx4 (Channel : Audio_Channel) return Byte renames Read_Blank;

   procedure Write_NRx0 (Channel : in out Audio_Channel; Value : Byte) is null;
   procedure Write_NRx1 (Channel : in out Audio_Channel; Value : Byte) is null;
   procedure Write_NRx2 (Channel : in out Audio_Channel; Value : Byte) is null;
   procedure Write_NRx3 (Channel : in out Audio_Channel; Value : Byte) is null;
   procedure Write_NRx4 (Channel : in out Audio_Channel; Value : Byte) is null;


   type Effect_Period_IO is mod 2 ** 3;

   Actual_Effect_Periods : constant array (Effect_Period_IO'Range)
     of Positive := (8, 1, 2, 3, 4, 5, 6, 7);


   generic
      Length_Bits : Positive;
   package Base is

      --  Adds length and trigger functionality
      type Base_Audio_Channel is abstract new Audio_Channel with private;

      overriding
      procedure Reset (Channel : out Base_Audio_Channel);

      overriding
      procedure Step (Channel : in out Base_Audio_Channel; S : out Sample);

      overriding
      procedure Trigger (Channel : in out Base_Audio_Channel) is null;

      overriding
      function Read_NRx4 (Channel : Base_Audio_Channel) return Byte;

      overriding
      procedure Write_NRx1 (Channel : in out Base_Audio_Channel; Value : Byte);

      overriding
      procedure Write_NRx4 (Channel : in out Base_Audio_Channel; Value : Byte);

      function Enabled (Channel : Base_Audio_Channel) return Boolean;

      overriding
      procedure Disable (Channel : in out Base_Audio_Channel);

      procedure Next_Sample_Level
        (Channel      : in out Base_Audio_Channel;
         Sample_Level : out Sample;
         Level_Cycles : out Positive) is abstract;

      overriding
      procedure Length_Step (Channel : in out Base_Audio_Channel);

   private

      Length_Max : constant Natural := 2 ** Length_Bits;
      NRx1_Length_Mask : constant Byte := Byte (Length_Max - 1);

      NRx4_Length_Enable_Mask : constant Byte := 16#BF#;

      type NRx4_Common_IO is record
         Trigger       : Boolean;
         Length_Enable : Boolean;
      end record;
      for NRx4_Common_IO use record
         Trigger       at 0 range 7 .. 7;
         Length_Enable at 0 range 6 .. 6;
      end record;
      for NRx4_Common_IO'Size use 8;

      function To_NRx4_Common_IO is new Ada.Unchecked_Conversion
        (Source => Byte,
         Target => NRx4_Common_IO);

      function To_Byte is new Ada.Unchecked_Conversion
        (Source => NRx4_Common_IO,
         Target => Byte);

      type Base_Audio_Channel is abstract new Audio_Channel with record
         Enabled      : Boolean; --  TODO: Not sure if really needed?
                                 --  Probably yes, as it can be enabled without using the length timer.
                                 --  Could use the Sample timer to derive it, though
         Length_Timer : Repeatable_Timer;
         Sample_Timer : Timer;
         Level        : Sample;
      end record;

      procedure Reload_Length (Channel : in out Base_Audio_Channel;
                               Length  : Natural);

   end Base;

   generic
      type Base_Channel is abstract new Audio_Channel with private;
   package Frequency_Mixin is

      Max_Period : constant := 2 ** 11;
      type Frequency_Type is mod Max_Period;

      --  TODO: Refactor sweep and make this private?
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

      type Channel_With_Frequency is abstract new Base_Channel with record
         Frequency_In : Frequency_IO;
      end record;

      procedure Set_Frequency
        (Channel : in out Channel_With_Frequency;
         Freq    : Frequency_Type) is abstract;

      overriding
      procedure Write_NRx3
        (Channel : in out Channel_With_Frequency;
         Value   : Byte);

      overriding
      procedure Write_NRx4
        (Channel : in out Channel_With_Frequency;
         Value   : Byte);

   end Frequency_Mixin;

   generic
      type Base_Channel is abstract new Audio_Channel with private;
   package Volume_Envelope_Mixin is

      type Envelope_Volume is mod 2 ** 4;
      type Envelope_Direction is (Down, Up);
      for Envelope_Direction use (Down => 0, Up => 1);
      subtype Envelope_Period is Effect_Period_IO;

      Volume_Max_Level : constant := Envelope_Volume'Last;
      Volume_Min_Level : constant := Envelope_Volume'First;


      type NRx2_Volume_Envelope_IO is record
         Volume    : Envelope_Volume;
         Direction : Envelope_Direction;
         Period    : Envelope_Period;
      end record;
      for NRx2_Volume_Envelope_IO use record
         Volume    at 0 range 4 .. 7;
         Direction at 0 range 3 .. 3;
         Period    at 0 range 0 .. 2;
      end record;
      for NRx2_Volume_Envelope_IO'Size use Byte'Size;

      function To_NRx2_Volume_Envelope_IO is new Ada.Unchecked_Conversion
        (Source => Byte,
         Target => NRx2_Volume_Envelope_IO);


      Steps : constant array (Envelope_Direction) of Integer := (-1, 1);

      type Volume_Envelope_Type is record
         Current_Volume : Natural; --  Keep track of volume for stepping
         Initial_Volume : Natural;
         Step           : Integer;
         Direction      : Envelope_Direction;
         Period         : Positive;
         Timer          : Repeatable_Timer;
      end record;

      function Enabled (Envelope : Volume_Envelope_Type) return Boolean;

      procedure Disable (Envelope : in out Volume_Envelope_Type);

      function Silent (Envelope : Volume_Envelope_Type) return Boolean;

      type Channel_With_Volume_Envelope is abstract new Base_Channel with record
         NRx2            : Byte;
         Volume_Envelope : Volume_Envelope_Type;
      end record;

      overriding
      procedure Trigger (Channel : in out Channel_With_Volume_Envelope);

      overriding
      procedure Disable (Channel : in out Channel_With_Volume_Envelope);

      procedure Set_Volume
        (Channel : in out Channel_With_Volume_Envelope;
         Volume  : Natural) is abstract;

      overriding
      function Read_NRx2 (Channel : Channel_With_Volume_Envelope) return Byte;

      overriding
      procedure Write_NRx2
        (Channel : in out Channel_With_Volume_Envelope;
         Value   : Byte);

   private

      procedure Volume_Envelope_Step
        (Channel : in out Channel_With_Volume_Envelope);

   end Volume_Envelope_Mixin;

end Gade.Audio.Channels;
