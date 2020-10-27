with Ada.Unchecked_Conversion;

with Gade.Audio.Timers; use Gade.Audio.Timers;
with System;

private package Gade.Audio.Channels is

   type Channel_Register is (NRx0, NRx1, NRx2, NRx3, NRx4);
   Channel_Register_Count : constant := Channel_Register'Range_Length;

   type Audio_Channel is abstract tagged private;

   procedure Create
     (Channel : out Audio_Channel;
      Audio   : Audio_Type);

   procedure Reset (Channel : in out Audio_Channel);

   function Read
     (Channel  : Audio_Channel'Class;
      Register : Channel_Register)
      return Byte;

   procedure Write
     (Channel  : in out Audio_Channel'Class;
      Register : Channel_Register;
      Value    : Byte);

   procedure Next_Sample
     (Channel : in out Audio_Channel;
      S       : out Sample);

   procedure Turn_Off (Channel : in out Audio_Channel);

   procedure Turn_On (Channel : in out Audio_Channel);

   function Enabled (Channel : Audio_Channel) return Boolean;

   procedure Tick_Length (Channel : in out Audio_Channel) is abstract;

   function Id (Channel : Audio_Channel) return Channel_Id is abstract;

   function Name (Channel : Audio_Channel'Class) return String;

private

   type Audio_Channel is abstract tagged record
      Audio        : Audio_Type;
      Powered      : Boolean;
      Level        : Sample;
      Sample_Timer : Timer_Type;
   end record;

   Blank_Value : constant Byte := 16#FF#;

   type Disable_Mode is (APU_Power_Off, DAC_Power_Off, Self_Disable);

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

   procedure Trigger (Channel : in out Audio_Channel);

   procedure Disable
     (Channel : in out Audio_Channel;
      Mode    : Disable_Mode);

   procedure Step_Sample (Channel : in out Audio_Channel);

   procedure Next_Sample_Level
     (Channel      : in out Audio_Channel;
      Sample_Level : out Sample;
      Level_Cycles : out Positive) is null;


   type Effect_Period_IO is mod 2 ** 3;

   Actual_Effect_Periods : constant array (Effect_Period_IO'Range)
     of Positive := (8, 1, 2, 3, 4, 5, 6, 7);

   generic
      Length_Bits : Positive;
   package Length_Trigger is

      type Length_Trigger_Channel is abstract new Audio_Channel with private;

      overriding
      procedure Turn_On (Channel : in out Length_Trigger_Channel);

      overriding
      procedure Disable
        (Channel : in out Length_Trigger_Channel;
         Mode    : Disable_Mode);

      function Can_Enable (Channel : Length_Trigger_Channel)
                           return Boolean is abstract;

      overriding
      function Read_NRx4 (Channel : Length_Trigger_Channel) return Byte;

      overriding
      procedure Write_NRx1
        (Channel : in out Length_Trigger_Channel;
         Value   : Byte);

      overriding
      procedure Write_NRx4
        (Channel : in out Length_Trigger_Channel;
         Value   : Byte);

      overriding
      procedure Tick_Length (Channel : in out Length_Trigger_Channel);

   private

      Length_Max : constant Natural := 2 ** Length_Bits;

      subtype Lengh_Steps is Frame_Sequencer_Step range
        Length_Counter .. Length_Counter_Frequency_Sweep;

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

      subtype Parent is Audio_Channel;
      type Length_Trigger_Channel is abstract new Parent with record
         Length_Timer   : Timer_Type;
         Length_Enabled : Boolean;
         Length         : Natural;
      end record;

      procedure Reload_Length
        (Channel : in out Length_Trigger_Channel;
         Length  : Natural);

      procedure Length_Triggered_Disable
        (Channel : in out Length_Trigger_Channel);

   end Length_Trigger;

   generic
      type Base_Channel is abstract new Audio_Channel with private;
   package Frequency_Mixin is

      Max_Period : constant := 2 ** 11;
      type Frequency_Type is mod Max_Period;
      Max_Frequency : constant := Frequency_Type'Last;

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

      subtype Parent is Base_Channel;
      type Channel_With_Frequency is abstract new Parent with record
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

      overriding
      procedure Disable
        (Channel : in out Channel_With_Frequency;
         Mode    : Disable_Mode);

   end Frequency_Mixin;

end Gade.Audio.Channels;
