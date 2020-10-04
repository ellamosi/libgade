with Ada.Unchecked_Conversion;
with Gade.Audio.Timers; use Gade.Audio.Timers;

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

   generic
      Length_Bits : Positive;
   package Base is

      --  Adds length and trigger functionality
      type Base_Audio_Channel is abstract new Audio_Channel with private;

      overriding
      procedure Reset (Channel : out Base_Audio_Channel);

      overriding
      procedure Step (Channel : in out Base_Audio_Channel; S : out Sample);

      procedure Trigger (Channel : in out Base_Audio_Channel) is null;

      overriding
      function Read_NRx4 (Channel : Base_Audio_Channel) return Byte;

      overriding
      procedure Write_NRx1 (Channel : in out Base_Audio_Channel; Value : Byte);

      overriding
      procedure Write_NRx4 (Channel : in out Base_Audio_Channel; Value : Byte);

      function Enabled (Channel : Base_Audio_Channel) return Boolean;

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

   type Effect_Period_IO is mod 2 ** 3;

   Actual_Effect_Periods : constant array (Effect_Period_IO'Range) of Positive
     := (8, 1, 2, 3, 4, 5, 6, 7);

end Gade.Audio.Channels;
