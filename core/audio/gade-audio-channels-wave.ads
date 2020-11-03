with System;

package Gade.Audio.Channels.Wave is

   subtype Wave_Table_IO_Address is Audio_IO_Address range 16#FF30# .. 16#FF3F#;

   type Wave_Channel is new Audio_Channel with private;

   overriding
   procedure Reset (Channel : in out Wave_Channel);

   function Read_Table
     (Channel : Wave_Channel;
      Address : Wave_Table_IO_Address) return Byte;

   procedure Write_Table
     (Channel : in out Wave_Channel;
      Address : Wave_Table_IO_Address;
      Value   : Byte);

   overriding
   function Id (Channel : Wave_Channel) return Channel_Id;

private

   type Wave_Sample is mod 2 ** 4;
   type Wave_Sample_Index is mod 2 ** 5;

   type Wave_Table is array (Wave_Sample_Index) of Wave_Sample;
   pragma Pack (Wave_Table);
   for Wave_Table'Scalar_Storage_Order use System.High_Order_First;
   --  Bytes contain the first sample in the higher bits, last sample in the
   --  low bits

   type Wave_Table_Bytes is array (Wave_Table_IO_Address) of Byte;

   --  TODO: Have a shared discriminant type
   type Table_Access_Type is (Named, Address);

   type Wave_Table_IO (S : Table_Access_Type := Address) is record
      case S is
         when Named =>
            Table : Wave_Table;
         when Address =>
            Space : Wave_Table_Bytes;
      end case;
   end record with Unchecked_Union;
   for Wave_Table_IO use record
      Table at 0 range 0 .. 16 * Byte'Size - 1;
      Space at 0 range 0 .. 16 * Byte'Size - 1;
   end record;
   for Wave_Table_IO'Size use 16 * Byte'Size;

   Initial_DMG_Table : Wave_Table_Bytes :=
     (16#84#, 16#40#, 16#43#, 16#AA#, 16#2D#, 16#78#, 16#92#, 16#3C#,
      16#60#, 16#59#, 16#59#, 16#B0#, 16#34#, 16#B8#, 16#2E#, 16#DA#);


   NRx2_Volume_Mask : constant Byte := 16#9F#;

   type Volume_Type is (None, Full, Half, Quarter);
   for Volume_Type use
     (None    => 2#00#,
      Full    => 2#01#,
      Half    => 2#10#,
      Quarter => 2#11#);

   Volume_Shifts : constant array (Volume_Type) of Natural :=
     (None    => 4,
      Full    => 0,
      Half    => 1,
      Quarter => 2);

   type NRx2_Volume_IO is record
      Volume : Volume_Type;
   end record;
   for NRx2_Volume_IO use record
      Volume at 0 range 5 .. 6;
   end record;
   for NRx2_Volume_IO'Size use Byte'Size;

   function To_NRx2_Volume_IO is new Ada.Unchecked_Conversion
     (Source => Byte,
      Target => NRx2_Volume_IO);


   NRx0_Power_Mask : constant Byte := 16#7F#;

   type NRx0_Power_IO is record
      Powered : Boolean;
   end record;
   for NRx0_Power_IO use record
      Powered at 0 range 7 .. 7;
   end record;
   for NRx0_Power_IO'Size use Byte'Size;

   function To_NRx0_Power_IO is new Ada.Unchecked_Conversion
     (Source => Byte,
      Target => NRx0_Power_IO);


   Length_Bit_Size : constant := 8;

   package Wave_Length_Trigger is new Length_Trigger (Length_Bit_Size);
   use Wave_Length_Trigger;

   package Wave_Frequency_Mixin is new Frequency_Mixin (Length_Trigger_Channel);
   use Wave_Frequency_Mixin;

   subtype Parent is Channel_With_Frequency;
   type Wave_Channel is new Parent with record
      NRx0, NRx2   : Byte;
      Powered      : Boolean;
      Volume_Shift : Natural;
      Table_IO     : Wave_Table_IO;
      Sample_Time  : Positive;
      Sample_Index : Wave_Sample_Index;
      Sample_Diff  : Sample;
   end record;

   overriding
   procedure Next_Sample_Level
     (Channel      : in out Wave_Channel;
      Sample_Level : out Sample;
      Level_Cycles : out Positive);

   overriding
   procedure Disable (Channel : in out Wave_Channel;
                      Mode    : Disable_Mode);

   overriding
   procedure Trigger (Channel : in out Wave_Channel);

   overriding
   procedure Set_Frequency
     (Channel : in out Wave_Channel;
      Freq    : Frequency_Type);

   overriding
   function Read_NRx0 (Channel : Wave_Channel) return Byte;

   overriding
   function Read_NRx2 (Channel : Wave_Channel) return Byte;

   overriding
   procedure Write_NRx0 (Channel : in out Wave_Channel; Value : Byte);

   overriding
   procedure Write_NRx2 (Channel : in out Wave_Channel; Value : Byte);

end Gade.Audio.Channels.Wave;
