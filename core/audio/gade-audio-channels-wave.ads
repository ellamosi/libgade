with System;

package Gade.Audio.Channels.Wave is

   type Wave_Sample is mod 2 ** 4;

   type Wave_Sample_Index is mod 2 ** 5;

   type Wave_Table is array (Wave_Sample_Index'Range) of Wave_Sample;
   pragma Pack (Wave_Table);
   for Wave_Table'Scalar_Storage_Order use System.High_Order_First;
   --  Bytes contain the first sample in the higher bits, last sample in the
   --  low bits

   subtype Wave_Table_IO_Range is Word range 16#FF30# .. 16#FF3F#;

   type Wave_Table_Bytes is array (Wave_Table_IO_Range) of Byte;

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

   type Wave_Table_IO_Access is access all Wave_Table_IO;


   type Wave_Channel is new Audio_Channel with private;

   --  TODO: Find a better way to assign the table
   procedure Set_Table
     (Channel : in out Wave_Channel;
      Table   : Wave_Table_IO_Access);

   overriding
   procedure Reset (Channel : out Wave_Channel);

private

   Length_Bit_Size : constant := 8;


   --  TODO: Consider a way to compose frequency definitions/set_frequency with
   --  square channel. NRx3/NRx4 read/writes too.
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


   NRx2_Volume_Mask : constant Byte := 16#9F#;

   type Volume_Type is (None, Full, Half, Quarter);
   for Volume_Type use
     (None    => 2#00#,
      Full    => 2#01#,
      Half    => 2#10#,
      Quarter => 2#11#);

   Volume_Shifts : constant array (Volume_Type'Range) of Natural :=
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


   package Base is new Channels.Base (Length_Bit_Size);

   type Wave_Channel is new Base.Base_Audio_Channel with record
      NRx0, NRx2   : Byte;
      Powered      : Boolean;
      Volume_Shift : Natural;
      Frequency_In : Frequency_IO;
      Table        : Wave_Table_IO_Access;
      Sample_Time  : Positive;
      Sample_Index : Wave_Sample_Index;
   end record;

   overriding
   procedure Next_Sample_Level
     (Channel      : in out Wave_Channel;
      Sample_Level : out Sample;
      Level_Cycles : out Positive);

   overriding
   procedure Trigger (Channel : in out Wave_Channel);

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

   overriding
   procedure Write_NRx3 (Channel : in out Wave_Channel; Value : Byte);

   overriding
   procedure Write_NRx4 (Channel : in out Wave_Channel; Value : Byte);

end Gade.Audio.Channels.Wave;
