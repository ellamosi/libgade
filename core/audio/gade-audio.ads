with Gade.Audio_Buffer; use Gade.Audio_Buffer;

limited with Gade.Audio.Frame_Sequencer;

private package Gade.Audio is

   subtype Audio_IO_Address is Word range 16#FF10# .. 16#FF3F#;

   type Audio_Type is private;

   procedure Create
     (Audio : aliased out Audio_Type);

   procedure Reset
     (Audio : in out Audio_Type);

   procedure Read
     (Audio   : in out Audio_Type;
      Address : Audio_IO_Address;
      Value   : out Byte);

   procedure Write
     (Audio   : in out Audio_Type;
      Address : Audio_IO_Address;
      Value   : Byte);

   procedure Report_Cycles
     (Audio        : in out Audio_Type;
      Audio_Buffer : Audio_Buffer_Access;
      Cycles       : Positive);

   procedure Flush_Frame
     (Audio        : in out Audio_Type;
      Audio_Buffer : Audio_Buffer_Access;
      Cycles       : Positive);

private

   Blank_Value : constant Byte := 16#FF#;

   type Channel_Id is (NR1, NR2, NR3, NR4);
   Channel_Count : constant := Channel_Id'Range_Length;

   subtype NR1x_IO_Address is Audio_IO_Address range 16#FF10# .. 16#FF14#;
   subtype NR2x_IO_Address is Audio_IO_Address range 16#FF15# .. 16#FF19#;
   subtype NR3x_IO_Address is Audio_IO_Address range 16#FF1A# .. 16#FF1E#;
   subtype NR4x_IO_Address is Audio_IO_Address range 16#FF1F# .. 16#FF23#;

   subtype NR50_IO_Address is Audio_IO_Address range 16#FF24# .. 16#FF24#;
   subtype NR51_IO_Address is Audio_IO_Address range 16#FF25# .. 16#FF25#;
   subtype NR52_IO_Address is Audio_IO_Address range 16#FF26# .. 16#FF26#;

   subtype Wave_Table_IO_Address is Audio_IO_Address range 16#FF30# .. 16#FF3F#;


   type Audio_Access_Type is (Named, Address);


   type Channel_Flags is array (Channel_Id) of Boolean;
   pragma Pack (Channel_Flags);

   type Power_Control_Status (S : Audio_Access_Type := Named) is record
      case S is
         when Named =>
            Length_Status : Channel_Flags;
            Power         : Boolean;
         when Address =>
            Space         : Byte;
      end case;
   end record with Unchecked_Union;
   for Power_Control_Status use record
      Length_Status at 0 range 0 .. 3;
      Power         at 0 range 7 .. 7;
   end record;
   for Power_Control_Status'Size use Byte'Size;

   Power_Control_Status_Write_Mask : constant Byte := 16#70#;


   function Read_Power_Control_Status (Audio : Audio_Type) return Byte;

   procedure Write_Power_Control_Status (Audio : in out Audio_Type;
                                         Value : Byte);

   function Frame_Sequencer_State (Audio : Audio_Type)
                                   return Frame_Sequencer.State;

   function Is_Powered (Audio : Audio_Type) return Boolean;

   type Opaque_Audio_Type;
   type Audio_Type is access Opaque_Audio_Type;

end Gade.Audio;
