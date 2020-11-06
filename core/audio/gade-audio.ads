with Gade.Audio_Buffer; use Gade.Audio_Buffer;

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

   subtype NR1_IO_Address is Audio_IO_Address range 16#FF10# .. 16#FF14#;
   subtype NR2_IO_Address is Audio_IO_Address range 16#FF15# .. 16#FF19#;
   subtype NR3_IO_Address is Audio_IO_Address range 16#FF1A# .. 16#FF1E#;
   subtype NR4_IO_Address is Audio_IO_Address range 16#FF1F# .. 16#FF23#;

   subtype Output_Volume_Control_IO_Address is Audio_IO_Address
   range 16#FF24# .. 16#FF24#;

   subtype Channel_Output_Control_IO_Address is Audio_IO_Address
   range 16#FF25# .. 16#FF25#;

   subtype Power_Control_Status_IO_Address is Audio_IO_Address
   range 16#FF26# .. 16#FF26#;

--     subtype Control_Address is Audio_IO_Address range 16#FF24# .. 16#FF26#;

   type Audio_Access_Type is (Named, Address);


   type Output_Volume is mod 2 ** 3;

   type Output_Volume_Control (S : Audio_Access_Type := Named) is record
      case S is
         when Named =>
            Right_Volume     : Output_Volume;
            Vin_Right_Enable : Boolean;
            Left_Volume      : Output_Volume;
            Vin_Left_Enable  : Boolean;
         when Address =>
            Space            : Byte;
      end case;
   end record with Unchecked_Union;
   for Output_Volume_Control use record
      Right_Volume     at 0 range 0 .. 2;
      Vin_Right_Enable at 0 range 3 .. 3;
      Left_Volume      at 0 range 4 .. 6;
      Vin_Left_Enable  at 0 range 7 .. 7;
      Space            at 0 range 0 .. 7;
   end record;
   for Output_Volume_Control'Size use Byte'Size;


   type Channel_Samples is array (Channel_Id) of Sample;

   type Channel_Flags is array (Channel_Id) of Boolean;
   pragma Pack (Channel_Flags);

   type Channel_Output_Control (S : Audio_Access_Type := Named) is record
      case S is
         when Named =>
            Right, Left : Channel_Flags;
         when Address =>
            Space       : Byte;
      end case;
   end record with Unchecked_Union;
   for Channel_Output_Control use record
      Right at 0 range 0 .. 3;
      Left  at 0 range 4 .. 7;
      Space at 0 range 0 .. 7;
   end record;
   for Channel_Output_Control'Size use Byte'Size;


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


   --  The Frame sequencer triggers the volume envelope tick at 64 Hz
   --  So we need to clock it a t 8 times that so it can complete its state
   --  cycle at that rate.
   Frame_Sequencer_Freq : constant := 512; -- Hz
   Samples_Frame_Sequencer_Tick : constant := Samples_Second / Frame_Sequencer_Freq;

--     type Frame_Sequencer_Trigger is
--       (Length_Counter, Frequency_Sweep, Volume_Envelope);

   type Frame_Sequencer_Step is
     (None, Volume_Envelope, Length_Counter, Length_Counter_Frequency_Sweep);

   type Frame_Sequencer_Step_Index is mod 8;

   Frame_Sequencer_Steps : constant array (Frame_Sequencer_Step_Index) of
     Frame_Sequencer_Step :=
       (Length_Counter,
        None,
        Length_Counter_Frequency_Sweep,
        None,
        Length_Counter,
        None,
        Length_Counter_Frequency_Sweep,
        Volume_Envelope);

   function Current_Frame_Sequencer_Step
     (Audio : Audio_Type)
      return Frame_Sequencer_Step;

   function Is_Powered (Audio : Audio_Type) return Boolean;

   procedure Tick_Frame_Sequencer (Audio : in out Audio_Type);

   type Opaque_Audio_Type;
   type Audio_Type is access Opaque_Audio_Type;

end Gade.Audio;
