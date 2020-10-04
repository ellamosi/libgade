with Gade.Audio_Buffer; use Gade.Audio_Buffer;

private package Gade.Audio is

   package Test is end Test;

   ------- Should be somewhere else (Channel package?)
   type Channel_Register is (NRx0, NRx1, NRx2, NRx3, NRx4);
   type Register_Masks is array (Channel_Register'Range) of Byte;

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

   type Channel is (NR1, NR2, NR3, NR4);

   Channel_Count : constant := Channel'Range_Length;
   Channel_Register_Count : constant := Channel_Register'Range_Length;

   subtype NR1_Address is Audio_IO_Address range 16#FF10# .. 16#FF14#;
   subtype NR2_Address is Audio_IO_Address range 16#FF15# .. 16#FF19#;
   subtype NR3_Address is Audio_IO_Address range 16#FF1A# .. 16#FF1E#;
   subtype NR4_Address is Audio_IO_Address range 16#FF1F# .. 16#FF23#;
   subtype Control_Address is Audio_IO_Address range 16#FF24# .. 16#FF26#;

   subtype Tick_Type is Natural range 0 .. 1;

   type Audio_Access_Type is (Named, Address);

   --  The Frame sequencer triggers the volume envelope tick at 64 Hz
   --  So we need to clock it a t 8 times that so it can complete its state
   --  cycle at that rate.
   Frame_Sequencer_Freq : constant := 512; -- Hz
   Samples_Frame_Sequencer_Tick : constant := Samples_Second / Frame_Sequencer_Freq;

   type Frame_Sequencer_Trigger is
     (Length_Counter, Frequency_Sweep, Volume_Envelope);

   type Frame_Sequencer_Step is
     (None, Length_Counter, Length_Counter_Frequency_Sweep, Volume_Envelope);

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

   procedure Tick_Frame_Sequencer (Audio : in out Audio_Type);

   function To_Channel_Register
     (Address : Audio_IO_Address)
      return Channel_Register;

   type Opaque_Audio_Type;
   type Audio_Type is access Opaque_Audio_Type;

end Gade.Audio;
