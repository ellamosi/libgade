with Gade.Audio_Buffer; use Gade.Audio_Buffer;
with System;

private package Gade.Audio is

   subtype Audio_IO_Address is Word range 16#FF10# .. 16#FF3F#;

   type Audio_Type is tagged private;

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

   type Duty_Type is (Eighth, Quarter, Half, Three_Quarters);
   for Duty_Type use
     (Eighth         => 2#00#,
      Quarter        => 2#01#,
      Half           => 2#10#,
      Three_Quarters => 2#11#);

   type Length_Load_Type is mod 2 ** 6;

   type Duty_Length_Load_Type is record
      --  Bit 5-0 - Sound length data (Write Only) (t1: 0-63)
      Length_Load : Length_Load_Type;
      --  Bit 7-6 - Wave Pattern Duty (Read/Write)
      Duty        : Duty_Type;
   end record;
   for Duty_Length_Load_Type use record
      Length_Load at 0 range 0 .. 5;
      Duty        at 0 range 6 .. 7;
   end record;

   type Channel_Volume_Type is mod 2 ** 4;

   type Envelope_Direction_Type is (Down, Up);
   for Envelope_Direction_Type use (Down => 0, Up => 1);

   type Period_Type is mod 2 ** 3;

   type Start_Volume_Envelope_Direction_Period is record
      --  Bit 2-0 - Number of envelope sweep (n: 0-7)
      --            (If zero, stop envelope operation.)
      Period             : Period_Type;
      --  Bit 3   - Envelope Direction (0=Decrease, 1=Increase)
      Envelope_Direction : Envelope_Direction_Type;
      --  Bit 7-4 - Initial Volume of envelope (0-0Fh) (0=No Sound)
      Volume             : Channel_Volume_Type;
   end record;
   for Start_Volume_Envelope_Direction_Period use record
      Period             at 0 range 0 .. 2;
      Envelope_Direction at 0 range 3 .. 3;
      Volume             at 0 range 4 .. 7;
   end record;

   type Frequency_Type is mod 2 ** 11;

   type Trigger_Length_Enable_Frequency is record
      --   Bit 2-0 - Frequency's higher 3 bits (x) (Write Only)
      Frequency : Frequency_Type;
      --   Bit 6   - Counter/consecutive selection (Read/Write)
      --             (1=Stop output when length in NR11 expires)
      Length_Enable : Boolean;
      --   Bit 7   - Initial (1=Restart Sound)     (Write Only)
      Trigger : Boolean;
   end record;
   for Trigger_Length_Enable_Frequency use record
      Frequency     at 0 range 0 .. 10;
      Length_Enable at 1 range 6 .. 6;
      Trigger       at 1 range 7 .. 7;
   end record;

   type Sweep_Period_Type is mod 2 ** 3;
   type Sweep_Shift_Type is mod 2 ** 3;

--  NR13 FF13 FFFF FFFF Frequency LSB
--  NR14 FF14 TL-- -FFF Trigger, Length enable, Frequency MSB

   type Square1_Channel_Type is record
      Sweep_Period       : Sweep_Period_Type;
      Negate             : Boolean;
      Shift              : Sweep_Shift_Type;
      Duty               : Duty_Type;
      Length_Load        : Length_Load_Type;
      Volume             : Channel_Volume_Type;
      Envelope_Direction : Envelope_Direction_Type;
      Period             : Period_Type;
      Frequency          : Frequency_Type;
      Trigger            : Boolean;
      Length_Enable      : Boolean;
   end record;
   for Square1_Channel_Type use record
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
   for Square1_Channel_Type'Scalar_Storage_Order use System.Low_Order_First;
   for Square1_Channel_Type'Size use 40;

   type Square2_Channel_Type is record
      --  Unused
      Duty               : Duty_Type;
      Length_Load        : Length_Load_Type;
      Volume             : Channel_Volume_Type;
      Envelope_Direction : Envelope_Direction_Type;
      Period             : Period_Type;
      Frequency          : Frequency_Type;
      Trigger            : Boolean;
      Length_Enable      : Boolean;
   end record;
   for Square2_Channel_Type use record
      Duty               at 1 range 6 .. 7;
      Length_Load        at 1 range 0 .. 5;
      Volume             at 2 range 4 .. 7;
      Envelope_Direction at 2 range 3 .. 3;
      Period             at 2 range 0 .. 2;
      Frequency          at 3 range 0 .. 10;
      Trigger            at 4 range 7 .. 7;
      Length_Enable      at 4 range 6 .. 6;
   end record;
   for Square2_Channel_Type'Scalar_Storage_Order use System.Low_Order_First;
   for Square2_Channel_Type'Size use 40;

   type Wave_Channel_Type is record
      --  Need new Length Load type
      Frequency          : Frequency_Type;
      --  Need volume code
      Trigger            : Boolean;
      Length_Enable      : Boolean;
   end record;
   for Wave_Channel_Type use record
      Frequency          at 3 range 0 .. 10;
      Trigger            at 4 range 7 .. 7;
      Length_Enable      at 4 range 6 .. 6;
   end record;
   for Wave_Channel_Type'Size use 40;

   type Noise_Channel_Type is record
      Length_Load        : Length_Load_Type;
      Trigger            : Boolean;
      Length_Enable      : Boolean;
   end record;
   for Noise_Channel_Type use record
      Length_Load        at 1 range 0 .. 5;
      Trigger            at 4 range 7 .. 7;
      Length_Enable      at 4 range 6 .. 6;
   end record;
   for Noise_Channel_Type'Size use 40;

   type Audio_Address_Space is array (Audio_IO_Address'Range) of Byte;

   type Audio_Access_Type is (Named, Address);
   type Audio_Map_Type (Access_Type : Audio_Access_Type := Named) is record
      case Access_Type is
         when Named =>
            Square1 : Square1_Channel_Type;
            Square2 : Square2_Channel_Type;
            Wave    : Wave_Channel_Type;
            Noise   : Noise_Channel_Type;
            --  Control
            --  Unused
            --  Wave Table
         when Address =>
            Space   : Audio_Address_Space;
      end case;
   end record with Unchecked_Union;

   Channel_Max_Level : constant := 15;
   Channel_Min_Level : constant := -15;

   type Pulse_State_Type is (Pulse_Low, Pulse_High);
   Next_Pulse_State : constant array (Pulse_State_Type) of Pulse_State_Type :=
     (Pulse_High, Pulse_Low);

   type Pulse_Levels_Type is array (Pulse_State_Type) of Sample;
   type Pulse_Cycles_Type is array (Pulse_State_Type) of Natural;

   Envelope_Steps : constant array (Envelope_Direction_Type) of Integer :=
     (-1, 1);

   type Square_Channel_State is record
      Level                : Sample;
      Pulse_Levels         : Pulse_Levels_Type;
      Pulse_Cycles         : Pulse_Cycles_Type;
      Pulse_State          : Pulse_State_Type;
      Elapsed_Pulse_Cycles : Natural; -- Deprecate
      Rem_Pulse_Cycles     : Natural;
      Enabled              : Boolean;
      --  length
      Rem_Length_Ticks     : Natural;
      Length_Tick          : Natural;
      --  Turn this into its own record?
      Rem_Vol_Env_Ticks    : Natural;
      --  Env_Period           : Natural;
      Env_Period_Ticks     : Natural;
      Env_Period_Tick      : Natural;
      Env_Step             : Integer;
      Env_Start_Volume     : Channel_Volume_Type;
      Volume               : Channel_Volume_Type;
      Duty                 : Duty_Type;

      --  Freq sweep exclusive:
      Freq_Sweep_Enabled   : Boolean;
      Shadow_Frequency     : Frequency_Type;
      Freq_Sweep_Tick      : Natural;
      Freq_Sweep_Ticks     : Natural;
      Rem_Sweep_Ticks      : Natural;
      Sweep_Shift          : Sweep_Shift_Type;
      Sweep_Negate         : Boolean;
   end record;

   type Base_Channel is tagged null record;
   type Extender_Channel is new Base_Channel with null record;

   procedure Reset (Ch : out Square_Channel_State);

   procedure Trigger
     (Ch        : in out Square_Channel_State;
      Frequency : Frequency_Type);
   procedure Reload_Length
     (Ch     : in out Square_Channel_State;
      Enable : Boolean;
      Load   : Length_Load_Type; -- Up to 256 for wave
      Duty   : Duty_Type);
   procedure Trigger_Volume_Envelope (Ch : in out Square_Channel_State);
   procedure Trigger_Length (Ch : in out Square_Channel_State);
   procedure Trigger_Frequency_Sweep (Ch        : in out Square_Channel_State;
      Frequency : Frequency_Type;
      Period    : Sweep_Period_Type;
      Negate    : Boolean;
                                      Shift     : Sweep_Shift_Type);

   procedure Set_Volume_Envelope
     (Ch        : in out Square_Channel_State;
      Volume    : Channel_Volume_Type;
      Period    : Period_Type;
      Direction : Envelope_Direction_Type);

   procedure Set_Volume
     (Ch     : in out Square_Channel_State;
      Volume : Channel_Volume_Type);
   procedure Set_Period
     (Ch     : in out Square_Channel_State;
      Period : Natural);
   procedure Square_Step (Ch : in out Square_Channel_State; S : out Sample);
   procedure Envelope_Step (Ch : in out Square_Channel_State);
   procedure Length_Step (Ch : in out Square_Channel_State);
   procedure Frequency_Sweep_Step (Ch : in out Square_Channel_State);
   procedure Frequency_Sweep_Shift (Ch : in out Square_Channel_State);

   --  TODO: Calculate these constants/double them
   Hi_Duty_Sample_Multiplier : constant array (Duty_Type) of Natural :=
     (Eighth         => 2,
      Quarter        => 4,
      Half           => 8,
      Three_Quarters => 12);
   Lo_Duty_Sample_Multiplier : constant array (Duty_Type) of Natural :=
     (Eighth         => 14,
      Quarter        => 12,
      Half           => 8,
      Three_Quarters => 4);

   type Sequencer_Cycle_Type is mod 2 ** 16;

   Length_Counter_Tick_Freq  : constant := 256;
   Frequency_Sweep_Tick_Freq : constant := 128;
   Volume_Envelope_Tick_Freq : constant := 64;

   Samples_Length_Counter_Tick  : constant := Samples_Second / Length_Counter_Tick_Freq;
   Samples_Frequency_Sweep_Tick : constant := Samples_Second / Frequency_Sweep_Tick_Freq;
   Samples_Volume_Envelope_Tick : constant := Samples_Second / Volume_Envelope_Tick_Freq;

   --  The Frame sequencer triggers the volume envelope tick at 64 Hz
   --  So we need to clock it a t 8 times that so it can complete its state
   --  cycle at that rate.
   Frame_Sequencer_Freq : constant := Volume_Envelope_Tick_Freq * 8; -- 512 Hz
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


   --  Stepping freq would be 64 Hz * 8 = 512 Hz

--     Hi_Levels : array (Channel_Volume_Type'Range) of Sample :=
--       (Channel_Max_Level, );

   type Audio_Type is tagged record
      Map              : Audio_Map_Type;
      Square1_State    : Square_Channel_State;
      Square2_State    : Square_Channel_State;
      Elapsed_Cycles   : Natural;
--        Envelope_Cycles  : Natural;
--        Length_Cycles    : Natural;
--    Sequencer_Cycles : Sequencer_Cycle_Type;
      Frame_Seq_Step_Idx : Frame_Sequencer_Step_Index;
      Rem_Frame_Seq_Ticks : Natural;
--        Edge_Cycles    : Natural;
--        Level          : Sample;
--        Level_High     : Sample;
--        Level_Low      : Sample;
--        Current        : Boolean;
--        Period         : Natural;
   end record;

end Gade.Audio;
