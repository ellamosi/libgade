with System;

private package Gade.Audio.Square is

   type Channel_Volume_Type is mod 2 ** 4; --  TODO: Not for WAVE
   type Frequency_Type is mod 2 ** 11;

   type Square_Channel is tagged private;

   procedure Reset (Ch : out Square_Channel);

   --  Override for additional Reset actions on channel extensions
   procedure Extended_Reset (Ch : in out Square_Channel) is null;

   procedure Read
     (Ch       : in out Square_Channel'Class;
      Register : Channel_Register;
      Value    : out Byte);

   procedure Write
     (Ch       : in out Square_Channel'Class;
      Register : Channel_Register;
      Value    : Byte);

   --  Override if channel extension implements the NRx0 register
   procedure NRx0_Read
     (Ch    : in out Square_Channel;
      Value : out Byte);

   --  Override if channel extension implements the NRx0 register
   procedure NRx0_Write
     (Ch    : in out Square_Channel;
      Value : Byte) is null;

   --  Override for additional Trigger actions on channel extensions
   --  TODO: Re-evaluate signature, we should not know what gets exposed to
   --  extensions or not
   procedure Extended_Trigger
     (Ch        : in out Square_Channel;
      Frequency : Frequency_Type) is null;

   procedure Square_Step (Ch : in out Square_Channel; S : out Sample);
   procedure Envelope_Step (Ch : in out Square_Channel);
   procedure Length_Step (Ch : in out Square_Channel);

   --  TODO: Useful for SQ1, SQ1, Noise, but not WAVE (volume ranges differ)
   procedure Set_Volume
     (Ch     : in out Square_Channel;
      Volume : Channel_Volume_Type);

   --  TODO: Useful for SQ1, SQ1, WAVE, but not Noise
   procedure Set_Frequency
     (Ch   : in out Square_Channel;
      Freq : Frequency_Type);

   --  TODO: Should be for all channels:
   procedure Disable (Ch : in out Square_Channel);

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

   type Sweep_Period_Type is mod 2 ** 3;
   type Sweep_Shift_Type is mod 2 ** 3;

   Channel_Max_Level : constant := 15;
   Channel_Min_Level : constant := -15;

   type Pulse_State_Type is (Pulse_Low, Pulse_High);
   Next_Pulse_State : constant array (Pulse_State_Type) of Pulse_State_Type :=
     (Pulse_High, Pulse_Low);

   type Pulse_Levels_Type is array (Pulse_State_Type) of Sample;
   type Pulse_Cycles_Type is array (Pulse_State_Type) of Natural;

   Envelope_Steps : constant array (Envelope_Direction_Type) of Integer :=
     (-1, 1);

   type Volume_Envelope_State is record
      TMR          : Timer;
      Step         : Integer;
      Start_Volume : Channel_Volume_Type;
   end record;

   procedure Trigger
     (Ch        : in out Square_Channel;
      Frequency : Frequency_Type);
   procedure Reload_Length
     (Ch     : in out Square_Channel;
      Enable : Boolean;
      Load   : Length_Load_Type; -- Up to 256 for wave
      Duty   : Duty_Type);
   procedure Trigger_Volume_Envelope (Ch : in out Square_Channel);
   procedure Trigger_Length (Ch : in out Square_Channel);

   procedure Set_Volume_Envelope
     (Ch        : in out Square_Channel;
      Volume    : Channel_Volume_Type;
      Period    : Period_Type;
      Direction : Envelope_Direction_Type);

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

   --  NRx0 is unused
   subtype Square_Register is Channel_Register range NRx1 .. NRx4;
   type Channel_IO_Space is array (Square_Register'Range) of Byte;
   type Channel_IO (Access_Type : Audio_Access_Type := Named) is record
      case Access_Type is
         when Named =>
            Duty               : Duty_Type;
            Length_Load        : Length_Load_Type;
            Volume             : Channel_Volume_Type;
            Envelope_Direction : Envelope_Direction_Type;
            Period             : Period_Type;
            Frequency          : Frequency_Type;
            Trigger            : Boolean;
            Length_Enable      : Boolean;
         when Address =>
            Space              : Channel_IO_Space;
      end case;
   end record with Unchecked_Union;
   for Channel_IO use record
      Duty               at 0 range 6 .. 7;
      Length_Load        at 0 range 0 .. 5;
      Volume             at 1 range 4 .. 7;
      Envelope_Direction at 1 range 3 .. 3;
      Period             at 1 range 0 .. 2;
      Frequency          at 2 range 0 .. 10;
      Trigger            at 3 range 7 .. 7;
      Length_Enable      at 3 range 6 .. 6;
   end record;
   for Channel_IO'Scalar_Storage_Order use System.Low_Order_First;
   for Channel_IO'Size use 32;

   type Square_Channel is tagged record
      IO                   : Channel_IO;
      Level                : Sample;
      Pulse_Levels         : Pulse_Levels_Type;
      Pulse_Cycles         : Pulse_Cycles_Type;
      Pulse_State          : Pulse_State_Type;
      Rem_Pulse_Cycles     : Natural;
      Enabled              : Boolean;
      Length_Timer         : Timer;
      Volume_Envelope : Volume_Envelope_State;
      Volume               : Channel_Volume_Type;
      Duty                 : Duty_Type;
   end record;

end Gade.Audio.Square;
