with System;

private package Gade.Audio.Noise is

   type Channel_Volume_Type is mod 2 ** 4; --

   type Noise_Channel is tagged private;

   procedure Reset (Ch : out Noise_Channel);

   procedure Read
     (Ch       : in out Noise_Channel'Class;
      Register : Channel_Register;
      Value    : out Byte);

   procedure Write
     (Ch       : in out Noise_Channel'Class;
      Register : Channel_Register;
      Value    : Byte);

   procedure Square_Step (Ch : in out Noise_Channel; S : out Sample);
   procedure Envelope_Step (Ch : in out Noise_Channel);
   procedure Length_Step (Ch : in out Noise_Channel);

private

   Read_Masks : constant Register_Masks :=
     (NRx0 => 16#FF#,
      NRx1 => 16#FF#,
      NRx2 => 16#00#,
      NRx3 => 16#00#,
      NRx4 => 16#BF#);

   Channel_Max_Level : constant := 15;
   Channel_Min_Level : constant := -15;

   type Sweep_Period_Type is mod 2 ** 3;
   type Sweep_Shift_Type is mod 2 ** 3;

   type Length_Load_Type is mod 2 ** 6;

   type Pulse_State_Type is (Pulse_Low, Pulse_High);
   Next_Pulse_State : constant array (Pulse_State_Type) of Pulse_State_Type :=
     (Pulse_High, Pulse_Low);

   type Pulse_Levels_Type is array (Pulse_State_Type) of Sample;
   type Pulse_Cycles_Type is array (Pulse_State_Type) of Natural;

   --  type Channel_Volume_Type is mod 2 ** 4; --  TODO: Not for WAVE
   type Envelope_Direction_Type is (Down, Up);
   for Envelope_Direction_Type use (Down => 0, Up => 1);
   type Period_Type is mod 2 ** 3;

   Envelope_Steps : constant array (Envelope_Direction_Type) of Integer :=
     (-1, 1);

   type Volume_Envelope_State is record
      TMR          : Timer;
      Step         : Integer;
      Start_Volume : Channel_Volume_Type;
   end record;

   type Clock_Shift_Type is mod 2 ** 4;
   type Divisor_Code_Type is mod 2 ** 3;

   Divisor : constant array (Divisor_Code_Type'Range) of Natural :=
     (0 => 8,
      1 => 16,
      2 => 32,
      3 => 48,
      4 => 64,
      5 => 80,
      6 => 96,
      7 => 112);

   type Shift_Register is mod 2**15;

   --       FF1F ---- ---- Not used
   --  NR41 FF20 --LL LLLL Length load (64-L)
   --  NR42 FF21 VVVV APPP Starting volume, Envelope add mode, period
   --  NR43 FF22 SSSS WDDD Clock shift, Width mode of LFSR, Divisor code
   --  NR44 FF23 TL-- ---- Trigger, Length enable

   type Channel_IO_Space is array (Channel_Register'Range) of Byte;
   type Channel_IO (Access_Type : Audio_Access_Type := Named) is record
      case Access_Type is
         when Named =>
            --  NRx1
            Length_Load        : Length_Load_Type;
            --  NRx2
            Volume             : Channel_Volume_Type;
            Envelope_Direction : Envelope_Direction_Type;
            Period             : Period_Type;
            --  NRx3
            Clock_Shift        : Clock_Shift_Type;
            LFSR_Width_Mode    : Boolean; --
            Divisor_Code       : Divisor_Code_Type;
            --  NRx4
            Trigger            : Boolean;
            Length_Enable      : Boolean;
         when Address =>
            Space              : Channel_IO_Space;
      end case;
   end record with Unchecked_Union;
   for Channel_IO use record
      Length_Load        at 1 range 0 .. 5;
      Volume             at 2 range 4 .. 7;
      Envelope_Direction at 2 range 3 .. 3;
      Period             at 2 range 0 .. 2;
      Clock_Shift        at 3 range 4 .. 7;
      LFSR_Width_Mode    at 3 range 3 .. 3; --
      Divisor_Code       at 3 range 0 .. 2;
      Trigger            at 4 range 7 .. 7;
      Length_Enable      at 4 range 6 .. 6;
   end record;
   for Channel_IO'Scalar_Storage_Order use System.Low_Order_First;
   for Channel_IO'Size use 40;

   type Noise_Channel is tagged record
      IO : Channel_IO;

      Level                : Sample;
      Pulse_Levels         : Pulse_Levels_Type;
      Pulse_Cycles         : Pulse_Cycles_Type;
      Pulse_State          : Pulse_State_Type;
      Rem_Pulse_Cycles     : Natural;
      Enabled              : Boolean;
      Length_Timer         : Timer;
      Volume_Envelope : Volume_Envelope_State;
      Volume               : Channel_Volume_Type;
      --  Noise specific
      Divisor : Natural;
      LFSR : Shift_Register;
   end record;

   --  TODO: Useful for SQ1, SQ1, Noise, but not WAVE (volume ranges differ)
   procedure Set_Volume
     (Ch     : in out Noise_Channel;
      Volume : Channel_Volume_Type);
   procedure Trigger (Ch : in out Noise_Channel);
   procedure Trigger_Volume_Envelope (Ch : in out Noise_Channel);
   procedure Trigger_Length (Ch : in out Noise_Channel);
   procedure Reload_Length (Ch : in out Noise_Channel);
   procedure Set_Volume_Envelope (Ch : in out Noise_Channel);
   procedure Set_Noise (Ch : in out Noise_Channel);

end Gade.Audio.Noise;
