with Gade.Audio.Channels.Mixins.Volume_Envelope;

private package Gade.Audio.Channels.Noise is

   type Noise_Channel is new Audio_Channel with private;

   overriding
   procedure Reset (Ch : out Noise_Channel);

--     procedure Envelope_Step (Ch : in out Noise_Channel);
--     procedure Length_Step (Ch : in out Noise_Channel);

private

   type Pulse_State_Type is (Pulse_Low, Pulse_High);

   type Pulse_Levels_Type is array (Pulse_State_Type) of Sample;
   type Pulse_Cycles_Type is array (Pulse_State_Type) of Natural;

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

   type LFSR_Width_Mode is (Full, Half);
   for LFSR_Width_Mode use (Full => 0, Half => 1);

   Output_Pulse_State : constant array (Shift_Register range 0 .. 1) of Pulse_State_Type :=
     (Pulse_High, Pulse_Low); -- Takes care of LFSR output negation

   type NRx3_Noise_IO is record
      Clock_Shift  : Clock_Shift_Type;
      LFSR_Width   : LFSR_Width_Mode;
      Divisor_Code : Divisor_Code_Type;
   end record;
   for NRx3_Noise_IO use record
      Clock_Shift  at 0 range 4 .. 7;
      LFSR_Width   at 0 range 3 .. 3;
      Divisor_Code at 0 range 0 .. 2;
   end record;
   for NRx3_Noise_IO'Size use Byte'Size;

   function To_NRx3_Noise_IO is new Ada.Unchecked_Conversion
     (Source => Byte,
      Target => NRx3_Noise_IO);

   package Base is new Channels.Base;

   package Volume_Envelope is new Mixins.Volume_Envelope (Base.Base_Audio_Channel);

   type Noise_Channel is new Volume_Envelope.Volume_Envelope_Audio_Channel with record
      Pulse_Levels         : Pulse_Levels_Type;
      --  Pulse_Cycles         : Pulse_Cycles_Type;
      --  Pulse_State          : Pulse_State_Type;
      --  Rem_Pulse_Cycles     : Natural;
      --  Noise specific
      Divisor : Natural;
      LFSR : Shift_Register;
      LFSR_Width : LFSR_Width_Mode;
      NRx3 : Byte;
   end record;

   --  TODO: Useful for SQ1, SQ1, Noise, but not WAVE (volume ranges differ)
--     procedure Set_Volume
--       (Ch     : in out Noise_Channel;
--        Volume : Channel_Volume_Type);


   overriding
   procedure Trigger (Ch : in out Noise_Channel);
   --  procedure Trigger_Volume_Envelope (Ch : in out Noise_Channel);
   --  procedure Set_Volume_Envelope (Ch : in out Noise_Channel);

   overriding
   procedure Next_Sample
     (Ch : in out Noise_Channel;
      S  : out Sample;
      T  : out Positive);

   overriding
   function Read_NRx3 (Ch : Noise_Channel) return Byte;

   overriding
   procedure Write_NRx3 (Ch : in out Noise_Channel; Value : Byte);

end Gade.Audio.Channels.Noise;
