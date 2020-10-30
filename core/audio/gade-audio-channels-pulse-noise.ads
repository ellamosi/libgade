package Gade.Audio.Channels.Pulse.Noise is

   type Noise_Channel is new Pulse_Channel with private;

   overriding
   function Id (Channel : Noise_Channel) return Channel_Id;

private

   type Clock_Shift_Type is mod 2 ** 4;
   type Divisor_Code_Type is mod 2 ** 3;

   --  https://gbdev.gg8.se/wiki/articles/Gameboy_sound_hardware#Noise_Channel
   Divisor : constant array (Divisor_Code_Type'Range) of Natural :=
     (0 => 8,
      1 => 16,
      2 => 32,
      3 => 48,
      4 => 64,
      5 => 80,
      6 => 96,
      7 => 112);

   Half_Divisor : constant array (Divisor_Code_Type'Range) of Natural :=
     (0 => Divisor (0) / 2,
      1 => Divisor (1) / 2,
      2 => Divisor (2) / 2,
      3 => Divisor (3) / 2,
      4 => Divisor (4) / 2,
      5 => Divisor (5) / 2,
      6 => Divisor (6) / 2,
      7 => Divisor (7) / 2);

   type Shift_Register is mod 2**15;

   type LFSR_Width_Mode is (Full, Half);
   for LFSR_Width_Mode use (Full => 0, Half => 1);

   --  Array to take care of LFSR output negation and type conversion
   Output_Pulse_State : constant array (Shift_Register range 0 .. 1)
     of Pulse_State_Type := (Pulse_High, Pulse_Low);

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

   subtype Parent is Pulse_Channel;
   type Noise_Channel is new Parent with record
      Clock_Divisor : Natural;
      Clock_Shift   : Natural;
      LFSR          : Shift_Register;
      LFSR_Width    : LFSR_Width_Mode;
      NRx3          : Byte;
   end record;

   overriding
   procedure Disable
     (Channel : in out Noise_Channel;
      Mode    : Disable_Mode);

   overriding
   procedure Trigger (Channel : in out Noise_Channel);

   overriding
   procedure Next_Sample_Level
     (Channel      : in out Noise_Channel;
      Sample_Level : out Sample;
      Level_Cycles : out Positive);

   overriding
   function Read_NRx3 (Channel : Noise_Channel) return Byte;

   overriding
   procedure Write_NRx3 (Channel : in out Noise_Channel; Value : Byte);

end Gade.Audio.Channels.Pulse.Noise;
