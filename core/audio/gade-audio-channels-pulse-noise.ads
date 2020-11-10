package Gade.Audio.Channels.Pulse.Noise is

   type Noise_Channel is new Pulse_Channel with private;

   type Noise_Channel_Access is access all Noise_Channel;

   overriding
   function Id (Channel : Noise_Channel) return Channel_Id;

private

   type Clock_Shift_Type is mod 2 ** 4;
   type Divisor_Code_Type is mod 2 ** 3;

   --  Divisor values from:
   --  https://gbdev.gg8.se/wiki/articles/Gameboy_sound_hardware#Noise_Channel
   --
   --  As they represent full periods, half them to actually get pulse times.
   Half_Divisor : constant array (Divisor_Code_Type'Range) of Natural :=
     (0 => 8 / 2,
      1 => 16 / 2,
      2 => 32 / 2,
      3 => 48 / 2,
      4 => 64 / 2,
      5 => 80 / 2,
      6 => 96 / 2,
      7 => 112 / 2);

   type Shift_Register is mod 2**15;

   type LFSR_Width_Mode is (Full, Half);
   for LFSR_Width_Mode use (Full => 0, Half => 1);

   --  Array to take care of LFSR output negation and type conversion
   Output_Pulse_State : constant array (Shift_Register range 0 .. 1)
     of Pulse_State_Type := (Pulse_High, Pulse_Low);

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
      Sample_Level : out Channel_Sample;
      Level_Cycles : out Positive);

   overriding
   function Read_NRx3 (Channel : Noise_Channel) return Byte;

   overriding
   procedure Write_NRx3 (Channel : in out Noise_Channel; Value : Byte);

end Gade.Audio.Channels.Pulse.Noise;
