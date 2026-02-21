package body Gade.Audio.Channels.Pulse.Noise is

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

   overriding
   procedure Disable
     (Channel : in out Noise_Channel;
      Mode    : Disable_Mode) is
   begin
      Parent (Channel).Disable (Mode);
      if Mode = APU_Power_Off then
         Channel.Clock_Divisor := 1;
         Channel.Clock_Shift := 0;
         Channel.LFSR := 0;
         Channel.NRx3 := 16#00#;
      end if;
   end Disable;

   overriding
   procedure Trigger (Channel : in out Noise_Channel) is
   begin
      Pulse_Channel (Channel).Trigger;

      --  https://gbdev.gg8.se/wiki/articles/Gameboy_sound_hardware#Trigger_Event
      --
      --  Noise channel's LFSR bits are all set to 1.
      Channel.LFSR := 16#7FFF#;
   end Trigger;

   overriding
   procedure Next_Sample_Level
     (Channel      : in out Noise_Channel;
      Sample_Level : out Channel_Sample;
      Level_Cycles : out Positive)
   is
      LFSR : Shift_Register renames Channel.LFSR;

      Low_0, Low_1, XORed : Shift_Register;
      New_Pulse_State : Pulse_State_Type;
   begin
      Level_Cycles := Channel.Clock_Divisor * 2 ** Channel.Clock_Shift;

      --  https://gbdev.gg8.se/wiki/articles/Gameboy_sound_hardware#Noise_Channel
      --
      --  When clocked by the frequency timer, the low two bits (0 and 1) are
      --  XORed, all bits are shifted right by one, and the result of the XOR is
      --  put into the now-empty high bit. If width mode is 1 (NR43), the XOR
      --  result is ALSO put into bit 6 AFTER the shift, resulting in a 7-bit
      --  LFSR.
      Low_0 := LFSR and 16#0001#;
      LFSR := LFSR / 2;
      Low_1 := LFSR and 16#0001#;
      XORed := Low_0 xor Low_1;
      LFSR := LFSR or (XORed * 2 ** 14);
      if Channel.LFSR_Width = Half then
         LFSR := (LFSR and 16#7FBF#) or (XORed * 2 ** 6);
      end if;

      --  The waveform output is bit 0 of the LFSR, INVERTED.
      New_Pulse_State := Output_Pulse_State (LFSR and 16#0001#);
      Sample_Level := Channel.Pulse_Levels (New_Pulse_State);
   end Next_Sample_Level;

   overriding
   function Read_NRx3 (Channel : Noise_Channel) return Byte is
   begin
      return Channel.NRx3; --  Does not require any masking
   end Read_NRx3;

   overriding
   procedure Write_NRx3 (Channel : in out Noise_Channel; Value : Byte) is
      NRx3_In : constant NRx3_Noise_IO := To_NRx3_Noise_IO (Value);
   begin
      Channel.NRx3 := Value;
      --  Clock the sample level timer to half the period:
      Channel.Clock_Divisor := Half_Divisor (NRx3_In.Divisor_Code);
      Channel.Clock_Shift := Natural (NRx3_In.Clock_Shift);
      Channel.LFSR_Width := NRx3_In.LFSR_Width;
   end Write_NRx3;

   overriding
   function Id (Channel : Noise_Channel) return Channel_Id is
      pragma Unreferenced (Channel);
   begin
      return NR4;
   end Id;

end Gade.Audio.Channels.Pulse.Noise;
