package body Gade.Audio.Channels.Noise is

   overriding
   procedure Reset (Ch : out Noise_Channel) is
   begin
      Base.Base_Audio_Channel'Class (Ch).Reset;
      --  Ch.Pulse_State := Pulse_Low;
      Ch.Pulse_Levels := (0, 0);

      --  Noise specific
      Ch.Divisor := 1;
      Ch.LFSR := 0;
   end Reset;

--     procedure Set_Volume
--       (Ch : in out Noise_Channel; Volume : Channel_Volume_Type)
--     is
--        Volume_Sample : constant Sample := Sample (Volume);
--     begin
--        --  TODO: Mixin
--        Ch.Volume := Volume;
--        Ch.Pulse_Levels := (-Volume_Sample, Volume_Sample);
--     end Set_Volume;

   overriding
   procedure Trigger (Ch : in out Noise_Channel) is
   begin
      --  TODO: Mixin
      --  Trigger_Volume_Envelope (Ch);
      --  Set_Volume (Ch, Ch.Volume_Envelope.Start_Volume);

      --  Noise Specific
      Ch.LFSR := 16#7FFF#;
   end Trigger;

   overriding
   procedure Next_Sample
     (Ch : in out Noise_Channel;
      S  : out Sample;
      T  : out Positive)
   is
      Low_0, Low_1, XORed : Shift_Register;
      --  R : Sample;
      New_Pulse_State : Pulse_State_Type;
   begin
      T := Ch.Divisor * 16; -- ???

      Low_0 := Ch.LFSR and 16#0001#;
      Ch.LFSR := Ch.LFSR / 2;
      Low_1 := Ch.LFSR and 16#0001#;
      XORed := Low_0 xor Low_1;
      Ch.LFSR := Ch.LFSR or (XORed * 2 ** 14);
      if Ch.LFSR_Width = Half then
         Ch.LFSR := (Ch.LFSR and 16#7FBF#) or (XORed * 2 ** 6);
      end if;
      New_Pulse_State := Output_Pulse_State (Ch.LFSR and 16#0001#);
      S := Pulse_Level (New_Pulse_State);
   end Next_Sample;

   overriding
   function Read_NRx3 (Ch : Noise_Channel) return Byte is
   begin
      return Ch.NRx3; --  Does not require any masking
   end Read_NRx3;

   overriding
   procedure Write_NRx3 (Ch : in out Noise_Channel; Value : Byte) is
      NRx3_In : constant NRx3_Noise_IO := To_NRx3_Noise_IO (Value);
   begin
      Ch.NRx3 := Value;
      Ch.Divisor := Divisor (NRx3_In.Divisor_Code);
      Ch.LFSR_Width := NRx3_In.LFSR_Width;
      --  TODO: Clock shift!?
   end Write_NRx3;

end Gade.Audio.Channels.Noise;
