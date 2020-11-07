separate (Gade.Audio.Channels)
package body Frequency_Mixin is

   overriding
   procedure Write_NRx3
     (Channel : in out Channel_With_Frequency;
      Value   : Byte)
   is
      New_Frequency : Frequency_Type;
   begin
      Channel.Frequency_In.NRx3 := Value;
      New_Frequency := Channel.Frequency_In.Frequency;
      Channel_With_Frequency'Class (Channel).Set_Frequency (New_Frequency);
   end Write_NRx3;

   overriding
   procedure Write_NRx4
     (Channel : in out Channel_With_Frequency;
      Value   : Byte)
   is
      New_Frequency : Frequency_Type;
   begin
      Channel.Frequency_In.NRx4 := Value;
      New_Frequency := Channel.Frequency_In.Frequency;
      Channel_With_Frequency'Class (Channel).Set_Frequency (New_Frequency);
      Base_Channel (Channel).Write_NRx4 (Value);
   end Write_NRx4;

   overriding
   procedure Disable
     (Channel : in out Channel_With_Frequency;
      Mode    : Disable_Mode)
   is
   begin
      Parent (Channel).Disable (Mode);
      if Mode = APU_Power_Off then Channel.Frequency_In.NRx3 := 0; end if;
   end Disable;

end Frequency_Mixin;
