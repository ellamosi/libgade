package body Gade.Audio.Channels.Wave is

   procedure Set_Table
     (Channel : in out Wave_Channel;
      Table   : Wave_Table_IO_Access)
   is
   begin
      Channel.Table :=  Table;
   end Set_Table;

   overriding
   procedure Reset (Channel : out Wave_Channel) is
   begin
      Base.Base_Audio_Channel (Channel).Reset;
      Channel.Sample_Index := 0;
      Channel.Sample_Time := 1;
      Channel.NRx0 := 0; --  TODO: Mask
      Channel.NRx2 := 0; --  TODO: Mask
      Channel.Volume_Shift := Volume_Shifts (None);
   end Reset;

   overriding
   procedure Next_Sample_Level
     (Channel      : in out Wave_Channel;
      Sample_Level : out Sample;
      Level_Cycles : out Positive)
   is
      Table_Sample  : Wave_Sample;
      Shifted_Sample : Sample;
   begin
      Table_Sample := Channel.Table.Table (Channel.Sample_Index);
      Shifted_Sample := Sample (Table_Sample) * 2 ** Channel.Volume_Shift;
      Sample_Level := Shifted_Sample mod 16;
      Level_Cycles := Channel.Sample_Time;
      Channel.Sample_Index := Channel.Sample_Index + 1;
   end Next_Sample_Level;

   procedure Set_Frequency
     (Channel : in out Wave_Channel;
      Freq    : Frequency_Type)
   is
   begin
      Channel.Sample_Time := Positive (Max_Period - Natural (Freq));
   end Set_Frequency;

   overriding
   procedure Trigger (Channel : in out Wave_Channel) is
   begin
      Base.Base_Audio_Channel (Channel).Trigger;
      --  TODO
   end Trigger;

   overriding
   function Read_NRx0 (Channel : Wave_Channel) return Byte is
   begin
      return Channel.NRx0;
   end Read_NRx0;

   overriding
   procedure Write_NRx0 (Channel : in out Wave_Channel; Value : Byte) is
      NRx0_In : constant NRx0_Power_IO := To_NRx0_Power_IO (Value);
   begin
      Channel.NRx0 := Value or NRx0_Power_Mask;
      Channel.Powered := NRx0_In.Powered;
   end Write_NRx0;

   overriding
   function Read_NRx2 (Channel : Wave_Channel) return Byte is
   begin
      return Channel.NRx2;
   end Read_NRx2;

   overriding
   procedure Write_NRx2 (Channel : in out Wave_Channel; Value : Byte) is
      NRx2_In : constant NRx2_Volume_IO := To_NRx2_Volume_IO (Value);
   begin
      Channel.NRx2 := Value or NRx2_Volume_Mask;
      Channel.Volume_Shift := Volume_Shifts (NRx2_In.Volume);
   end Write_NRx2;

   overriding
   procedure Write_NRx3 (Channel : in out Wave_Channel; Value : Byte) is
   begin
      Channel.Frequency_In.NRx3 := Value;
      Set_Frequency (Channel, Channel.Frequency_In.Frequency);
   end Write_NRx3;

   overriding
   procedure Write_NRx4 (Channel : in out Wave_Channel; Value : Byte) is
   begin
      Channel.Frequency_In.NRx4 := Value;
      Set_Frequency (Channel, Channel.Frequency_In.Frequency);
      Base.Base_Audio_Channel (Channel).Write_NRx4 (Value);
   end Write_NRx4;

end Gade.Audio.Channels.Wave;
