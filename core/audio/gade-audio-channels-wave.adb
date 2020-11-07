package body Gade.Audio.Channels.Wave is

   overriding
   procedure Reset (Channel : in out Wave_Channel) is
   begin
      Parent (Channel).Reset;
      Channel.Table_IO.Space := Initial_DMG_Table;
   end Reset;

   overriding
   procedure Disable
     (Channel : in out Wave_Channel;
      Mode    : Disable_Mode)
   is
   begin
      Parent (Channel).Disable (Mode);
      if Mode = APU_Power_Off then
         Channel.Sample_Index := 0;
         Channel.Sample_Time := 1;
         Channel.NRx0 := NRx0_Power_Mask;
         Channel.NRx2 := NRx2_Volume_Mask;
         Channel.Volume_Shift := Volume_Shifts (None);
         Channel.Sample_Diff := 16 / 2 ** Channel.Volume_Shift;
      end if;
   end Disable;

   overriding
   procedure Next_Sample_Level
     (Channel      : in out Wave_Channel;
      Sample_Level : out Channel_Sample;
      Level_Cycles : out Positive)
   is
      Table_Sample  : Wave_Sample;
   begin
      Table_Sample := Channel.Table_IO.Table (Channel.Sample_Index);
      Sample_Level := Channel_Sample (Table_Sample) / 2 ** Channel.Volume_Shift;
      Sample_Level := Sample_Level * 2 - Channel.Sample_Diff;
      Level_Cycles := Channel.Sample_Time;
      Channel.Sample_Index := Channel.Sample_Index + 1;
   end Next_Sample_Level;

   overriding
   procedure Set_Frequency
     (Channel : in out Wave_Channel;
      Freq    : Frequency_Type)
   is
      Period : constant Natural := Max_Period - Natural (Freq);
   begin
      Channel.Sample_Time := Natural'Max (Period / 2, 1);
   end Set_Frequency;

   overriding
   procedure Trigger (Channel : in out Wave_Channel) is
   begin
      Parent (Channel).Trigger;
      Channel.Sample_Index := 0;
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
      Channel.Update_DAC_Power_State (NRx0_In.Powered);
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
      Channel.Sample_Diff := Sample_Diffs (NRx2_In.Volume);
   end Write_NRx2;

   function Read_Table
     (Channel : Wave_Channel;
      Address : Wave_Table_IO_Address) return Byte is
   begin
      return Channel.Table_IO.Space (Address);
   end Read_Table;

   procedure Write_Table
     (Channel : in out Wave_Channel;
      Address : Wave_Table_IO_Address;
      Value   : Byte)
   is
   begin
      Channel.Table_IO.Space (Address) := Value;
   end Write_Table;

   overriding
   function Id (Channel : Wave_Channel) return Channel_Id is
      pragma Unreferenced (Channel);
   begin
      return NR3;
   end Id;

end Gade.Audio.Channels.Wave;
