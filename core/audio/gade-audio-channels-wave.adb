with Ada.Text_IO; use Ada.Text_IO;

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
      end if;
   end Disable;

   overriding
   procedure Next_Sample_Level
     (Channel      : in out Wave_Channel;
      Sample_Level : out Sample;
      Level_Cycles : out Positive)
   is
      Table_Sample  : Wave_Sample;
   begin
      Table_Sample := Channel.Table_IO.Table (Channel.Sample_Index);
      Sample_Level := Sample (Table_Sample) / 2 ** Channel.Volume_Shift;
      Sample_Level := Sample_Level * 2; -- TODO: Should not need this
      Level_Cycles := Channel.Sample_Time;
      Channel.Sample_Index := Channel.Sample_Index + 1;
      --  Put (Sample_Level'Img);
   end Next_Sample_Level;

   overriding
   procedure Set_Frequency
     (Channel : in out Wave_Channel;
      Freq    : Frequency_Type)
   is
   begin
      Put_Line ("Wave.Set_Frequency" & Freq'Img);
      Channel.Sample_Time := Natural'Max ((Max_Period - Natural (Freq)) / 2, 1);
   end Set_Frequency;

   overriding
   procedure Trigger (Channel : in out Wave_Channel) is
   begin
      Put_Line ("Wave Trigger (Power: " & Channel.Powered'Img & ")");
      Parent (Channel).Trigger;
      Channel.Sample_Index := 0;
--        if Channel.Powered then
--
--        else
--           Put_Line ("Trigger Unpowered Wave Channel");
--        end if;
      --  TODO: Handle 0 length (also on pulse channels)
   end Trigger;

   overriding
   function Can_Enable (Channel : Wave_Channel) return Boolean is
   begin
      return Channel.Powered;
   end Can_Enable;

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
      if not Channel.Powered then
         Put_Line ("Powering OFF Wave Channel");
         Wave_Channel'Class (Channel).Disable (DAC_Power_Off);
         --  Registers get cleared when powering off?
         --  Channel.Volume_Shift := Volume_Shifts (None);
--           Channel.NRx0 := 0; --  TODO: Mask
--           Channel.NRx2 := 0; --  TODO: Mask
--           Channel.Set_Frequency (0);
      else
         Put_Line ("Powering ON Wave Channel");
      end if;
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
