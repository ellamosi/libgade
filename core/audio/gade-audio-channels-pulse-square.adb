package body Gade.Audio.Channels.Pulse.Square is

   overriding
   procedure Reset (Channel : out Square_Channel) is
   begin
      Pulse_Channel (Channel).Reset;
      Channel.Pulse_State := Pulse_Low;
      Channel.Pulse_Cycles := (1, 1);
      Channel.Duty := Duty_Type'Val (0);
      Channel.NRx1 := NRx1_Duty_Mask;
   end Reset;

   overriding
   procedure Turn_Off (Channel : in out Square_Channel) is
   begin
      Pulse_Channel (Channel).Turn_Off;
      Channel.Pulse_State := Pulse_Low;
      Channel.Pulse_Cycles := (1, 1);
      Channel.Duty := Duty_Type'Val (0);
      Channel.NRx1 := NRx1_Duty_Mask;
   end Turn_Off;

   overriding
   procedure Set_Frequency
     (Channel : in out Square_Channel;
      Freq    : Frequency_Type)
   is
      Period  : constant Natural := Max_Period - Natural (Freq);
   begin
      Channel.Pulse_Cycles :=
        (Pulse_Low  => Period * Lo_Duty_Sample_Multiplier (Channel.Duty),
         Pulse_High => Period * Hi_Duty_Sample_Multiplier (Channel.Duty));
--        Put_Line ("Set_Frequency" &
--                  Freq'Img & Channel.Pulse_Cycles (Pulse_Low)'Img &
--                    Freq'Img & Channel.Pulse_Cycles (Pulse_High)'Img);
   end Set_Frequency;

   overriding
   procedure Next_Sample_Level
     (Channel      : in out Square_Channel;
      Sample_Level : out Sample;
      Level_Cycles : out Positive)
   is
   begin
      Channel.Pulse_State := Next_Pulse_State (Channel.Pulse_State);
      Sample_Level := Channel.Pulse_Levels (Channel.Pulse_State);
      Level_Cycles := Channel.Pulse_Cycles (Channel.Pulse_State);
   end Next_Sample_Level;

   overriding
   function Read_NRx1 (Channel : Square_Channel) return Byte is
   begin
      return Channel.NRx1;
   end Read_NRx1;

   overriding
   procedure Write_NRx1 (Channel : in out Square_Channel; Value : Byte) is
      Duty : constant Duty_Type := Duty_Type'Val (Value / NRx1_Duty_Div);
   begin
      Channel.Duty := Duty;
      Channel.NRx1 := Value or NRx1_Duty_Mask;
      Pulse_Channel (Channel).Write_NRx1 (Value);
   end Write_NRx1;

   overriding
   function Name (Channel : Square_Channel) return String is
      pragma Unreferenced (Channel);
   begin
      return "Square 2";
   end Name;

end Gade.Audio.Channels.Pulse.Square;
