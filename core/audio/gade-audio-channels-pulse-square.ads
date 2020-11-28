package Gade.Audio.Channels.Pulse.Square is

   type Square_Channel is new Pulse_Channel with private;

   type Square_Channel_Access is access all Square_Channel;

   overriding
   function Id (Channel : Square_Channel) return Channel_Id;

private

   Next_Pulse_State : constant array (Pulse_State_Type) of Pulse_State_Type :=
     (Pulse_High, Pulse_Low);

   type Duty_Type is (Eighth, Quarter, Half, Three_Quarters);
   for Duty_Type use
     (Eighth         => 2#00#,
      Quarter        => 2#01#,
      Half           => 2#10#,
      Three_Quarters => 2#11#);

   type Pulse_Cycles_Type is array (Pulse_State_Type) of Positive;

   Hi_Duty_Sample_Multiplier : constant array (Duty_Type) of Natural :=
     (Eighth         => 1,
      Quarter        => 2,
      Half           => 4,
      Three_Quarters => 6);
   Lo_Duty_Sample_Multiplier : constant array (Duty_Type) of Natural :=
     (Eighth         => 7,
      Quarter        => 6,
      Half           => 4,
      Three_Quarters => 2);

   NRx1_Duty_Mask : constant Byte := 16#3F#;
   NRx1_Duty_Div : constant Byte := 2 ** 6;


   package Frequency_Mixin is new Channels.Frequency_Mixin (Pulse_Channel);
   use Frequency_Mixin;

   subtype Parent is Channel_With_Frequency;
   type Square_Channel is new Parent with record
      Pulse_Cycles : Pulse_Cycles_Type;
      Pulse_State  : Pulse_State_Type;
      Duty         : Duty_Type;
      NRx1         : Byte;
   end record;

   overriding
   procedure Disable
     (Channel : in out Square_Channel;
      Mode    : Disable_Mode);

   overriding
   procedure Next_Sample_Level
     (Channel      : in out Square_Channel;
      Sample_Level : out Channel_Sample;
      Level_Cycles : out Positive);

   overriding
   function Read_NRx1 (Channel : Square_Channel) return Byte;

   overriding
   procedure Write_NRx1 (Channel : in out Square_Channel; Value : Byte);

   overriding
   procedure Set_Frequency
     (Channel : in out Square_Channel;
      Freq    : Frequency_Type);

end Gade.Audio.Channels.Pulse.Square;
