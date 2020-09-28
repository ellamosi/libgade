package Gade.Audio.Square.Plain is

   Read_Masks : constant Register_Masks :=
     (NRx0 => 16#FF#,
      NRx1 => 16#3F#,
      NRx2 => 16#00#,
      NRx3 => 16#FF#,
      NRx4 => 16#BF#);

   type Plain_Square_Channel is new Square_Channel with private;

   procedure Reset (Ch : out Plain_Square_Channel);

   --  Override for additional Reset actions on channel extensions
   procedure Extended_Reset (Ch : in out Plain_Square_Channel) is null;

   function Get_Read_Masks (Ch : Plain_Square_Channel) return Register_Masks;

   procedure Read
     (Ch       : in out Plain_Square_Channel'Class;
      Register : Channel_Register;
      Value    : out Byte);

   procedure Write
     (Ch       : in out Plain_Square_Channel'Class;
      Register : Channel_Register;
      Value    : Byte);

   --  Override for additional Trigger actions on channel extensions
   --  TODO: Re-evaluate signature, we should not know what gets exposed to
   --  extensions or not
   procedure Extended_Trigger (Ch : in out Plain_Square_Channel) is null;

   procedure Square_Step (Ch : in out Plain_Square_Channel; S : out Sample);
   procedure Envelope_Step (Ch : in out Plain_Square_Channel);
   procedure Length_Step (Ch : in out Plain_Square_Channel);

   --  TODO: Useful for SQ1, SQ1, WAVE, but not Noise
   procedure Set_Frequency
     (Ch   : in out Plain_Square_Channel;
      Freq : Frequency_Type);

   --  TODO: Should be for all channels:
   procedure Disable (Ch : in out Plain_Square_Channel);

private

   Channel_Max_Level : constant := 15;
   Channel_Min_Level : constant := -15;

   type Pulse_State_Type is (Pulse_Low, Pulse_High);
   Next_Pulse_State : constant array (Pulse_State_Type) of Pulse_State_Type :=
     (Pulse_High, Pulse_Low);

   type Pulse_Levels_Type is array (Pulse_State_Type) of Sample;
   type Pulse_Cycles_Type is array (Pulse_State_Type) of Natural;

   Envelope_Steps : constant array (Envelope_Direction_Type) of Integer :=
     (-1, 1);

   type Volume_Envelope_State is record
      TMR          : Timer;
      Step         : Integer;
      Start_Volume : Channel_Volume_Type;
   end record;

   --  TODO: Useful for SQ1, SQ1, Noise, but not WAVE (volume ranges differ)
   procedure Set_Volume
     (Ch     : in out Plain_Square_Channel;
      Volume : Channel_Volume_Type);
   procedure Trigger (Ch : in out Plain_Square_Channel);
   procedure Reload_Length
     (Ch : in out Plain_Square_Channel);
   procedure Trigger_Volume_Envelope (Ch : in out Plain_Square_Channel);
   procedure Trigger_Length (Ch : in out Plain_Square_Channel);

   procedure Set_Volume_Envelope
     (Ch : in out Plain_Square_Channel);

   --  TODO: Calculate these constants/double them
   Hi_Duty_Sample_Multiplier : constant array (Duty_Type) of Natural :=
     (Eighth         => 2,
      Quarter        => 4,
      Half           => 8,
      Three_Quarters => 12);
   Lo_Duty_Sample_Multiplier : constant array (Duty_Type) of Natural :=
     (Eighth         => 14,
      Quarter        => 12,
      Half           => 8,
      Three_Quarters => 4);

   type Plain_Square_Channel is new Square_Channel with record
      Level                : Sample;
      Pulse_Levels         : Pulse_Levels_Type;
      Pulse_Cycles         : Pulse_Cycles_Type;
      Pulse_State          : Pulse_State_Type;
      Rem_Pulse_Cycles     : Natural;
      Enabled              : Boolean;
      Length_Timer         : Timer;
      Volume_Envelope : Volume_Envelope_State;
      Volume               : Channel_Volume_Type;
      Duty                 : Duty_Type;
      Read_Masks           : Register_Masks;
   end record;

end Gade.Audio.Square.Plain;
