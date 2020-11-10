package body Gade.Audio.Channels.Pulse is

   NRx2_Volume_Envelope_Mask : constant Byte := 16#00#;

   type NRx2_Volume_Envelope_IO is record
      Volume    : Envelope_Volume;
      Direction : Envelope_Direction;
      Period    : Envelope_Period;
   end record;
   for NRx2_Volume_Envelope_IO use record
      Volume    at 0 range 4 .. 7;
      Direction at 0 range 3 .. 3;
      Period    at 0 range 0 .. 2;
   end record;
   for NRx2_Volume_Envelope_IO'Size use Byte'Size;

   function To_NRx2_Volume_Envelope_IO is new Ada.Unchecked_Conversion
     (Source => Byte,
      Target => NRx2_Volume_Envelope_IO);


   procedure Step_Volume_Envelope (Channel : in out Pulse_Channel);

   function Powers_DAC (NRx2_In : NRx2_Volume_Envelope_IO) return Boolean;

   overriding
   procedure Disable
     (Channel : in out Pulse_Channel;
      Mode    : Disable_Mode)
   is
      Envelope : Volume_Envelope_Details renames Channel.Volume_Envelope;
   begin
      Parent (Channel).Disable (Mode);
      Channel.Set_Volume (0);
      Envelope.Timer.Disable;
      if Mode = APU_Power_Off then
         Channel.NRx2 := 16#00#;
         Envelope.Timer.Disable;
         Envelope.Step := 0;
         Envelope.Period := 0;
         Envelope.Current_Volume := 0;
         Envelope.Initial_Volume := 0;
         Envelope.Direction := Down;
      end if;
   end Disable;

   overriding
   procedure Trigger (Channel : in out Pulse_Channel) is
      Envelope : Volume_Envelope_Details renames Channel.Volume_Envelope;
   begin
      Parent (Channel).Trigger;

      --  https://gbdev.gg8.se/wiki/articles/Gameboy_sound_hardware#Trigger_Event
      --
      --  Channel volume is reloaded from NRx2.
      Envelope.Current_Volume := Envelope.Initial_Volume;
      Set_Volume (Channel, Envelope.Current_Volume);
      --  Volume envelope timer is reloaded with period.
      Envelope.Timer.Start (Actual_Effect_Periods (Envelope.Period));
   end Trigger;

   procedure Set_Volume (Channel : in out Pulse_Channel; Volume : Natural) is
      Volume_Sample : constant Sample := Sample (Volume);
   begin
      Channel.Volume_Envelope.Current_Volume := Volume;
      Channel.Pulse_Levels := (-Volume_Sample, Volume_Sample);
   end Set_Volume;

   procedure Step_Volume_Envelope (Channel : in out Pulse_Channel) is
      Envelope : Volume_Envelope_Details renames Channel.Volume_Envelope;
      Final_Volume : constant Natural := Final_Volumes (Envelope.Direction);
   begin
      --  https://gbdev.gg8.se/wiki/articles/Gameboy_sound_hardware#Volume_Envelope
      --
      --  When the timer generates a clock and the envelope period is not zero,
      --  a new volume is calculated by adding or subtracting (as set by NRx2)
      --  one from the current volume.
      --
      --  If this new volume within the 0 to 15 range, the volume is updated,
      --  otherwise it is left unchanged and no further automatic increments/
      --  decrements are made to the volume until the channel is triggered
      --  again.
      if Final_Volume /= Envelope.Current_Volume and Envelope.Period /= 0 then
         Envelope.Current_Volume := Envelope.Current_Volume + Envelope.Step;
         Set_Volume (Channel, Envelope.Current_Volume);
         Envelope.Timer.Start (Positive (Envelope.Period));
      end if;
   end Step_Volume_Envelope;

   procedure Tick_Volume_Envelope (Channel : in out Pulse_Channel) is
      procedure Tick_Notify_Volume_Envelope_Step is new Tick_Notify
        (Observer_Type => Pulse_Channel,
         Notify        => Step_Volume_Envelope);
   begin
      Tick_Notify_Volume_Envelope_Step (Channel.Volume_Envelope.Timer, Channel);
   end Tick_Volume_Envelope;

   overriding
   function Read_NRx2 (Channel : Pulse_Channel) return Byte is
   begin
      return Channel.NRx2; --  Does not require any masking
   end Read_NRx2;

   overriding
   procedure Write_NRx2 (Channel : in out Pulse_Channel; Value : Byte) is
      NRx2_In : constant NRx2_Volume_Envelope_IO :=
        To_NRx2_Volume_Envelope_IO (Value);
      Envelope : Volume_Envelope_Details renames Channel.Volume_Envelope;
   begin
      Channel.NRx2 := Value or NRx2_Volume_Envelope_Mask;

      Envelope.Initial_Volume := Natural (NRx2_In.Volume);
      Envelope.Direction      := NRx2_In.Direction;
      Envelope.Step           := Steps (Envelope.Direction);
      Envelope.Period         := NRx2_In.Period;

      --  https://gbdev.gg8.se/wiki/articles/Gameboy_sound_hardware#Channel_DAC
      --
      --  DAC power is controlled by the upper 5 bits of NRx2 (top bit of NR30
      --  for wave channel). If these bits are not all clear, the DAC is on,
      --  otherwise it's off and outputs 0 volts. Also, any time the DAC is off
      --  the channel is kept disabled (but turning the DAC back on does NOT
      --  enable the channel).
      Channel.Update_DAC_Power_State (Powers_DAC (NRx2_In));

      --  TODO: Investigate how/if volume should be set here. It's a bit wonky
      --  and probably complex. Prehistorik Man's intro audio probably relies
      --  on this being accurately implemented. Zombie mode maybe?
      --
      --  https://gbdev.gg8.se/wiki/articles/Gameboy_sound_hardware#Obscure_Behavior
   end Write_NRx2;

   function Powers_DAC (NRx2_In : NRx2_Volume_Envelope_IO) return Boolean is
   begin
      return
        NRx2_In.Volume /= 0 or NRx2_In.Direction /= Envelope_Direction'Val (0);
   end Powers_DAC;

end Gade.Audio.Channels.Pulse;
