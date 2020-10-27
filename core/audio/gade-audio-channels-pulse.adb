with Ada.Text_IO; use Ada.Text_IO;

package body Gade.Audio.Channels.Pulse is

   overriding
   procedure Disable
     (Channel : in out Pulse_Channel;
      Mode    : Disable_Mode)
   is
      Envelope : Volume_Envelope_Type renames Channel.Volume_Envelope;
   begin
      Parent (Channel).Disable (Mode);
      Channel.Set_Volume (0);
      Disable (Channel.Volume_Envelope);
      if Mode = APU_Power_Off then
         Channel.NRx2 := 16#00#;
         Setup (Envelope.Timer);
         Envelope.Step := 0;
         Envelope.Period := 0;
         Envelope.Current_Volume := 0;
         Envelope.Initial_Volume := 0;
         Envelope.Direction := Down;
      end if;
   end Disable;

   overriding
   function Read_NRx2 (Channel : Pulse_Channel) return Byte is
   begin
      return Channel.NRx2; --  Does not require any masking
   end Read_NRx2;

   overriding
   procedure Write_NRx2 (Channel : in out Pulse_Channel; Value : Byte) is
      NRx2_In : constant NRx2_Volume_Envelope_IO :=
        To_NRx2_Volume_Envelope_IO (Value);
   begin
      Channel.NRx2 := Value or NRx2_Volume_Envelope_Mask;

      Setup
        (Channel.Volume_Envelope,
         Volume    => NRx2_In.Volume,
         Direction => NRx2_In.Direction,
         Period    => NRx2_In.Period);

      Put_Line ("Set up Vol Env (Vol" & NRx2_In.Volume'Img &
                  ", Dir " & NRx2_In.Direction'Img &
                  ", Per" & NRx2_In.Period'Img & ")");

      Channel.Set_Volume (Natural (NRx2_In.Volume));

      --  TODO: Reuse this expression
      if Channel.Volume_Envelope.Initial_Volume = Volume_Min_Level and
        Channel.Volume_Envelope.Direction = Down
      then
         Put_Line ("Powering OFF Wave Channel");
         Pulse_Channel'Class (Channel).Disable (DAC_Power_Off);
      end if;
   end Write_NRx2;

   overriding
   procedure Trigger (Channel : in out Pulse_Channel) is
      Envelope : Volume_Envelope_Type renames Channel.Volume_Envelope;
   begin
      Parent (Channel).Trigger;

      Trigger (Envelope);
      if Silent (Envelope) then
         --  Disable the channel altogether for better performance, which is
         --  probably incorrect as writing the volume can have side effects
         --  without re-triggering the channel. Deal with that later.
         --  OR NOT!
         --  Pulse_Channel'Class (Channel).Disable;
         null;
      else
         Set_Volume (Channel, Envelope.Current_Volume);
      end if;
   end Trigger;

   overriding
   function Can_Enable (Channel : Pulse_Channel) return Boolean is
   begin
      return Channel.Volume_Envelope.Initial_Volume /= Volume_Min_Level or
        Channel.Volume_Envelope.Direction = Up;
   end Can_Enable;

   procedure Set_Volume (Channel : in out Pulse_Channel; Volume : Natural) is
      Volume_Sample : constant Sample := Sample (Volume);
   begin
      --  Put_Line ("Set_Volume" & Volume'Img);
      Channel.Volume_Envelope.Current_Volume := Volume;
      Channel.Pulse_Levels := (-Volume_Sample, Volume_Sample);
   end Set_Volume;

   procedure Step_Volume_Envelope (Channel : in out Pulse_Channel) is
      Envelope : Volume_Envelope_Type renames Channel.Volume_Envelope;
   begin
      Step (Envelope);
      Set_Volume (Channel, Envelope.Current_Volume);
   end Step_Volume_Envelope;

   procedure Tick_Volume_Envelope (Channel : in out Pulse_Channel) is
      procedure Tick_Notify_Volume_Envelope_Step is new Tick_Notify_Repeatable
        (Observer_Type => Pulse_Channel,
         Finished      => Step_Volume_Envelope);
   begin
      --  Put_Line ("Tick_Volume_Envelope, Rem:" & Channel.Volume_Envelope.Timer.Ticks_Remaining'Img);
      Tick_Notify_Volume_Envelope_Step (Channel.Volume_Envelope.Timer, Channel);
   end Tick_Volume_Envelope;

   procedure Setup
     (Envelope  : in out Volume_Envelope_Type;
      Volume    : Envelope_Volume;
      Direction : Envelope_Direction;
      Period    : Envelope_Period)
   is
   begin
      Envelope.Initial_Volume := Natural (Volume);
      Envelope.Direction      := Direction;
      Envelope.Step           := Steps (Envelope.Direction);
      Envelope.Period         := Period;
--        Put_Line ("Setting up envelope (Direction: " & Direction'Img &
--                    " Period:" & Envelope.Period'Img & " Initial" & Volume'Img &
--                    " Step:" & Envelope.Step'Img & ")");
   end Setup;

   procedure Trigger (Envelope : in out Volume_Envelope_Type) is
      Volume : constant Natural := Envelope.Initial_Volume;
   begin
      Envelope.Current_Volume := Volume;
      if Final_Edge_Volume (Volume, Envelope.Direction) or Envelope.Period /= 0
      then
         Start (Envelope.Timer, Actual_Effect_Periods (Envelope.Period));
      else
         Stop (Envelope.Timer);
      end if;
   end Trigger;

   function Enabled (Envelope : Volume_Envelope_Type) return Boolean is
   begin
      return Enabled (Envelope.Timer);
   end Enabled;

   procedure Disable (Envelope : in out Volume_Envelope_Type) is
   begin
      Stop (Envelope.Timer);
   end Disable;

   function Silent (Envelope : Volume_Envelope_Type) return Boolean is
   begin
      return
        Envelope.Current_Volume = Volume_Min_Level and
        (not Enabled (Envelope.Timer) or Envelope.Direction = Down);
   end Silent;

   procedure Step (Envelope : in out Volume_Envelope_Type) is
   begin
      Envelope.Current_Volume := Envelope.Current_Volume + Envelope.Step;
      Put_Line ("Vol Env Step:" & Envelope.Current_Volume'Img);
      if Edge_Volume (Envelope.Current_Volume) then
         Disable (Envelope);
      else
         Start (Envelope.Timer, Actual_Effect_Periods (Envelope.Period));
      end if;
   end Step;

   function Edge_Volume (Volume : Natural) return Boolean is
   begin
      return Volume = Volume_Max_Level or Volume = Volume_Min_Level;
   end Edge_Volume;

   function Final_Edge_Volume
     (Volume    : Natural;
      Direction : Envelope_Direction)
      return Boolean
   is
   begin
      return Final_Edge_Volumes (Direction) = Volume;
   end Final_Edge_Volume;

   function Current_Volume (Envelope : Volume_Envelope_Type) return Natural is
   begin
      return Envelope.Current_Volume;
   end Current_Volume;

end Gade.Audio.Channels.Pulse;
