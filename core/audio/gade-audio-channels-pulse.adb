--  with Ada.Text_IO; use Ada.Text_IO;

package body Gade.Audio.Channels.Pulse is

   overriding
   procedure Reset (Channel : out Pulse_Channel) is
      Envelope : Volume_Envelope_Type renames Channel.Volume_Envelope;
   begin
      Base.Base_Audio_Channel (Channel).Reset;
      Channel.NRx2 := 0;
      Channel.Pulse_Levels := (0, 0);
      Setup (Envelope.Timer);
      Envelope.Step := 0;
      Envelope.Current_Volume := 0;
      Envelope.Initial_Volume := 0;
      Envelope.Direction := Down;
   end Reset;

   overriding
   function Read_NRx2 (Channel : Pulse_Channel) return Byte is
   begin
      return Channel.NRx2; --  Does not require any masking
   end Read_NRx2;

   overriding
   procedure Write_NRx2 (Channel : in out Pulse_Channel; Value : Byte) is
      Envelope : Volume_Envelope_Type renames Channel.Volume_Envelope;

      NRx2_In : constant NRx2_Volume_Envelope_IO :=
        To_NRx2_Volume_Envelope_IO (Value);
      Volume : constant Natural := Natural (NRx2_In.Volume);

      Period : Natural;
   begin
      Envelope.Initial_Volume := Volume;
      Envelope.Direction      := NRx2_In.Direction;
      --  Envelope.Period         := Actual_Effect_Periods (NRx2_In.Period);
      Period         := Natural (NRx2_In.Period);

      if Period > 0 then
         Envelope.Period := Period;
      --  if Envelope.Period > 0 then
         --  TODO: Treat 0 period as 8
         Setup (Envelope.Timer, Envelope.Period);

         --  TODO: Make this more readable, shouldn't require nesting
         if ((Volume /= Volume_Min_Level and Envelope.Direction = Down) or
             (Volume /= Volume_Max_Level and Envelope.Direction = Up))
         then
            Start (Envelope.Timer);
            Envelope.Step := Steps (Envelope.Direction);
         end if;
      else
         Stop (Envelope.Timer);
      end if;
   end Write_NRx2;

   overriding
   procedure Trigger (Channel : in out Pulse_Channel) is
      Envelope : Volume_Envelope_Type renames Channel.Volume_Envelope;
   begin
      --  Base.Base_Audio_Channel (Ch).Trigger;
      Set_Volume (Channel, Envelope.Initial_Volume);
      Reset (Envelope.Timer);
      if Silent (Envelope) then
         --  Disable the channel altogether for better performance, which is
         --  probably incorrect as writing the volume can have side effects
         --  without re-triggering the channel. Deal with that later.
         Pulse_Channel'Class (Channel).Disable;
      end if;
   end Trigger;

   procedure Set_Volume (Channel : in out Pulse_Channel; Volume : Natural) is
      Volume_Sample : constant Sample := Sample (Volume);
   begin
      --  Put_Line ("Set_Volume" & Volume'Img);
      Channel.Volume_Envelope.Current_Volume := Volume;
      Channel.Pulse_Levels := (-Volume_Sample, Volume_Sample);
   end Set_Volume;

   procedure Volume_Envelope_Step (Channel : in out Pulse_Channel) is
      Envelope : Volume_Envelope_Type renames Channel.Volume_Envelope;

      New_Volume : Natural;
   begin
      Tick (Envelope.Timer);
      if Has_Finished (Envelope.Timer) then
         New_Volume := Envelope.Current_Volume + Envelope.Step;
         --  Put_Line ("New Envelope Volume" & New_Volume'Img);
         if New_Volume = Volume_Max_Level then
            Set_Volume (Channel, New_Volume);
            Stop (Envelope.Timer);
         elsif New_Volume = Volume_Min_Level then
            --  Disable the channel altogether for better performance, which is
            --  probably incorrect as writing the volume can have side effects
            --  without re-triggering the channel. Deal with that later.
            Pulse_Channel'Class (Channel).Disable;
         else
            Set_Volume (Channel, New_Volume);
            Reset (Envelope.Timer);
         end if;
      end if;
   end Volume_Envelope_Step;

   overriding
   procedure Disable (Channel : in out Pulse_Channel) is
   begin
      Base.Base_Audio_Channel (Channel).Disable;
      Channel.Set_Volume (0);
      Disable (Channel.Volume_Envelope);
   end Disable;

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

end Gade.Audio.Channels.Pulse;
