package body Gade.Audio.Channels is

   function Read
     (Channel  : Audio_Channel'Class;
      Register : Channel_Register)
      return Byte
   is
   begin
      return
        (case Register is
            when NRx0 => Channel.Read_NRx0,
            when NRx1 => Channel.Read_NRx1,
            when NRx2 => Channel.Read_NRx2,
            when NRx3 => Channel.Read_NRx3,
            when NRx4 => Channel.Read_NRx4);
   end Read;

   procedure Write
     (Channel  : in out Audio_Channel'Class;
      Register : Channel_Register;
      Value    : Byte)
   is
   begin
      case Register is
         when NRx0 => Channel.Write_NRx0 (Value);
         when NRx1 => Channel.Write_NRx1 (Value);
         when NRx2 => Channel.Write_NRx2 (Value);
         when NRx3 => Channel.Write_NRx3 (Value);
         when NRx4 => Channel.Write_NRx4 (Value);
      end case;
   end Write;

   function Read_Blank (Channel : Audio_Channel) return Byte is
      pragma Unreferenced (Channel);
   begin
      return Blank_Value;
   end Read_Blank;

   package body Base is
      overriding
      procedure Reset (Channel : out Base_Audio_Channel) is
      begin
         Channel.Enabled := False;
         Channel.Level := 0;
         Setup (Channel.Length_Timer);
         Setup (Channel.Sample_Timer);
      end Reset;

      overriding
      procedure Step (Channel : in out Base_Audio_Channel; S : out Sample) is
         New_Sample_Level : Sample;
         New_Level_Time   : Positive;
      begin
         S := Channel.Level;
         if Channel.Enabled then
            Tick (Channel.Sample_Timer);
            if Has_Finished (Channel.Sample_Timer) then
               --  Could maybe find a way to avoid a dynamic dispatch here for
               --  performance. Inlinable generic function?
               Base_Audio_Channel'Class (Channel).Next_Sample_Level
                 (New_Sample_Level, New_Level_Time);
               Channel.Level := New_Sample_Level;
               Start (Channel.Sample_Timer, New_Level_Time);
            end if;
         end if;
      end Step;

      overriding
      function Read_NRx4 (Channel : Base_Audio_Channel) return Byte is
         NRx4_Out : NRx4_Common_IO;
      begin
         NRx4_Out.Length_Enable := Enabled (Channel.Length_Timer);
         return To_Byte (NRx4_Out) or NRx4_Length_Enable_Mask;
      end Read_NRx4;

      overriding
      procedure Write_NRx1
        (Channel : in out Base_Audio_Channel;
         Value   : Byte)
      is
         Length : constant Natural :=
           Length_Max - Natural (Value and NRx1_Length_Mask);
      begin
         Channel.Reload_Length (Length);
      end Write_NRx1;

      overriding
      procedure Write_NRx4
        (Channel : in out Base_Audio_Channel;
         Value   : Byte)
      is
         NRx4_In : constant NRx4_Common_IO := To_NRx4_Common_IO (Value);
      begin
         if NRx4_In.Trigger then
            Channel.Enabled := True;
            if NRx4_In.Length_Enable then
               Reset (Channel.Length_Timer);
            end if;
            --  We don't know how long the next sample will be yet, fetch next
            --  sample in the following tick:
            Start (Channel.Sample_Timer, 1);
            Base_Audio_Channel'Class (Channel).Trigger;
         end if;
      end Write_NRx4;

      procedure Reload_Length
        (Channel : in out Base_Audio_Channel;
         Length  : Natural) is
      begin
         if Enabled (Channel.Length_Timer) then
            Start (Channel.Length_Timer, Length);
         else
            Stop (Channel.Length_Timer);
         end if;
      end Reload_Length;

      function Enabled (Channel : Base_Audio_Channel) return Boolean is
      begin
         return Channel.Enabled;
      end Enabled;

      overriding
      procedure Disable (Channel : in out Base_Audio_Channel) is
      begin
         Channel.Level := 0;
         Channel.Enabled := False;
         Stop (Channel.Length_Timer);
         Stop (Channel.Sample_Timer);
      end Disable;

      overriding
      procedure Length_Step (Channel : in out Base_Audio_Channel) is
      begin
         Tick (Channel.Length_Timer);
         if Has_Finished (Channel.Length_Timer) then
            Base_Audio_Channel'Class (Channel).Disable;
         end if;
      end Length_Step;

   end Base;

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

   end Frequency_Mixin;

   package body Volume_Envelope_Mixin is

--        overriding
--        procedure Reset (Channel : out Pulse_Channel) is
--           Envelope : Volume_Envelope_Type renames Channel.Volume_Envelope;
--        begin
--           Base.Base_Audio_Channel (Channel).Reset;
--           Channel.NRx2 := 0;
--           Channel.Pulse_Levels := (0, 0);
--           Setup (Envelope.Timer);
--           Envelope.Step := 0;
--           Envelope.Current_Volume := 0;
--           Envelope.Initial_Volume := 0;
--           Envelope.Direction := Down;
--        end Reset;

      overriding
      function Read_NRx2 (Channel : Channel_With_Volume_Envelope) return Byte is
      begin
         return Channel.NRx2; --  Does not require any masking
      end Read_NRx2;

      overriding
      procedure Write_NRx2
        (Channel : in out Channel_With_Volume_Envelope;
         Value   : Byte)
      is
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
      procedure Trigger (Channel : in out Channel_With_Volume_Envelope) is
         Envelope : Volume_Envelope_Type renames Channel.Volume_Envelope;

         Initial_Volume : constant Natural := Envelope.Initial_Volume;
      begin
         Base_Channel (Channel).Trigger;
         Channel_With_Volume_Envelope'Class (Channel).Set_Volume (Initial_Volume);
         Reset (Envelope.Timer);
         if Silent (Envelope) then
            --  Disable the channel altogether for better performance, which is
            --  probably incorrect as writing the volume can have side effects
            --  without re-triggering the channel. Deal with that later.
            Channel_With_Volume_Envelope'Class (Channel).Disable;
         end if;
      end Trigger;

--        procedure Set_Volume
--          (Channel : in out Channel_With_Volume_Envelope;
--           Volume  : Natural)
--        is
--           Volume_Sample : constant Sample := Sample (Volume);
--        begin
--           --  Put_Line ("Set_Volume" & Volume'Img);
--           Channel.Volume_Envelope.Current_Volume := Volume;
--           Channel.Pulse_Levels := (-Volume_Sample, Volume_Sample);
--        end Set_Volume;

      procedure Volume_Envelope_Step
        (Channel : in out Channel_With_Volume_Envelope)
      is
         Envelope : Volume_Envelope_Type renames Channel.Volume_Envelope;

         New_Volume : Natural;
      begin
         Tick (Envelope.Timer);
         if Has_Finished (Envelope.Timer) then
            New_Volume := Envelope.Current_Volume + Envelope.Step;
            --  Put_Line ("New Envelope Volume" & New_Volume'Img);
            if New_Volume = Volume_Max_Level then
               Channel_With_Volume_Envelope'Class (Channel).Set_Volume (New_Volume);
               Stop (Envelope.Timer);
            elsif New_Volume = Volume_Min_Level then
               --  Disable the channel altogether for better performance, which is
               --  probably incorrect as writing the volume can have side effects
               --  without re-triggering the channel. Deal with that later.
               Channel_With_Volume_Envelope'Class (Channel).Disable;
            else
               Channel_With_Volume_Envelope'Class (Channel).Set_Volume (New_Volume);
               Reset (Envelope.Timer);
            end if;
         end if;
      end Volume_Envelope_Step;

      overriding
      procedure Disable (Channel : in out Channel_With_Volume_Envelope) is
      begin
         Base_Channel (Channel).Disable;
         Channel_With_Volume_Envelope'Class (Channel).Set_Volume (0);
         --  Channel.Set_Volume (0);
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

   end Volume_Envelope_Mixin;

end Gade.Audio.Channels;
