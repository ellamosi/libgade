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
      procedure Next_Sample (Channel : in out Base_Audio_Channel; S : out Sample) is
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
      end Next_Sample;

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

      procedure Step_Length (Channel : in out Base_Audio_Channel) is
      begin
         Base_Audio_Channel'Class (Channel).Disable;
      end Step_Length;

      overriding
      procedure Tick_Length (Channel : in out Base_Audio_Channel) is
         procedure Tick_Notify_Length_Step is new Tick_Notify_Repeatable
           (Observer_Type => Base_Audio_Channel,
            Finished      => Step_Length);
      begin
         Tick_Notify_Length_Step (Channel.Length_Timer, Channel);
      end Tick_Length;

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

end Gade.Audio.Channels;
