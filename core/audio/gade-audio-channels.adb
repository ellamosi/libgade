with Ada.Text_IO; use Ada.Text_IO;

package body Gade.Audio.Channels is

   procedure Reset (Channel : out Audio_Channel) is
   begin
      Channel.Powered := True;
   end Reset;

   procedure Turn_On (Channel : in out Audio_Channel) is
   begin
      Channel.Powered := True;
   end Turn_On;

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
      if Channel.Powered then
         case Register is
         when NRx0 => Channel.Write_NRx0 (Value);
         when NRx1 => Channel.Write_NRx1 (Value);
         when NRx2 => Channel.Write_NRx2 (Value);
         when NRx3 => Channel.Write_NRx3 (Value);
         when NRx4 => Channel.Write_NRx4 (Value);
         end case;
      end if;
   end Write;

   function Read_Blank (Channel : Audio_Channel) return Byte is
      pragma Unreferenced (Channel);
   begin
      return Blank_Value;
   end Read_Blank;

   procedure Turn_Off (Channel : in out Audio_Channel) is
   begin
      Audio_Channel'Class (Channel).Disable;
      Audio_Channel'Class (Channel).Reset;
      Channel.Powered := False;
   end Turn_Off;

   package body Base is
      overriding
      procedure Reset (Channel : out Base_Audio_Channel) is
      begin
         Audio_Channel (Channel).Reset;
         Channel.Enabled := False;
         Channel.Powered := True;
         Channel.Length_Enabled := False;
         Channel.Level := 0;
         Setup (Channel.Length_Timer);
         Setup (Channel.Sample_Timer);
      end Reset;

      overriding
      procedure Next_Sample
        (Channel : in out Base_Audio_Channel;
         S       : out Sample)
      is
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
         --  TODO: Just save IO byte and keep it updated?
         NRx4_Out.Length_Enable := Enabled (Channel.Length_Timer);
         return To_Byte (NRx4_Out) or NRx4_Length_Enable_Mask;
      end Read_NRx4;

      overriding
      procedure Write_NRx1
        (Channel : in out Base_Audio_Channel;
         Value   : Byte)
      is
--           Length : constant Natural :=
--             Length_Max - Natural (Value and NRx1_Length_Mask);
         Length : constant Natural := Natural (Value and NRx1_Length_Mask);
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
         Put_Line (Base_Audio_Channel'Class (Channel).Name & " - Write_NRx4" & Value'Img &
                   " Timer Finished: " & Has_Finished (Channel.Length_Timer)'Img);

         --  Length Enable gets set regardless of trigger:
         --  Channel.Length_Enabled := NRx4_In.Length_Enable

         Channel.Length_Enabled := NRx4_In.Length_Enable;

         if Channel.Length_Enabled then
            Resume (Channel.Length_Timer);
         else
            --  Put_Line (Base_Audio_Channel'Class (Channel).Name & " - Length timer disabled");
            Pause (Channel.Length_Timer);
         end if;
         if NRx4_In.Trigger and Base_Audio_Channel'Class (Channel).Can_Enable then
            Channel.Enabled := True;

            if Has_Finished (Channel.Length_Timer) and Channel.Length_Enabled then
               Put_Line ("Trigger Convert length to max (started)");
               Start (Channel.Length_Timer, Length_Max);
            elsif Has_Finished (Channel.Length_Timer) and not Channel.Length_Enabled then
               Put_Line ("Trigger Convert length to max (paused)");
               Setup (Channel.Length_Timer, Length_Max);
            end if;
            --  We don't know how long the next sample will be yet, fetch next
            --  sample in the following tick:
            Start (Channel.Sample_Timer, 1);
            Put_Line (Base_Audio_Channel'Class (Channel).Name & " - LE: " &
                        Channel.Length_Enabled'Img & ' ' & Enabled (Channel.Length_Timer)'Img);
            Base_Audio_Channel'Class (Channel).Trigger;
         end if;
      end Write_NRx4;

      procedure Reload_Length
        (Channel : in out Base_Audio_Channel;
         Length  : Natural) is
      begin
         Channel.Length := Length_Max - Length; --(if Length = 0 then Length_Max else Length_Max - Length);
         if Channel.Length_Enabled then
            Put_Line (Base_Audio_Channel'Class (Channel).Name & " - Reload_Length (running)" & Channel.Length'Img);
            Start (Channel.Length_Timer, Channel.Length);
         else
            Put_Line (Base_Audio_Channel'Class (Channel).Name & " - Reload_Length (paused)" & Channel.Length'Img);
            Setup (Channel.Length_Timer, Channel.Length);
         end if;

--           if Enabled (Channel.Length_Timer) then
--              Start (Channel.Length_Timer, Length);
--           else
--              --  Put_Line ("Skipping Length Reload");
--              Setup (Channel.Length_Timer, Length);
--              --  Stop (Channel.Length_Timer);
--           end if;
      end Reload_Length;

      function Enabled (Channel : Base_Audio_Channel) return Boolean is
      begin
         return Channel.Enabled;
      end Enabled;

      overriding
      procedure Disable (Channel : in out Base_Audio_Channel) is
      begin
         Put_Line (Base_Audio_Channel'Class (Channel).Name & " - Disabled");
         Channel.Level := 0;

         Channel.Enabled := False;
         --  Maybe pause instead
         --  Stop (Channel.Length_Timer);
         Stop (Channel.Sample_Timer);
      end Disable;

      overriding
      function Length_Enabled (Channel : Base_Audio_Channel) return Boolean is
      begin
         Put_Line (Base_Audio_Channel'Class (Channel).Name & " - Length enable check: " &
                     Enabled (Channel.Length_Timer)'Img &
                   " Lenfth Ticks Left:" & Ticks_Remaining (Channel.Length_Timer)'Img);
         --  return Enabled (Channel.Length_Timer);
         return Channel.Enabled;
      end Length_Enabled;

      procedure Step_Length (Channel : in out Base_Audio_Channel) is
      begin
         --  Put_Line (Base_Audio_Channel'Class (Channel).Name & " - Length Timer Stopped");
         Pause (Channel.Length_Timer);
         --  Channel.Length_Enabled := False;
         --  if Channel.Length_Enabled then
            Put_Line (Base_Audio_Channel'Class (Channel).Name & " - Length triggered disable");
            Base_Audio_Channel'Class (Channel).Disable;
         --  end if;
      end Step_Length;

      overriding
      procedure Tick_Length (Channel : in out Base_Audio_Channel) is
         procedure Tick_Notify_Length_Step is new Tick_Notify_Repeatable
           (Observer_Type => Base_Audio_Channel,
            Finished      => Step_Length);
      begin
         --  Put_Line ("Tick_Length Enabled" & Enabled (Channel.Length_Timer)'Img);
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
