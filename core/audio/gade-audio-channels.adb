with Ada.Text_IO; use Ada.Text_IO;

package body Gade.Audio.Channels is

   procedure Create
     (Channel : out Audio_Channel;
      Audio   : Audio_Type)
   is
   begin
      Channel.Audio := Audio;
   end Create;

   procedure Reset (Channel : out Audio_Channel) is
   begin
      Channel.Powered := True;
   end Reset;

   procedure Turn_On (Channel : in out Audio_Channel) is
   begin
      Channel.Powered := True;
   end Turn_On;

   procedure Turn_Off (Channel : in out Audio_Channel) is
   begin
      Channel.Powered := False;
   end Turn_Off;

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
      if Channel.Powered or Register = NRx1 then
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

   package body Base is
      overriding
      procedure Create
        (Channel : out Base_Audio_Channel;
         Audio   : Audio_Type)
      is
      begin
         Audio_Channel (Channel).Create (Audio);
         Setup (Channel.Length_Timer);
      end Create;

      overriding
      procedure Reset (Channel : out Base_Audio_Channel) is
      begin
         Audio_Channel (Channel).Reset;
         Channel.Enabled := False;
         Channel.Length_Enabled := False;
         Channel.Level := 0;
         Setup (Channel.Sample_Timer);
      end Reset;

      overriding
      procedure Turn_Off (Channel : in out Base_Audio_Channel) is
      begin
         Audio_Channel (Channel).Turn_Off;
         Channel.Length_Enabled := False;
         Channel.Level := 0;
         Channel.Length_Timer.Pause;
         Setup (Channel.Sample_Timer);
      end Turn_Off;

      overriding
      procedure Turn_On (Channel : in out Base_Audio_Channel) is
      begin
         Put_Line (Base_Audio_Channel'Class (Channel).Name & " - Turn On (LE: " & Channel.Length_Enabled'Img &
                     " Rem:" & Channel.Length_Timer.Ticks_Remaining'Img & ")");
         Audio_Channel (Channel).Turn_On;
         if Channel.Length_Enabled then
            Put_Line (Base_Audio_Channel'Class (Channel).Name & " - Resume LT");
            Channel.Length_Timer.Resume;
         end if;
         Channel.Enabled := False;
         Channel.Level := 0;
         Setup (Channel.Sample_Timer);
      end Turn_On;

      procedure Step_Sample (Channel : in out Base_Audio_Channel) is
         New_Sample_Level : Sample;
         New_Level_Time   : Positive;
      begin
         --  Could maybe find a way to avoid a dynamic dispatch here for
         --  performance. Inlinable generic function?
         Base_Audio_Channel'Class (Channel).Next_Sample_Level
           (New_Sample_Level, New_Level_Time);
         Channel.Level := New_Sample_Level;
         Start (Channel.Sample_Timer, New_Level_Time);
      end Step_Sample;

      overriding
      procedure Next_Sample
        (Channel : in out Base_Audio_Channel;
         S       : out Sample)
      is
         procedure Tick_Notify_Sample_Step is new Tick_Notify
           (Observer_Type => Base_Audio_Channel,
            Finished      => Step_Sample);
      begin
         S := Channel.Level;
         --  TODO: Can probably avoid this IF by just disabling the sample timer
         if Channel.Enabled then
            --  Put_Line ("Tick_Notify_Sample_Step");
            Tick_Notify_Sample_Step (Channel.Sample_Timer, Channel);
         end if;
      end Next_Sample;

      overriding
      function Read_NRx4 (Channel : Base_Audio_Channel) return Byte is
         NRx4_Out : NRx4_Common_IO;
      begin
         --  TODO: Just save IO byte and keep it updated?
         NRx4_Out.Length_Enable := Channel.Length_Enabled;
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

         Trigger       : constant Boolean := NRx4_In.Trigger;
         Length_Enable : constant Boolean := NRx4_In.Length_Enable;

         Length_Timer : Timer renames Channel.Length_Timer;

         Extra_Tick : Boolean;

         CE : Boolean;

         --  TODO: Remove
         B : constant Boolean := Current_Frame_Sequencer_Step (Channel.Audio) in Lengh_Steps;
      begin
         if not Channel.Powered then return; end if;

         CE := Base_Audio_Channel'Class (Channel).Can_Enable;
         Put_Line (Base_Audio_Channel'Class (Channel).Name & " - Write_NRx4" & Value'Img &
                     " Timer Rem:" & Length_Timer.Ticks_Remaining'Img &
                     " Length was enabled:" & Channel.Length_Enabled'Img &
                     " Length timer was enabled:" & Channel.Length_Timer.Enabled'Img &
                     " Extra clock FSeq state:" & B'Img & " CE " & CE'Img);

         --  The timer will stop ticking once it reaches 0, but the length
         --  enable flag will remain on. Need to check timer state and not previous
         --  flag state.
         Extra_Tick := not Length_Timer.Enabled and
           Length_Enable and
           Current_Frame_Sequencer_Step (Channel.Audio) in Lengh_Steps;

         Channel.Length_Enabled := Length_Enable;

         if Length_Enable and not Length_Timer.Has_Finished then
            Length_Timer.Resume;

            if Extra_Tick then
               Put_Line ("ENABLING LENGTH IN FIRST HALF OF PERIOD! EXTRA LE TICK! (Rem:" &
                           Length_Timer.Ticks_Remaining'Img & ")");
               Length_Timer.Tick;
            end if;

            if Length_Timer.Has_Finished and Channel.Enabled and not Trigger
            then
               Length_Triggered_Disable (Channel); --  Disables channel
            end if;
         end if;

         if Length_Timer.Has_Finished or not Length_Enable then
            Length_Timer.Pause;
         end if;

         --  TODO: Try to make this cleaner
         if Trigger and Length_Timer.Has_Finished then
            Put_Line (Base_Audio_Channel'Class (Channel).Name & " - 0 length reset to max (1)");
            Length_Timer.Setup (Length_Max);
         end if;

         if Trigger and Length_Enable then
            Length_Timer.Resume;

            if Extra_Tick then
               Put_Line ("ENABLING LENGTH IN FIRST HALF OF PERIOD! EXTRA TR TICK! (Rem:" &
                           Length_Timer.Ticks_Remaining'Img & ")");
               Length_Timer.Tick; --  TODO: Handle underflow?
            end if;

            if Channel.Length_Timer.Has_Finished then
               Put_Line (Base_Audio_Channel'Class (Channel).Name & " - 0 length reset to max (2)");
               Length_Timer.Start (Length_Max);
            end if;
         end if;

         if Trigger and Base_Audio_Channel'Class (Channel).Can_Enable then
            Channel.Enabled := True;
            --  We don't know how long the next sample will be yet, fetch next
            --  sample in the following tick:
            Start (Channel.Sample_Timer, 1);
            Put_Line (Base_Audio_Channel'Class (Channel).Name & " - LE: " &
                        Channel.Length_Enabled'Img & ' ' & Length_Timer.Enabled'Img);
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
      end Reload_Length;

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
      function Enabled (Channel : Base_Audio_Channel) return Boolean is
      begin
         Put_Line (Base_Audio_Channel'Class (Channel).Name &
                     "Enabled? " & Channel.Enabled'Img &
                     " - LT:" & Channel.Length_Timer.Ticks_Remaining'Img);
         return Channel.Enabled;
      end Enabled;

      procedure Length_Triggered_Disable (Channel : in out Base_Audio_Channel) is
      begin
         --  Put_Line (Base_Audio_Channel'Class (Channel).Name & " - Length Timer Stopped");
         --  Pause (Channel.Length_Timer); --  TODO: Do this as part of the timer
         --  Put_Line ("Notify_Length_Step");
         if Channel.Length_Enabled then -- TODO: Unsure if needed
            --  Disabled channel should still clock length
            --  Length counter reaching 0 does NOT disable the length enable flag?
            --  Channel.Length_Enabled := False;
            Put_Line (Base_Audio_Channel'Class (Channel).Name & " - Length triggered disable");
            Base_Audio_Channel'Class (Channel).Disable;
         end if;
         --  end if;
      end Length_Triggered_Disable;

      overriding
      procedure Tick_Length (Channel : in out Base_Audio_Channel) is
         procedure Tick_Notify_Length_Step is new Tick_Notify
           (Observer_Type => Base_Audio_Channel,
            Finished      => Length_Triggered_Disable);
      begin
         if Base_Audio_Channel'Class (Channel).Name = "Square 1" then
            Put_Line ("Tick_Length " & Channel.Length_Timer.Ticks_Remaining'Img);
         end if;
         Tick_Notify_Length_Step (Channel.Length_Timer, Channel);
         --  Put_Line ("Length tick" & Ticks_Remaining (Channel.Length_Timer)'Img);
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

      overriding
      procedure Turn_Off (Channel : in out Channel_With_Frequency) is
      begin
         Base_Channel (Channel).Turn_Off;
         Channel.Frequency_In.NRx3 := 0;
      end Turn_Off;

   end Frequency_Mixin;

end Gade.Audio.Channels;
