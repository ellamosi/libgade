separate (Gade.Audio.Channels)
package body Length_Trigger is

   overriding
   procedure Disable
     (Channel : in out Length_Trigger_Channel;
      Mode    : Disable_Mode)
   is
   begin
      Parent (Channel).Disable (Mode);
      Put_Line (Channel.Name & " - Disabled");
      Channel.Level := 0;
      Channel.Sample_Timer.Pause;
      Channel.Enabled := False;
      if Mode = APU_Power_Off then
         Channel.Length_Enabled := False;
         Channel.Length_Timer.Setup;
      end if;
   end Disable;

   overriding
   procedure Turn_On (Channel : in out Length_Trigger_Channel) is
   begin
      Put_Line (Channel.Name & " - Turn On (LE: " & Channel.Length_Enabled'Img &
                  " Rem:" & Channel.Length_Timer.Ticks_Remaining'Img & ")");
      Parent (Channel).Turn_On;
      if Channel.Length_Enabled then
         Put_Line (Channel.Name & " - Resume LT");
         Channel.Length_Timer.Resume;
      end if;
      Channel.Enabled := False;
      Channel.Level := 0;
      Setup (Channel.Sample_Timer);
   end Turn_On;

   procedure Step_Sample (Channel : in out Length_Trigger_Channel) is
      New_Sample_Level : Sample;
      New_Level_Time   : Positive;
   begin
      --  Could maybe find a way to avoid a dynamic dispatch here for
      --  performance. Inlinable generic function?
      Length_Trigger_Channel'Class (Channel).Next_Sample_Level
        (New_Sample_Level, New_Level_Time);
      Channel.Level := New_Sample_Level;
      Start (Channel.Sample_Timer, New_Level_Time);
   end Step_Sample;

   overriding
   procedure Next_Sample
     (Channel : in out Length_Trigger_Channel;
      S       : out Sample)
   is
      procedure Tick_Notify_Sample_Step is new Tick_Notify
        (Observer_Type => Length_Trigger_Channel,
         Finished      => Step_Sample);
   begin
      S := Channel.Level;
      --  TODO: Can probably avoid this IF by just disabling the sample timer
      --  Put_Line ("NS:" & Channel.Enabled'Img);
      if Channel.Enabled then
         --  Put_Line ("Tick_Notify_Sample_Step");
         Tick_Notify_Sample_Step (Channel.Sample_Timer, Channel);
      end if;
   end Next_Sample;

   overriding
   function Read_NRx4 (Channel : Length_Trigger_Channel) return Byte is
      NRx4_Out : NRx4_Common_IO;
   begin
      --  TODO: Just save IO byte and keep it updated?
      NRx4_Out.Length_Enable := Channel.Length_Enabled;
      return To_Byte (NRx4_Out) or NRx4_Length_Enable_Mask;
   end Read_NRx4;

   overriding
   procedure Write_NRx1
     (Channel : in out Length_Trigger_Channel;
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
     (Channel : in out Length_Trigger_Channel;
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

      CE := Length_Trigger_Channel'Class (Channel).Can_Enable;
      Put_Line (Channel.Name & " - Write_NRx4" & Value'Img &
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
         Put_Line (Channel.Name & " - 0 length reset to max (1)");
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
            Put_Line (Channel.Name & " - 0 length reset to max (2)");
            Length_Timer.Start (Length_Max);
         end if;
      end if;

      if Trigger and Length_Trigger_Channel'Class (Channel).Can_Enable then
         Channel.Enabled := True;
         --  We don't know how long the next sample will be yet, fetch next
         --  sample in the following tick:
         Start (Channel.Sample_Timer, 1);
         Length_Trigger_Channel'Class (Channel).Trigger;
      end if;
   end Write_NRx4;

   procedure Reload_Length
     (Channel : in out Length_Trigger_Channel;
      Length  : Natural) is
   begin
      Channel.Length := Length_Max - Length;
      if Channel.Length_Enabled then
         Start (Channel.Length_Timer, Channel.Length);
      else
         Setup (Channel.Length_Timer, Channel.Length);
      end if;
   end Reload_Length;

   overriding
   function Enabled (Channel : Length_Trigger_Channel) return Boolean is
   begin
      return Channel.Enabled;
   end Enabled;

   procedure Length_Triggered_Disable
     (Channel : in out Length_Trigger_Channel)
   is
   begin
      if Channel.Length_Enabled then -- TODO: Unsure if needed
         --  Disabled channel should still clock length
         Put_Line (Channel.Name & " - Length triggered disable");
         Length_Trigger_Channel'Class (Channel).Disable (Self_Disable);
      end if;
   end Length_Triggered_Disable;

   overriding
   procedure Tick_Length (Channel : in out Length_Trigger_Channel) is
      procedure Tick_Notify_Length_Step is new Tick_Notify
        (Observer_Type => Length_Trigger_Channel,
         Finished      => Length_Triggered_Disable);
   begin
      Tick_Notify_Length_Step (Channel.Length_Timer, Channel);
   end Tick_Length;

end Length_Trigger;
