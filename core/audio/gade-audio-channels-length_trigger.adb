with Ada.Text_IO; use Ada.Text_IO;

with Gade.Audio.Frame_Sequencer; use Gade.Audio.Frame_Sequencer;

separate (Gade.Audio.Channels)
package body Length_Trigger is

   overriding
   procedure Disable
     (Channel : in out Length_Trigger_Channel;
      Mode    : Disable_Mode)
   is
   begin
      Parent (Channel).Disable (Mode);
      if Mode = APU_Power_Off then
         Channel.Length.Enabled := False;
         Channel.Length.Timer.Setup;
         Channel.NRx4 := NRx4_Length_Enable_Mask;
      end if;
   end Disable;

   overriding
   procedure Turn_On (Channel : in out Length_Trigger_Channel) is
   begin
      Parent (Channel).Turn_On;
      if Channel.Length.Enabled then Channel.Length.Timer.Resume; end if;
   end Turn_On;

   overriding
   function Read_NRx4 (Channel : Length_Trigger_Channel) return Byte is
   begin
      return Channel.NRx4;
   end Read_NRx4;

   overriding
   procedure Write_NRx1
     (Channel : in out Length_Trigger_Channel;
      Value   : Byte)
   is
      Length : Length_Details renames Channel.Length;

      Length_Value : constant Natural := Natural (Value and NRx1_Length_Mask);
   begin
      Length.Value := Length_Max - Length_Value;
      Put_Line ("Length Reload:" & Length.Value'Img & " (LE: " & Length.Enabled'Img & ")");
      if Length.Enabled then
         Length.Timer.Start (Length.Value);
      else
         Length.Timer.Setup (Length.Value);
      end if;
   end Write_NRx1;

   overriding
   procedure Write_NRx4
     (Channel : in out Length_Trigger_Channel;
      Value   : Byte)
   is
      Length : Length_Details renames Channel.Length;

      NRx4_In : constant NRx4_Common_IO := To_NRx4_Common_IO (Value);

      Trigger       : constant Boolean := NRx4_In.Trigger;
      Length_Enable : constant Boolean := NRx4_In.Length_Enable;

      Extra_Tick : Boolean;

      CE : constant Boolean := Channel.DAC_Powered;

      --  TODO: Remove
      B : constant Boolean := Current_Frame_Sequencer_Step (Channel.Audio) in Lengh_Step;
   begin
      Channel.NRx4 := Value or NRx4_Length_Enable_Mask;

      Put_Line (Channel.Name & " - Write_NRx4" & Value'Img &
                  " Timer Rem:" & Length.Timer.Ticks_Remaining'Img &
                  " Length was enabled:" & Length.Enabled'Img &
                  " Length timer was enabled:" & Length.Timer.Enabled'Img &
                  " Extra clock FSeq state:" & B'Img & " CE " & CE'Img);

      --  The timer will stop ticking once it reaches 0, but the length
      --  enable flag will remain on. Need to check timer state and not previous
      --  flag state.
      Extra_Tick := not Length.Timer.Enabled and
        Length_Enable and
        Current_Frame_Sequencer_Step (Channel.Audio) in Lengh_Step;

      Length.Enabled := Length_Enable;

      if Length_Enable and not Length.Timer.Has_Finished then
         Length.Timer.Resume;

         if Extra_Tick then
            Put_Line ("ENABLING LENGTH IN FIRST HALF OF PERIOD! EXTRA LE TICK! (Rem:" &
                        Length.Timer.Ticks_Remaining'Img & ")");
            Length.Timer.Tick;
         end if;

         if Length.Timer.Has_Finished and Channel.Enabled and not Trigger
         then
            Length_Triggered_Disable (Channel); --  Disables channel
         end if;
      end if;

      if Length.Timer.Has_Finished or not Length_Enable then
         Length.Timer.Pause;
      end if;

      --  TODO: Try to make this cleaner
      if Trigger and Length.Timer.Has_Finished then
         Put_Line (Channel.Name & " - 0 length reset to max (1)");
         Length.Timer.Setup (Length_Max);
      end if;

      if Trigger and Length_Enable then
         Length.Timer.Resume;

         if Extra_Tick then
            Put_Line ("ENABLING LENGTH IN FIRST HALF OF PERIOD! EXTRA TR TICK! (Rem:" &
                        Length.Timer.Ticks_Remaining'Img & ")");
            Length.Timer.Tick; --  TODO: Handle underflow?
         end if;

         if Length.Timer.Has_Finished then
            Put_Line (Channel.Name & " - 0 length reset to max (2)");
            Length.Timer.Start (Length_Max);
         end if;
      end if;

      if Trigger and CE then
         Length_Trigger_Channel'Class (Channel).Trigger;
      end if;
   end Write_NRx4;

   procedure Length_Triggered_Disable
     (Channel : in out Length_Trigger_Channel)
   is
   begin
      --  Reminder: Disabled channel should still clock length
      Length_Trigger_Channel'Class (Channel).Disable (Self_Disable);
   end Length_Triggered_Disable;

   overriding
   procedure Tick_Length (Channel : in out Length_Trigger_Channel) is
      procedure Tick_Notify_Length_Step is new Tick_Notify
        (Observer_Type => Length_Trigger_Channel,
         Notify        => Length_Triggered_Disable);
   begin
      Tick_Notify_Length_Step (Channel.Length.Timer, Channel);
   end Tick_Length;

end Length_Trigger;
