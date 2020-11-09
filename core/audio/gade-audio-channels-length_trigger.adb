with Gade.Audio.Frame_Sequencer; use Gade.Audio.Frame_Sequencer;

separate (Gade.Audio.Channels)
package body Length_Trigger is

   procedure Length_Triggered_Disable (Channel : in out Length_Trigger_Channel);

   --  TODO: Rename: The frame sequencer doesn't have a "current" step
   --  (externally at least) it's a sequence of "instant" events, so might
   --  as well use a name that reflects that.
   function In_Length_Step (Audio : Audio_Type) return Boolean;

   procedure Tick_Notify_Length_Step is new Tick_Notify
     (Observer_Type => Length_Trigger_Channel,
      Notify        => Length_Triggered_Disable);

   overriding
   procedure Disable
     (Channel : in out Length_Trigger_Channel;
      Mode    : Disable_Mode)
   is
   begin
      Parent (Channel).Disable (Mode);
      if Mode = APU_Power_Off then
         Channel.Length.Enabled := False;
         Channel.Length.Timer.Disable;
         Channel.NRx4 := NRx4_Length_Enable_Mask;
      end if;
   end Disable;

   overriding
   procedure Turn_On (Channel : in out Length_Trigger_Channel) is
   begin
      Parent (Channel).Turn_On;
      if Channel.Length.Enabled then Channel.Length.Timer.Enable; end if;
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
      Input_Value  : constant Natural := Natural (Value and NRx1_Length_Mask);
      Length_Value : constant Positive := Length_Max - Input_Value;
   begin
      Channel.Length.Timer.Reload (Length_Value, Channel.Length.Enabled);
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

      Timer_Was_Enabled : constant Boolean := Length.Timer.Is_Enabled;
      Is_Length_Step    : constant Boolean := In_Length_Step (Channel.Audio);

      --  The timer will stop ticking once it reaches 0, but the length
      --  enable flag will remain on. Need to check timer state and not previous
      --  flag state to determine if the extra tick was necessary.
      Extra_Tick : constant Boolean :=
        not Timer_Was_Enabled and Length_Enable and Is_Length_Step;
   begin
      Channel.NRx4 := Value or NRx4_Length_Enable_Mask;

      Length.Enabled := Length_Enable;

      Length.Timer.Enable (Length_Enable);

      --  https://gbdev.gg8.se/wiki/articles/Gameboy_sound_hardware#Obscure_Behavior
      --
      --  Extra length clocking occurs when writing to NRx4 when the frame
      --  sequencer's next step is one that doesn't clock the length counter. In
      --  this case, if the length counter was PREVIOUSLY disabled and now
      --  enabled and the length counter is not zero, it is decremented. If this
      --  decrement makes it zero and trigger is clear, the channel is disabled.
      if Length_Enable and not Length.Timer.Has_Finished and Extra_Tick then
         Tick_Length (Channel);
      end if;

      --  https://gbdev.gg8.se/wiki/articles/Gameboy_sound_hardware#Trigger_Event
      --
      --  If length counter is zero, it is set to 64 (256 for wave channel).
      if Trigger and Length.Timer.Has_Finished then
         Length.Timer.Reload (Length_Max, Length_Enable);
      end if;

      --  https://gbdev.gg8.se/wiki/articles/Gameboy_sound_hardware#Obscure_Behavior
      --
      --  If a channel is triggered when the frame sequencer's next step is one
      --  that doesn't clock the length counter and the length counter is now
      --  enabled and length is being set to 64 (256 for wave channel) because
      --  it was previously zero, it is set to 63 instead (255 for wave
      --  channel).
      --
      --  @ellamosi: In practice it just seems to be the effect of extra clocking.
      if Trigger and Length_Enable and Extra_Tick then
         Tick_Length (Channel);
      end if;

      if Trigger and Channel.DAC_Powered then
         Length_Trigger_Channel'Class (Channel).Trigger;
      end if;
   end Write_NRx4;

   procedure Length_Triggered_Disable
     (Channel : in out Length_Trigger_Channel)
   is
   begin
      Length_Trigger_Channel'Class (Channel).Disable (Self_Disable);
   end Length_Triggered_Disable;

   overriding
   procedure Tick_Length (Channel : in out Length_Trigger_Channel) is
   begin
      Tick_Notify_Length_Step (Channel.Length.Timer, Channel);
   end Tick_Length;

   function In_Length_Step (Audio : Audio_Type) return Boolean is
   begin
      return Frame_Sequencer_State (Audio) in Lengh_Step;
   end In_Length_Step;

end Length_Trigger;
