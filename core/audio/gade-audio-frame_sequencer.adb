package body Gade.Audio.Frame_Sequencer is

   procedure Create
     (FS       : out Frame_Sequencer;
      Square_1 : not null Sweeping_Square_Channel_Access;
      Square_2 : not null Square_Channel_Access;
      Wave     : not null Wave_Channel_Access;
      Noise    : not null Noise_Channel_Access)
   is
   begin
      FS.Square_1 := Square_1;
      FS.Square_2 := Square_2;
      FS.Wave := Wave;
      FS.Noise := Noise;
   end Create;

   procedure Reset (FS : in out Frame_Sequencer) is
   begin
      --  Revise values being set equivalent
--        Turn_Off (FS);
--        Turn_On (FS);
      FS.Step_Idx := 0;
      FS.Step := 1;
      FS.Rem_Ticks := Samples_Frame_Sequencer_Tick;
   end Reset;

   procedure Turn_Off (FS : in out Frame_Sequencer) is
   begin
      FS.Step := 0;
   end Turn_Off;

   procedure Turn_On (FS : in out Frame_Sequencer) is
   begin
      --  Unsure about this:
      --  When powered on, the frame sequencer is reset so that the next step
      --  will be 0
      FS.Step_Idx := 7;
      FS.Rem_Ticks := Samples_Frame_Sequencer_Tick;
      FS.Step := 1;
      FS.Tick;
   end Turn_On;

   procedure Tick (FS : in out Frame_Sequencer) is
   begin
      FS.Rem_Ticks := FS.Rem_Ticks - FS.Step;
      if FS.Rem_Ticks = 0 then
         FS.Step_Idx := FS.Step_Idx + 1;
         case Frame_Sequencer_Steps (FS.Step_Idx) is
            when Length_Counter =>
               FS.Square_1.Tick_Length;
               FS.Square_2.Tick_Length;
               FS.Wave.Tick_Length;
               FS.Noise.Tick_Length;
            when Length_Counter_Frequency_Sweep =>
               FS.Square_1.Tick_Length;
               FS.Square_2.Tick_Length;
               FS.Wave.Tick_Length;
               FS.Noise.Tick_Length;
               FS.Square_1.Tick_Frequency_Sweep;
            when Volume_Envelope => null;
               FS.Square_1.Tick_Volume_Envelope;
               FS.Square_2.Tick_Volume_Envelope;
               FS.Noise.Tick_Volume_Envelope;
            when None => null;
         end case;
         FS.Rem_Ticks := Samples_Frame_Sequencer_Tick;
      end if;
   end Tick;

   function Step (FS : Frame_Sequencer) return Frame_Sequencer_Step is
   begin
      return Frame_Sequencer_Steps (FS.Step_Idx);
   end Step;

end Gade.Audio.Frame_Sequencer;
