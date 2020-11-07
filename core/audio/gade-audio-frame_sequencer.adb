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
      Turn_Off (FS);
      Turn_On (FS);
   end Reset;

   procedure Turn_Off (FS : in out Frame_Sequencer) is
   begin
      FS.Timer.Stop;
   end Turn_Off;

   procedure Turn_On (FS : in out Frame_Sequencer) is
   begin
      --  Unsure about this:
      --  When powered on, the frame sequencer is reset so that the next step
      --  will be 0. Re-evaluate once mid instruction timings are implemented.
      FS.Step_Idx := 7;
      FS.Timer.Start (Samples_Tick);
      FS.Tick;
   end Turn_On;

   procedure Step_Frame_Sequencer (FS : in out Frame_Sequencer) is
   begin
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
      FS.Timer.Start (Samples_Tick);
   end Step_Frame_Sequencer;

   procedure Tick (FS : in out Frame_Sequencer) is
      procedure Tick_Notify_Sample_Step is new Tick_Notify
        (Observer_Type => Frame_Sequencer,
         Notify        => Step_Frame_Sequencer);
   begin
      Tick_Notify_Sample_Step (FS.Timer, FS);
   end Tick;

   function Step (FS : Frame_Sequencer) return Frame_Sequencer_Step is
   begin
      return Frame_Sequencer_Steps (FS.Step_Idx);
   end Step;

end Gade.Audio.Frame_Sequencer;
