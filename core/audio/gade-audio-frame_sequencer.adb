package body Gade.Audio.Frame_Sequencer is

   procedure Create (Frame_Sequencer : out Audio_Frame_Sequencer;
                     Square_1        : not null Sweeping_Square_Channel_Access;
                     Square_2        : not null Square_Channel_Access;
                     Wave            : not null Wave_Channel_Access;
                     Noise           : not null Noise_Channel_Access) is
   begin
      Frame_Sequencer.Square_1 := Square_1;
      Frame_Sequencer.Square_2 := Square_2;
      Frame_Sequencer.Wave := Wave;
      Frame_Sequencer.Noise := Noise;
   end Create;

   procedure Reset (Frame_Sequencer : in out Audio_Frame_Sequencer) is
   begin
      Frame_Sequencer.Turn_Off;
      Frame_Sequencer.Turn_On;
   end Reset;

   procedure Turn_Off (Frame_Sequencer : in out Audio_Frame_Sequencer) is
   begin
      Frame_Sequencer.Timer.Disable;
   end Turn_Off;

   procedure Turn_On (Frame_Sequencer : in out Audio_Frame_Sequencer) is
   begin
      --  Unsure about this:
      --  When powered on, the frame sequencer is reset so that the next step
      --  will be 0. Re-evaluate once mid instruction timings are implemented.
      Frame_Sequencer.Step_Idx := 7;
      Frame_Sequencer.Timer.Start (Samples_Tick);
      Frame_Sequencer.Tick;
   end Turn_On;

   procedure Step_Frame_Sequencer
     (Frame_Sequencer : in out Audio_Frame_Sequencer)
   is
   begin
      Frame_Sequencer.Step_Idx := Frame_Sequencer.Step_Idx + 1;
      case Frame_Sequencer_Steps (Frame_Sequencer.Step_Idx) is
         when Length_Counter =>
            Frame_Sequencer.Square_1.Tick_Length;
            Frame_Sequencer.Square_2.Tick_Length;
            Frame_Sequencer.Wave.Tick_Length;
            Frame_Sequencer.Noise.Tick_Length;
         when Length_Counter_Frequency_Sweep =>
            Frame_Sequencer.Square_1.Tick_Length;
            Frame_Sequencer.Square_2.Tick_Length;
            Frame_Sequencer.Wave.Tick_Length;
            Frame_Sequencer.Noise.Tick_Length;
            Frame_Sequencer.Square_1.Tick_Frequency_Sweep;
         when Volume_Envelope => null;
            Frame_Sequencer.Square_1.Tick_Volume_Envelope;
            Frame_Sequencer.Square_2.Tick_Volume_Envelope;
            Frame_Sequencer.Noise.Tick_Volume_Envelope;
         when None => null;
      end case;
      Frame_Sequencer.Timer.Start (Samples_Tick);
   end Step_Frame_Sequencer;

   procedure Tick (Frame_Sequencer : in out Audio_Frame_Sequencer) is
      procedure Tick_Notify_Sample_Step is new Tick_Notify
        (Observer_Type => Audio_Frame_Sequencer,
         Notify        => Step_Frame_Sequencer);
   begin
      Tick_Notify_Sample_Step (Frame_Sequencer.Timer, Frame_Sequencer);
   end Tick;

   function Current_State (Frame_Sequencer : Audio_Frame_Sequencer)
                          return State
   is
   begin
      return Frame_Sequencer_Steps (Frame_Sequencer.Step_Idx);
   end Current_State;

end Gade.Audio.Frame_Sequencer;
