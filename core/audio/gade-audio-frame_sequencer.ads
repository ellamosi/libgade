with Gade.Audio.Channels.Pulse.Noise,
     Gade.Audio.Channels.Pulse.Square,
     Gade.Audio.Channels.Pulse.Square.Sweeping,
     Gade.Audio.Channels.Wave,
     Gade.Audio.Timers;

use Gade.Audio.Channels.Pulse.Noise,
    Gade.Audio.Channels.Pulse.Square,
    Gade.Audio.Channels.Pulse.Square.Sweeping,
    Gade.Audio.Channels.Wave,
    Gade.Audio.Timers;

--  https://gbdev.gg8.se/wiki/articles/Gameboy_sound_hardware#Frame_Sequencer
--
--  The frame sequencer generates low frequency clocks for the modulation units.
--  It is clocked by a 512 Hz timer.
--
--  Step   Length Ctr  Vol Env     Sweep
--  ---------------------------------------
--  0      Clock       -           -
--  1      -           -           -
--  2      Clock       -           Clock
--  3      -           -           -
--  4      Clock       -           -
--  5      -           -           -
--  6      Clock       -           Clock
--  7      -           Clock       -
--  ---------------------------------------
--  Rate   256 Hz      64 Hz       128 Hz

private package Gade.Audio.Frame_Sequencer is

   type Audio_Frame_Sequencer is tagged private;

   procedure Create (Frame_Sequencer : out Audio_Frame_Sequencer;
                     Square_1        : not null Sweeping_Square_Channel_Access;
                     Square_2        : not null Square_Channel_Access;
                     Wave            : not null Wave_Channel_Access;
                     Noise           : not null Noise_Channel_Access);

   procedure Reset (Frame_Sequencer : in out Audio_Frame_Sequencer);

   procedure Turn_Off (Frame_Sequencer : in out Audio_Frame_Sequencer);

   procedure Turn_On (Frame_Sequencer : in out Audio_Frame_Sequencer);

   procedure Tick (Frame_Sequencer : in out Audio_Frame_Sequencer);

   type State is
     (None,
      Volume_Envelope,
      Length_Counter,
      Length_Counter_Frequency_Sweep);

   subtype Lengh_Step is State range
        Length_Counter .. Length_Counter_Frequency_Sweep;

   function Current_State (Frame_Sequencer : Audio_Frame_Sequencer)
                           return State;

private

   Frequency : constant := 512; -- Hz
   Samples_Tick : constant := Samples_Second / Frequency;

   type Frame_Sequencer_Step_Index is mod 8;

   Frame_Sequencer_Steps : constant array (Frame_Sequencer_Step_Index) of
     State :=
       (Length_Counter,
        None,
        Length_Counter_Frequency_Sweep,
        None,
        Length_Counter,
        None,
        Length_Counter_Frequency_Sweep,
        Volume_Envelope);

   type Audio_Frame_Sequencer is tagged record
      Square_1 : Sweeping_Square_Channel_Access;
      Square_2 : Square_Channel_Access;
      Wave     : Wave_Channel_Access;
      Noise    : Noise_Channel_Access;

      Step_Idx : Frame_Sequencer_Step_Index;
      Timer    : Timer_Type;
   end record;

   procedure Step_Frame_Sequencer
     (Frame_Sequencer : in out Audio_Frame_Sequencer);

end Gade.Audio.Frame_Sequencer;
