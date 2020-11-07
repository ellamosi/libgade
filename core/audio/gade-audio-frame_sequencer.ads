with Gade.Audio.Channels.Pulse.Noise,
     Gade.Audio.Channels.Pulse.Square,
     Gade.Audio.Channels.Pulse.Square.Sweeping,
     Gade.Audio.Channels.Wave;

use Gade.Audio.Channels.Pulse.Noise,
    Gade.Audio.Channels.Pulse.Square,
    Gade.Audio.Channels.Pulse.Square.Sweeping,
    Gade.Audio.Channels.Wave;

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

   type Frame_Sequencer is tagged private;

   procedure Create
     (FS       : out Frame_Sequencer;
      Square_1 : not null Sweeping_Square_Channel_Access;
      Square_2 : not null Square_Channel_Access;
      Wave     : not null Wave_Channel_Access;
      Noise    : not null Noise_Channel_Access);

   procedure Reset (FS : in out Frame_Sequencer);

   procedure Turn_Off (FS : in out Frame_Sequencer);

   procedure Turn_On (FS : in out Frame_Sequencer);

   procedure Tick (FS : in out Frame_Sequencer);

   type Frame_Sequencer_Step is --  TODO: Rename to State, externally at least
     (None, Volume_Envelope, Length_Counter, Length_Counter_Frequency_Sweep);

   subtype Lengh_Step is Frame_Sequencer_Step range
        Length_Counter .. Length_Counter_Frequency_Sweep;

   function Step (FS : Frame_Sequencer) return Frame_Sequencer_Step;

private

   --  The Frame sequencer triggers the volume envelope tick at 64 Hz
   --  So we need to clock it a t 8 times that so it can complete its state
   --  cycle at that rate.
   Frame_Sequencer_Freq : constant := 512; -- Hz
   Samples_Frame_Sequencer_Tick : constant := Samples_Second / Frame_Sequencer_Freq; --

   type Frame_Sequencer_Step_Index is mod 8;

   Frame_Sequencer_Steps : constant array (Frame_Sequencer_Step_Index) of
     Frame_Sequencer_Step :=
       (Length_Counter,
        None,
        Length_Counter_Frequency_Sweep,
        None,
        Length_Counter,
        None,
        Length_Counter_Frequency_Sweep,
        Volume_Envelope);

   type Frame_Sequencer is tagged record
      Square_1 : Sweeping_Square_Channel_Access;
      Square_2 : Square_Channel_Access;
      Wave     : Wave_Channel_Access;
      Noise    : Noise_Channel_Access;

      Step_Idx  : Frame_Sequencer_Step_Index;
      Rem_Ticks : Natural;
      Step      : Natural;
   end record;

end Gade.Audio.Frame_Sequencer;
