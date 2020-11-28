# Audio
The GB hardware relies on 4 different simple waveform generators, each of them with their own 4-bit DAC, the output of which are mixed together into an stereo signal through an analog circuit.

Documentation about the audio hardware's behavior was mostly found on [this article](https://gbdev.gg8.se/wiki/articles/Gameboy_sound_hardware) in the GB Dev wiki. It's a highly recommended read to understand the details of the actual hardware. This README focuses on describing how the described features are implemented and the rationale behind their implementation, but expects the reader to be already somewhat familiar with some of the referenced concepts.

This implementation relies on these four basic components:
- Audio type encapsulating the entirety the Audio Processing Unit logic, its IO, forwarding accesses as necessary to any of its su components and APU power states.
- Frame Sequencer
- Mixer
- Channel Implementations

See the following UML diagram to get a sense of how it all fits together. Note that that is not exhaustive and is just meant to give an overview of the general design. Most notably constructors and power state related methods were purposedly ommitted, among others.

![Gade Audio UML Diagram](./doc/Gade-Audio-UML.svg)

## Frame Sequencer
The Frame Sequencer clocks each of the channel's modulation units at 512 Hz (which ones varies on its state machine). Currently relies on a timer that gets ticked on every CPU M-Cycle, but that should happen as part of a Scheduled event once the emulator implements an event scheduler.

## Mixer
The Mixer component fetches the outputs from each of the channels, mixes them together based on per channel Left/Right enable flags and generates stereo samples from it.

In the real hardware, each of the channels feeds a 4-bit value into their DAC. The analog mixing circuitry applies a high-pass filter which will gradually even out the reference silence level. To simplify the implementation of the emulator, each of the channel implementations will output a value in the range -15 to 15 which can then be mixed together without the need for a high pass filter. This should work correctly for the majority of the games, but some special uses of the sound hardware will likely require proper emulation of the low pass filter, such as [Smooth Player](https://eldred.fr/projects/smooth-player).

## Channels
All 4 channels share some behavior, but each of them has some disctinct features or a variation of the common ones. The design attempts to fully re-use the implementations of each of these. The audio channels use 5 IO registers each (NRx0..NRx4), so a common IO interface is defined for them on their root type and the APU handler figures out which register within which channel to access given an address.

Let's take a look into the features supported by each channel and then explain how each of them is implemented on each concrete or abstract channel type:

| Channels/Features      | Square 1    | Square 2    | Wave         | Noise       |
|------------------------|-------------|-------------|--------------|-------------|
| Length Counter         | 6-bit       | 6-bit       | 8-bit        | 6-bit       |
| Volume Envelope        | Yes         | Yes         | No           | Yes         |
| Volume Precision       | 4-bit       | 4-bit       | 2-bit        | 4-bit       |
| Configurable Frequency | Yes         | Yes         | Yes          | No          |
| Frequency Sweep        | Yes         | No          | No           | No          |
| Kind                   | Pulse-based | Pulse-based | Sample-based | Pulse-based |

### Abstract Channel Implementations

#### Audio Channel
Serves as root channel type. Implements little more than the necessary logic to maintain an output level for a given time, as well as the ability to keep track of its DAC power state. Even though all channels implement a Length Counter, that would require making the root type generic, which would make the type instantiations incompatible. Therefore, that is implemented one level further up in the chain. Additionally, it defines per channel register read/write methods so each register access can have a default implementation that can be overriden as necessary.

#### Length Trigger Channel
Adds the Length Counter and Trigger event functionalities. The length counter is based on a generic bit-length, so Square 1/2 and Noise channels can use a 6-bit length counter and the Wave channel can use an 8-bit length counter and the generic implementation takes care of handling the IO accordingly. The Trigger event logic implementation is defined at this level as it's tightly coupled to the behavior of the length counter.

#### Pulse Channel
Represents all the pulse based channels (Square 1/2 and Noise) and implements the Volume Envelope functionality that they all use.

#### Frequency Mixin
Used in all the channels that allow explicit frequency adjustment (Square 1/2 and Wave), handles the IO of the channel's frequency. Implemented as a mixin as it's less of a defining trait than the Pulse/Sample based nature of the channels and had to be used in both branches of the hierarchy tree.

### Concrete Channel Implementations

#### Sweeping Square Channel (Square 1)
Extends the basic Square Channel implementation and relies on `Set_Frequency` to dynamically adjust the channel's frequency.

#### Square Channel (Square 2)
Basic square channel implementation, adds support for Duty Cycle handling and defines the method `Set_Frequency` for its extensions to use.

#### Wave Channel
This is the only sample based channel. It uses its own volume implementation (by shifting right the 4-bit samples). Defines the wave table to play the samples from and handles its IO.

#### Noise Channel
Uses the pseudo random pulse generation by implementation the Linear-feedback shift register and its necessary IO.

## Testsuite
The following sets of test ROMs are used to validate the audio behavior:
- [MBC1 Mooneye Test suite](https://github.com/retrio/gb-test-roms/tree/master/dmg_sound) except the ones requiring accurate mid instruction timings:
  - 09-wave read while on.gb
  - 10-wave trigger while on.gb
  - 12-wave write while on.gb

## Unsupported Features
- [Zombie mode](https://gbdev.gg8.se/wiki/articles/Gameboy_sound_hardware#Obscure_Behavior): Apparently required for Prehistorik Man to sound right
- Analog High-pass filter
- [Wave Channel sample buffer quirks](https://gbdev.gg8.se/wiki/articles/Gameboy_sound_hardware#Obscure_Behavior)
- Accurate mid instruction behavior: Constrained by the CPU implementation not yet supporting it
= [Smooth Player](https://eldred.fr/projects/smooth-player): Probably will require emulating the analog high-pass filter and a few other efforts