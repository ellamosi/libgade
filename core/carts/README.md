# Cartridges
GB cartridges can contain different hardware that needs to be emulated in order to make the game playable. The most visible example of this would be the GB camera, but less visibly, most of the GB games also include what's called a Memory Bank Controller. These are used to be able to map larger ROM or RAM chips into the relatively small external ROM and external RAM address spaces (32kB and 16kB respectively). Many GB games rely on a battery backed RAM to save their progress. So while it's possible to emulate a small game like Tetris (32kB) without supporting additional hardware, supporting more complex games requires implementing their specific hardware.

Fortunately, many of the games share the same MBC chips. And some of the behaviors for these is similar.

Cart support is probably the most over engineered component of the emulator.
The goals of this design are the following:
- Minimize code duplication between cartridge implementations
- Enforce static checks on address ranges and bank indexes

To achieve these, we make extensive use of [Ada generics](https://en.wikibooks.org/wiki/Ada_Programming/Generics), composition and multiple inheritance (via mixins). A simplified overview of the design's structure follows, some components have been ommitted to simplify the overall diagram. The components involved are further described below.

![Gade Carts UML Diagram](./doc/Gade-Carts-UML.svg)

## Cartridge Types
This is public interface for the rest of the emulator to use cartridges.

#### Cart (Abstract)
Defines the interface for all of the cart types to follow. Mainly the methods to handle IO through the external ROM/RAM address spaces, but in addition it also defines utily method for cartridge save handling and to CPU clock synchronization.

#### Blank
Emulates behavior with no cartridge inserted. It's the cartridge type that the emulator instantiates upon initialization and ould be used to test the bootrom.

### Supported

#### Plain (Max 32kB ROM / 16kB RAM)
Cartridges without controller are supported, including up to 16kB RAM saves even though there is no known game that uses RAM without relying on a bank controller.

#### MBC1 (Max 2MB ROM / 32kB RAM)
Fully supported including RAM saves.

#### MBC2 (Max 256kB ROM / 512x4 bit RAM)
Ues [BGB's 512 Byte file format spec](http://bgb.bircd.org/mbc2save.html) for file save support to prevent ambiguity in regards to endianess type.

#### MBC3 (Max 2MB ROM / 64kB RAM or 128kB RAM for MBC30 / RTC)
Fully supported including its MBC30 (up to 8 RAM banks instead of 4) variant and the RTC (Real Time Clock). It uses [BGB's file format spec](http://bgb.bircd.org/rtcsave.html) to save RAM and RTC data. It's able to read both 32 and 64 bit RTC file formats, although saves will always be performed in 64 bit. The state is synchronized with the CPU clock once per frame, so if emulation does not happen in realtime that's accurately reflected in the RTC behavior.

### Unsupported
The following cartridge types are currently unsupported: MBC5, MBC6, GB Camera, HuC1, HuC3, MM01, MBC1 based multicarts. Aside from MBC5, all of these are very rare and not a priority to implement. MBC5 is used by most GB Color carts and should be trivial to add support for with the current architecture.

## Mixins
Mixins allow the concrete cartrige implementations to pick and choose which components to support without having to re-define the implementation of the Cart's interface methods.

### Banked Memory
Implements the concept of a memory that can be banked.

The number of accessible banks can be defined through generics (their size will also be statically adjusted accordingly). Generally most cart controllers rely on 2x16kB accessible ROM banks and 1x16kB accessible RAM banks, but this is not always the case. MBC6 uses 4x8kB accessible ROM banks.

#### Banked ROM
The ROM implementation of banked memory limits the banks that can be selected to only ROM type banks. This done because:
- Conceptually it makes sense, no bank types other than ROM need to be accessible in the ROM space
- It prevents dynamic dispatching for bank type in one of the most frequently ocurring operations: ROM read accesses. There's still a dynamic dispatch remaining (for the cart type), but preventing that would break the encapsulation of the cartridge implementation and is currently not a bottleneck.

#### Banked RAM
Adds the RAM enable/disable capability to the banked memory implementation.

The RAM implementation can select any kind of bank. This is used for features such as the RTC support, or to handle the specific behavior of the MBC2 RAM. As RAM is accessed far less frequently than the ROM, incurring a dynamic dispatch is not really a concern.

### ROM_RAM
This is just a shortcut to instantiate ROM/RAM in a single declaration, as the majority of cartridge implementations support

### MBC
Implements common behavior shared by MBC style carts. Namely, identifying bank controller commands based on the ROM address being written to, as well as decoding RAM enabling/disabling commands.

## Banks
Bank types help implement the bank switching behaviour of the bank controllers. For example, selecting a bank with a specific index will make the bank instance with the right offset for that index to be accessible, while a different index can expose the bank that provides access to RTC values.

### ROM
Handles ROM access to a specific memory offset.

### RAM
Handles RAM access to a specific memory offset.

### MBC2 RAM
Handles the 4-bit MBC2 RAM, the upper half of the content bytes cannot be read/written to and will always be ones.

### RTC
Handles the RTC registers for the MBC3 clock.

### Blank
Handles an absent RAM chip, it's also used to implement the disabled RAM state.

## Common Components

### ROM
Representation of the actual plain ROM contents, without banking concerns. Provides means to load a ROM file into memory.

### RAM
Representation of the actual plain RAM contents, without banking concerns. Provides means to read and write RAM data files to/from memory.

### RTC
Representation of the actual RTC, without banking concerns. Provides means to save/load its state into a file.

## Testsuite
The following sets of test ROMs are used to validate cartridge behavior:
- [MBC1 Mooneye Test suite](https://github.com/Gekkio/mooneye-gb/tree/master/tests/emulator-only/mbc1) (except multicarts)
- [MBC2 Mooneye Test suite](https://github.com/Gekkio/mooneye-gb/tree/master/tests/emulator-only/mbc2)
