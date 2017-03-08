# libgade
A GameBoy emulation library in Ada

This is a proof of concept for an interpreting emulator developed in Ada. It's meant to test the language's suitability for a project of this kind, the performance of generated code and cross language interaction. It's not meant to be the go-to emulator to just play GameBoy games.

## Background
This project started off as an homage to [my university's Computer Architecture department](https://www.ac.upc.edu/en?set_language=en). I throughly enjoyed their courses, in which I went from not knowing binary to fully understanding how a current CPU works, all the way down to logic gate level. This inspired me to write an emulator for a system, and looking for a balance between simplicity and size as well as popularity of its software catalog I set my eyes on the venerable GameBoy.

Why Ada? I learned to program in Ada, and I always felt that it was a more reliable candidate for native code compilation than the likes of C. The precise and platform independent representation clauses that the language offers that allow precisely defining the representation of memory mapped hardware was also a factor in the decision. Also, while such emulators exists in plenty of other languages I suspect it's the first one written in Ada.

### State of affairs

#### What works
- Full CPU emulation (Passes Blargg's CPU instruction tests)
- ROM, MBC1, MBC2 cartridge types
- Joypad
- Timer
- Background
- Sprites

#### What does not work
- Sound
- Other cartridge controller types
- Window layer rendering
- Game saves
- Accurate timings

#### Next steps
- Window rendering
- Performance optimizations (GPU rendering)
- Performance optimizations (CPU interpretation)

