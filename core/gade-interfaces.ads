limited with Gade.Input;
limited with Gade.Logging;
limited with Gade.Video_Buffer;
limited with Gade.Audio_Buffer;
with Gade.Timing; use Gade.Timing;

package Gade.Interfaces is

   CPU_Clock_Frequency           : constant := 4_194_304; -- Hz (T-Cycles)
   CPU_M_Frequency               : constant := CPU_Clock_Frequency / 4; -- Hz (M-Cycles)
   --  The frontend-facing audio contract is one generated sample per M-cycle.
   CPU_M_Cycles_Per_Audio_Sample : constant M_Cycle_Count := 1;

   --  Timing normalization follow-up:
   --  - CPU instruction table already reports M-cycles.
   --  - APU outer scheduling already runs in M-cycles.
   --  - Timer and RTC still use T-cycles internally by design.
   --  - LCD handlers still use T-cycle timings and are converted at the
   --    display boundary for now.

   type Gade_Type is private;

   --  Create a new emulator instance with default input reader and logger.
   --
   --  Ownership:
   --  - The returned handle is owned by the caller and must be released with
   --    Finalize exactly once.
   --
   --  Error behavior:
   --  - Any allocation/runtime initialization exception propagates to caller.
   procedure Create (G : out Gade_Type);

   --  Create a new emulator instance with optional injected interfaces.
   --
   --  Nullability:
   --  - Reader may be null (treated as "no input pressed").
   --  - Logger may be null (Default_Logger is used).
   --
   --  Ownership:
   --  - The caller retains ownership of Reader and Logger.
   --  - Reader and Logger must remain valid while attached to this instance.
   --  - The returned handle is owned by the caller and must be released with
   --    Finalize exactly once.
   --
   --  Error behavior:
   --  - Any allocation/runtime initialization exception propagates to caller.
   procedure Create
     (G      : out Gade_Type;
      Reader : Gade.Input.Reader_Access;
      Logger : Gade.Logging.Logger_Access);

   --  Reset emulator state (CPU, memory-mapped devices, and loaded cartridge
   --  runtime state) to power-on defaults.
   --
   --  Preconditions:
   --  - G must be a non-null handle previously returned by Create and not
   --    finalized yet.
   --
   --  Error behavior:
   --  - Invalid handles (for example null/dangling) raise runtime access-check
   --    exceptions.
   procedure Reset (G : Gade_Type);

   --  Load a ROM file and attach its cartridge to the emulator instance.
   --
   --  Preconditions:
   --  - G must be a non-null handle previously returned by Create and not
   --    finalized yet.
   --  - Path must designate a readable ROM file.
   --
   --  Ownership:
   --  - Path content is copied/loaded immediately; caller retains ownership of
   --    Path storage.
   --
   --  Error behavior:
   --  - File I/O and ROM decoding exceptions propagate to caller.
   procedure Load_ROM (G : Gade_Type; Path : String);

   --  Set or replace the input reader used by the joypad device.
   --
   --  Nullability:
   --  - Reader may be null to detach custom input and read neutral input.
   --
   --  Preconditions:
   --  - G must be a non-null handle previously returned by Create and not
   --    finalized yet.
   --
   --  Ownership:
   --  - The caller retains ownership of Reader and must keep it valid while
   --    attached.
   --
   --  Error behavior:
   --  - Invalid handles (for example null/dangling) raise runtime access-check
   --    exceptions.
   procedure Set_Input_Reader (G : Gade_Type; Reader : Gade.Input.Reader_Access);

   --  Execute emulation until a frame is completed or Requested_Samples are
   --  generated, whichever happens first.
   --
   --  Preconditions:
   --  - G must be a non-null handle previously returned by Create and not
   --    finalized yet.
   --  - Video must be non-null and designate a full
   --    Gade.Video_Buffer.RGB32_Display_Buffer.
   --  - Audio must be non-null and designate a full
   --    Gade.Audio_Buffer.Frame_Audio_Buffer.
   --
   --  Buffer size expectations:
   --  - Video buffer dimensions are fixed by Display_Width/Display_Height.
   --  - Audio buffer capacity is fixed by Maximum_Samples.
   --  - Generated_Samples may overshoot Requested_Samples by up to 4.
   --  - The implementation may still write up to Maximum_Samples due to audio
   --    frame flushing behavior; callers should always provide full capacity.
   --
   --  Error behavior:
   --  - Invalid handles or null buffers raise runtime access-check exceptions.
   procedure Run_For
     (G                 : Gade_Type;
      --  Number of audio samples requested at the APU rate (1 sample/M-cycle).
      Requested_Samples : Positive;
      --  Number of generated audio samples at the APU rate.
      --  Can overshoot up to 4 samples from Requested_Samples.
      Generated_Samples : out Natural;
      Video             : Gade.Video_Buffer.RGB32_Display_Buffer_Access;
      Audio             : Gade.Audio_Buffer.Audio_Buffer_Access;
      Frame_Finished    : out Boolean);

   function Debug_State (G : Gade_Type) return String;

   --  Persist cartridge RAM (when applicable) and release emulator resources.
   --
   --  Preconditions:
   --  - G must be a non-null handle previously returned by Create and not yet
   --    finalized.
   --
   --  Ownership:
   --  - Releases resources owned by G and sets G to null.
   --  - Does not free caller-owned Reader/Logger implementations.
   --
   --  Error behavior:
   --  - Save I/O exceptions propagate to caller.
   --  - Calling Finalize with a null/invalid handle raises runtime
   --    access-check exceptions.
   procedure Finalize (G : in out Gade_Type);

private

   type Opaque_Gade_Type;
   type Gade_Type is access Opaque_Gade_Type;
   pragma Convention (C, Gade_Type);

end Gade.Interfaces;
