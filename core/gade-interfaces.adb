with Ada.Unchecked_Deallocation;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with Gade.GB;             use Gade.GB;
with Gade.GB.Memory_Map;  use Gade.GB.Memory_Map;
with Gade.Dev.CPU;        use Gade.Dev.CPU;
with Gade.Input;          use Gade.Input;
with Gade.Audio_Buffer;   use Gade.Audio_Buffer;
with Gade.Video_Buffer;   use Gade.Video_Buffer;
with Gade.Dev.CPU.Exec;   use Gade.Dev.CPU.Exec;
with Gade.Dev.Interrupts; use Gade.Dev.Interrupts;
with Gade.Dev.Display;
with Gade.Carts;
with Gade.Logging;

package body Gade.Interfaces is
   use type Gade.Logging.Logger_Access;

   function Hex_Digit (Value : Natural) return Character;

   function Hex_Byte (Value : Byte) return String;

   function Hex_Word (Value : Word) return String;

   type Opaque_Gade_Type is record
      GB     : Gade.GB.GB_Type;
      Logger : Gade.Logging.Logger_Access;
   end record;

   function Hex_Digit (Value : Natural) return Character is
   begin
      if Value < 10 then
         return Character'Val (Character'Pos ('0') + Value);
      else
         return Character'Val (Character'Pos ('A') + Value - 10);
      end if;
   end Hex_Digit;

   function Hex_Byte (Value : Byte) return String is
      Raw : constant Natural := Natural (Value);
   begin
      return [1 => Hex_Digit (Raw / 16), 2 => Hex_Digit (Raw mod 16)];
   end Hex_Byte;

   function Hex_Word (Value : Word) return String is
      Raw : constant Natural := Natural (Value);
   begin
      return
        [1 => Hex_Digit ((Raw / 16#1000#) mod 16),
         2 => Hex_Digit ((Raw / 16#100#) mod 16),
         3 => Hex_Digit ((Raw / 16#10#) mod 16),
         4 => Hex_Digit (Raw mod 16)];
   end Hex_Word;

   procedure Create (G : out Gade_Type) is
   begin
      Create (G, null, null);
   end Create;

   procedure Create
     (G      : out Gade_Type;
      Reader : Gade.Input.Reader_Access;
      Logger : Gade.Logging.Logger_Access) is
   begin
      G := new Opaque_Gade_Type;
      G.Logger := (if Logger = null then Gade.Logging.Default_Logger else Logger);
      G.GB.Create (G.Logger);
      G.GB.Joypad.Set_Input_Reader (Reader);
   end Create;

   procedure Reset (G : Gade_Type) is
   begin
      G.GB.Reset;
   end Reset;

   procedure Load_ROM (G : Gade_Type; Path : String) is
   begin
      G.GB.Cart := Gade.Carts.Load_ROM (Path, G.Logger);
   end Load_ROM;

   procedure Set_Input_Reader (G : Gade_Type; Reader : Gade.Input.Reader_Access) is
   begin
      G.GB.Joypad.Set_Input_Reader (Reader);
   end Set_Input_Reader;

   procedure Run_For
     (G                 : Gade_Type;
      Requested_Samples : Positive;
      Generated_Samples : out Natural;
      Video             : Gade.Video_Buffer.RGB32_Display_Buffer_Access;
      Audio             : Gade.Audio_Buffer.Audio_Buffer_Access;
      Frame_Finished    : out Boolean)
   is
      Instruction_Cycles, Interrupt_Cycles : M_Cycle_Count;
      Iteration_Cycles                     : M_Cycle_Count;
      Iteration_Samples                    : Natural;
      Generated_Cycles                     : M_Cycle_Count := 0;
   begin
      Frame_Finished := False;
      Generated_Samples := 0;
      pragma Assert (Video /= null);
      pragma Assert (Audio /= null);
      Set_Run_Context (G.GB, Video, Audio);
      begin
         while not Frame_Finished and Generated_Samples < Requested_Samples loop
            Instruction_Cycles := CPU_M_Cycles_Per_Audio_Sample;
            if not G.GB.CPU.Halted then
               Gade.Dev.CPU.Exec.Execute (G.GB.CPU, G.GB, Instruction_Cycles);
            else
               Tick_M_Cycles (G.GB, Instruction_Cycles);
            end if;
            Gade.Dev.Interrupts.Service_Interrupts (G.GB, Interrupt_Cycles);

            --  Keep both units explicit: T-cycles for frame/accounting paths and
            --  audio samples for the frontend-facing output contract.
            Iteration_Cycles := Instruction_Cycles + Interrupt_Cycles;
            Iteration_Samples :=
              Natural (Iteration_Cycles / CPU_M_Cycles_Per_Audio_Sample);

            Generated_Cycles := Generated_Cycles + Iteration_Cycles;
            Generated_Samples := Generated_Samples + Iteration_Samples;
            Gade.Dev.Display.Check_Frame_Finished (G.GB.Display, Frame_Finished);
         end loop;
      exception
         when others =>
            Clear_Run_Context (G.GB);
            raise;
      end;
      Clear_Run_Context (G.GB);
      --  Should really figure out when/how to flush things...
      --  Report_Frame still expects elapsed CPU cycles, not audio samples.
      Report_Frame (G.GB, Audio, Generated_Cycles);
   end Run_For;

   function Debug_State (G : Gade_Type) return String is
      IF_Value   : constant Byte := Read_Byte (G.GB, 16#FF0F#);
      IE_Value   : constant Byte := Read_Byte (G.GB, 16#FFFF#);
      LY_Value   : constant Byte := Read_Byte (G.GB, 16#FF44#);
      LYC_Value  : constant Byte := Read_Byte (G.GB, 16#FF45#);
      STAT_Value : constant Byte := Read_Byte (G.GB, 16#FF41#);
      Stack_0    : constant Byte := Read_Byte (G.GB, G.GB.CPU.Regs.SP + 0);
      Stack_1    : constant Byte := Read_Byte (G.GB, G.GB.CPU.Regs.SP + 1);
      Stack_2    : constant Byte := Read_Byte (G.GB, G.GB.CPU.Regs.SP + 2);
      Stack_3    : constant Byte := Read_Byte (G.GB, G.GB.CPU.Regs.SP + 3);
      IME_State  : constant String :=
        (case G.GB.CPU.IFF is
           when IE_DI         => "DI",
           when IE_EI_Pending => "PENDING",
           when IE_EI         => "EI");
   begin
      return
        "PC=0x"
        & Hex_Word (G.GB.CPU.PC)
        & " SP=0x"
        & Hex_Word (G.GB.CPU.Regs.SP)
        & " IME="
        & IME_State
        & " HALT="
        & (if G.GB.CPU.Halted then "1" else "0")
        & " HALTBUG="
        & (if G.GB.CPU.Halt_Bug then "1" else "0")
        & " IF=0x"
        & Hex_Byte (IF_Value)
        & " IE=0x"
        & Hex_Byte (IE_Value)
        & " LY="
        & Trim (Natural'Image (Natural (LY_Value)), Ada.Strings.Both)
        & " LYC="
        & Trim (Natural'Image (Natural (LYC_Value)), Ada.Strings.Both)
        & " STAT=0x"
        & Hex_Byte (STAT_Value)
        & " IRQS="
        & Trim
            (Natural'Image (G.GB.Interrupt_Service_Counts (VBlank_Interrupt)),
             Ada.Strings.Both)
        & "/"
        & Trim
            (Natural'Image (G.GB.Interrupt_Service_Counts (LCDC_Interrupt)),
             Ada.Strings.Both)
        & "/"
        & Trim
            (Natural'Image (G.GB.Interrupt_Service_Counts (Timer_Interrupt)),
             Ada.Strings.Both)
        & " STACK="
        & Hex_Byte (Stack_0)
        & " "
        & Hex_Byte (Stack_1)
        & " "
        & Hex_Byte (Stack_2)
        & " "
        & Hex_Byte (Stack_3);
   end Debug_State;

   procedure Finalize (G : in out Gade_Type) is
      procedure Free is new
        Ada.Unchecked_Deallocation (Object => Opaque_Gade_Type, Name => Gade_Type);
   begin
      G.GB.Cart.Save_RAM;
      Gade.Logging.Debug (G.Logger, "Finalize");
      Free (G);
   end Finalize;

end Gade.Interfaces;
