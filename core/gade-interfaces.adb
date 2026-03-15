with Ada.Unchecked_Deallocation;

with Gade.GB;           use Gade.GB;
with Gade.Input; use Gade.Input;
with Gade.Audio_Buffer; use Gade.Audio_Buffer;
with Gade.Video_Buffer; use Gade.Video_Buffer;
with Gade.Dev.CPU.Instructions.Exec;        use Gade.Dev.CPU.Instructions.Exec;
with Gade.Dev.Interrupts; use Gade.Dev.Interrupts;
with Gade.Dev.Display;
with Gade.Carts;
with Gade.Logging;

package body Gade.Interfaces is
   use type Gade.Logging.Logger_Access;

   type Opaque_Gade_Type is record
      GB     : Gade.GB.GB_Type;
      Logger : Gade.Logging.Logger_Access;
   end record;

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

   procedure Load_ROM
     (G    : Gade_Type;
      Path : String) is
   begin
      G.GB.Cart := Gade.Carts.Load_ROM (Path, G.Logger);
   end Load_ROM;

   procedure Set_Input_Reader
     (G      : Gade_Type;
      Reader : Gade.Input.Reader_Access) is
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
      while not Frame_Finished and Generated_Samples < Requested_Samples loop
         Instruction_Cycles := CPU_M_Cycles_Per_Audio_Sample;
         if not G.GB.CPU.Halted then
            Gade.Dev.CPU.Instructions.Exec.Execute
              (G.GB.CPU, G.GB, Instruction_Cycles);
         end if;
         Report_Cycles (G.GB, Video, Audio, Instruction_Cycles);
         Gade.Dev.Interrupts.Service_Interrupts (G.GB, Interrupt_Cycles);
         if Interrupt_Cycles > 0 then
            Report_Cycles (G.GB, Video, Audio, Interrupt_Cycles);
         end if;

         --  Keep both units explicit: T-cycles for frame/accounting paths and
         --  audio samples for the frontend-facing output contract.
         Iteration_Cycles := Instruction_Cycles + Interrupt_Cycles;
         Iteration_Samples := Natural (Iteration_Cycles / CPU_M_Cycles_Per_Audio_Sample);

         Generated_Cycles := Generated_Cycles + Iteration_Cycles;
         Generated_Samples := Generated_Samples + Iteration_Samples;
         Gade.Dev.Display.Check_Frame_Finished (G.GB.Display, Frame_Finished);
      end loop;
      --  Should really figure out when/how to flush things...
      --  Report_Frame still expects elapsed CPU cycles, not audio samples.
      Report_Frame (G.GB, Audio, Generated_Cycles);
   end Run_For;

   procedure Finalize (G : in out Gade_Type) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Object => Opaque_Gade_Type, Name => Gade_Type);
   begin
      G.GB.Cart.Save_RAM;
      Gade.Logging.Debug (G.Logger, "Finalize");
      Free (G);
   end Finalize;

end Gade.Interfaces;
