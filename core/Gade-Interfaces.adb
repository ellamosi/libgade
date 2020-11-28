with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with Gade.GB;           use Gade.GB;
with Gade.Input_Reader; use Gade.Input_Reader;
with Gade.Audio_Buffer; use Gade.Audio_Buffer;
with Gade.Video_Buffer; use Gade.Video_Buffer;
with Gade.Dev.CPU.Instructions.Exec;        use Gade.Dev.CPU.Instructions.Exec;
with Gade.Dev.Interrupts; use Gade.Dev.Interrupts;
with Gade.Dev.Display;
with Gade.Carts;

package body Gade.Interfaces is

   type Opaque_Gade_Type is record
      GB : Gade.GB.GB_Type;
   end record;

   procedure Create (G : out Gade_Type) is
   begin
      G := new Opaque_Gade_Type;
      G.GB.Create;
   end Create;

   procedure Reset (G : Gade_Type) is
   begin
      G.GB.Reset;
   end Reset;

   procedure Load_ROM
     (G    : Gade_Type;
      Path : String) is
   begin
      G.GB.Cart := Gade.Carts.Load_ROM (Path);
   end Load_ROM;

   procedure Set_Input_Reader
     (G      : Gade_Type;
      Reader : Gade.Input_Reader.Input_Reader_Access) is
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
      Instruction_Cycles, Interrupt_Cycles : Natural;
   begin
      Frame_Finished := False;
      Generated_Samples := 0;
      while not Frame_Finished and Generated_Samples < Requested_Samples loop
         Instruction_Cycles := 4;
         if not G.GB.CPU.Halted then
            Gade.Dev.CPU.Instructions.Exec.Execute
              (G.GB.CPU, G.GB, Instruction_Cycles);
         end if;
         Report_Cycles (G.GB, Video, Audio, Instruction_Cycles);
         Gade.Dev.Interrupts.Service_Interrupts (G.GB, Interrupt_Cycles);
         if Interrupt_Cycles > 0 then
            Report_Cycles (G.GB, Video, Audio, Interrupt_Cycles);
         end if;
         Generated_Samples := Generated_Samples + Instruction_Cycles + Interrupt_Cycles;
         Gade.Dev.Display.Check_Frame_Finished (G.GB.Display, Frame_Finished);
      end loop;
      --  Should really figure out when/how to flush things...
      Report_Frame (G.GB, Audio, Generated_Samples);
   end Run_For;

   procedure Finalize (G : in out Gade_Type) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Object => Opaque_Gade_Type, Name => Gade_Type);
   begin
      G.GB.Cart.Save_RAM;
      Put_Line ("Finalize");
      Free (G);
   end Finalize;

end Gade.Interfaces;
