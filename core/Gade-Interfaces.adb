with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with Gade.GB;           use Gade.GB;
with Gade.Input_Reader; use Gade.Input_Reader;
with Gade.Video_Buffer; use Gade.Video_Buffer;
with Gade.Dev.CPU.Instructions.Exec;        use Gade.Dev.CPU.Instructions.Exec;
with Gade.Dev.Interrupts; use Gade.Dev.Interrupts;
with Gade.Dev.Display;
with Gade.Cart;

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
      Gade.Cart.Load_ROM
        (G.GB.External_ROM,
         G.GB.External_RAM,
         Path);
   end Load_ROM;

   procedure Set_Input_Reader
     (G      : Gade_Type;
      Reader : Gade.Input_Reader.Input_Reader_Access) is
   begin
      G.GB.Joypad.Set_Input_Reader (Reader);
   end Set_Input_Reader;

   procedure Next_Frame
     (G     : Gade_Type;
      Video : Gade.Video_Buffer.RGB32_Display_Buffer_Access) is
      Frame_Finished : Boolean := False;
      Instruction_Cycles, Interrupt_Cycles : Natural;
   begin
      Interrupt_Cycles := 0;
      while not Frame_Finished loop
         Instruction_Cycles := 1;
         if not G.GB.CPU.Halted then
            Gade.Dev.CPU.Instructions.Exec.Execute
              (G.GB.CPU, G.GB, Instruction_Cycles);
         end if;
         Report_Cycles (G.GB, Video, Instruction_Cycles);
         Gade.Dev.Interrupts.Service_Interrupts (G.GB, Interrupt_Cycles);
         if Interrupt_Cycles > 0 then
            Report_Cycles (G.GB, Video, Interrupt_Cycles);
         end if;
         Gade.Dev.Display.Check_Frame_Finished (G.GB.Display, Frame_Finished);
      end loop;
   end Next_Frame;

   procedure Finalize (G : in out Gade_Type) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Object => Opaque_Gade_Type, Name => Gade_Type);
   begin
      G.GB.External_RAM.Set_Enabled (False);
      Put_Line ("Finalize");
      Free (G);
   end Finalize;

end Gade.Interfaces;
