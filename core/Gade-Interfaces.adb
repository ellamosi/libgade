with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with Gade.GB;           use Gade.GB;
with Gade.Input_Reader; use Gade.Input_Reader;
with Gade.Video_Buffer; use Gade.Video_Buffer;
with Gade.Dev.CPU.Instructions.Exec;        use Gade.Dev.CPU.Instructions.Exec;
with Gade.Dev.Interrupts; use Gade.Dev.Interrupts;
with Gade.Dev.Display;

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
      G.GB.External_ROM.Load_ROM(Path);
   end Load_ROM;

   procedure Set_Input_Reader
     (G      : Gade_Type;
      Reader : Gade.Input_Reader.Input_Reader_Access) is
   begin
      G.GB.Joypad.Set_Input_Reader(Reader);
   end Set_Input_Reader;

   procedure Next_Frame
     (G     : Gade_Type;
      Video : Gade.Video_Buffer.RGB32_Display_Buffer_Access) is
      Frame_Finished : Boolean := False;
      Cycles : Natural;
   begin
      while not Frame_Finished loop
         Cycles := 1;
         if not G.GB.CPU.Halted then
            Gade.Dev.CPU.Instructions.Exec.Execute(G.GB.CPU, G.GB, Cycles);
         end if;
         for i in 1..Cycles loop
            Report_Cycle(G.GB, Video);
         end loop;
         Gade.Dev.Interrupts.Handle_Interrupts(G.GB);
         Gade.Dev.Display.Check_Frame_Finished(G.GB.Display, Frame_Finished);
      end loop;
   end Next_Frame;

   procedure Finalize (This: in out Gade_Type) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Object => Opaque_Gade_Type, Name => Gade_Type);
   begin
      Put_Line("Finalize");
      Free(This);
   end Finalize;

end Gade.Interfaces;
