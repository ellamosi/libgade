limited with Gade.Input_Reader;
limited with Gade.Video_Buffer;

package Gade.Interfaces is

   type Gade_Type is private;

   procedure Create (G : out Gade_Type);

   procedure Reset (G : Gade_Type);

   procedure Load_ROM
     (G    : Gade_Type;
      Path : String);

   procedure Set_Input_Reader
     (G      : Gade_Type;
      Reader : Gade.Input_Reader.Input_Reader_Access);

   procedure Next_Frame
     (G     : Gade_Type;
      Video : Gade.Video_Buffer.RGB32_Display_Buffer_Access);

   procedure Finalize (This : in out Gade_Type);

private

   type Opaque_Gade_Type;
   type Gade_Type is access Opaque_Gade_Type;
   pragma Convention (C, Gade_Type);

end Gade.Interfaces;
