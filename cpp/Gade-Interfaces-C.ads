with Gade.Video_Buffer; use Gade.Video_Buffer;
with Gade.Input_Reader; use Gade.Input_Reader;

with System;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package Gade.Interfaces.C is

   type Gade_Type is private;

   procedure Initialize (This : out Gade_Type);
   pragma Export (C, Initialize, "gadeInit");

   procedure Finalize (This : in out Gade_Type);
   pragma Export (C, Finalize, "gadeFinal");

   procedure Reset (This: Gade_Type);
   pragma Export (C, Reset, "gadeReset");

   procedure Load_ROM
     (This     : Gade_Type;
      ROM_File : chars_ptr);
   pragma Export (C, Load_ROM, "gadeLoad");

   procedure Next_Frame
     (This  : Gade_Type;
      Video : RGB32_Display_Buffer_Access);
   pragma Export (C, Next_Frame, "gadeNextFrame");

   type Input_Reader_Class_Access is private;

   procedure Set_Input_Reader
     (This         : Gade_Type;
      Input_Reader : Input_Reader_Class_Access);
   pragma Export (C, Set_Input_Reader, "gadeSetInputReader");

private

   type Gade_Type is record
      Vptr   : System.Address;
      G      : Gade.Interfaces.Gade_Type;
   end record;
   pragma Convention (C, Gade_Type);

   type Input_Reader_Class;

   type Input_Reader_Class_Access is access all Input_Reader_Class;
   pragma Convention(C, Input_Reader_Class_Access);

end Gade.Interfaces.C;
