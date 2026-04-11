with Gade.Audio_Buffer; use Gade.Audio_Buffer;
with Gade.Video_Buffer; use Gade.Video_Buffer;

with System;
with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Gade.Camera;          use Gade.Camera;
with Gade.Input;           use Gade.Input;

package Gade.Interfaces.C is

   type Gade_Type is private;

   procedure Initialize (This : out Gade_Type);
   pragma Export (C, Initialize, "gadeInit");

   procedure Finalize (This : in out Gade_Type);
   pragma Export (C, Finalize, "gadeFinal");

   procedure Reset (This : Gade_Type);
   pragma Export (C, Reset, "gadeReset");

   procedure Load_ROM (This : Gade_Type; ROM_File : chars_ptr);
   pragma Export (C, Load_ROM, "gadeLoad");

   procedure Run_For
     (This              : Gade_Type;
      Requested_Samples : unsigned;
      Generated_Samples : out unsigned;
      Video             : RGB32_Display_Buffer_Access;
      Audio             : Audio_Buffer_Access;
      Frame_Finished    : out unsigned_char);
   pragma Export (C, Run_For, "gadeRunFor");

   procedure Next_Frame (This : Gade_Type; Video : RGB32_Display_Buffer_Access);
   pragma Export (C, Next_Frame, "gadeNextFrame");

   type Input_Reader_Class_Access is private;

   procedure Set_Input_Reader
     (This : in out Gade_Type; Input_Reader : Input_Reader_Class_Access);
   pragma Export (C, Set_Input_Reader, "gadeSetInputReader");

   type Camera_Provider_Class_Access is private;

   procedure Set_Camera_Provider
     (This : in out Gade_Type; Camera_Provider : Camera_Provider_Class_Access);
   pragma Export (C, Set_Camera_Provider, "gadeSetCameraProvider");

private

   type C_State;
   type C_State_Access is access all C_State;

   type Camera_Provider_Wrapper;
   type Camera_Provider_Wrapper_Access is access all Camera_Provider_Wrapper;

   type Input_Reader_Wrapper;
   type Input_Reader_Wrapper_Access is access all Input_Reader_Wrapper;

   type Gade_Type is record
      Vptr  : System.Address;
      State : C_State_Access;
   end record;
   pragma Convention (C, Gade_Type);

   type Camera_Provider_Class;

   type Camera_Provider_Class_Access is access all Camera_Provider_Class;
   pragma Convention (C, Camera_Provider_Class_Access);

   type Camera_Provider_Wrapper is new Provider_Interface with record
      C_Instance : Camera_Provider_Class_Access;
   end record;

   overriding
   procedure Capture_Frame (Wrapper : Camera_Provider_Wrapper; Frame : out Bitmap);

   type Input_Reader_Class;

   type Input_Reader_Class_Access is access all Input_Reader_Class;
   pragma Convention (C, Input_Reader_Class_Access);

   type Input_Reader_Wrapper is new Reader_Interface with record
      C_Instance : Input_Reader_Class_Access;
   end record;

   overriding
   function Read_Input (Wrapper : Input_Reader_Wrapper) return State;

   type C_State is record
      Camera_Provider : Camera_Provider_Wrapper_Access := null;
      G               : Gade.Interfaces.Gade_Type;
      Input_Reader    : Input_Reader_Wrapper_Access := null;
   end record;

end Gade.Interfaces.C;
