with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

package body Gade.Interfaces.C is

   type Flat_Bitmap is
     array (0 .. Gade.Camera.Capture_Height * Gade.Camera.Capture_Width - 1)
     of unsigned_char;
   pragma Convention (C, Flat_Bitmap);

   procedure Free_Camera_Provider is new
     Ada.Unchecked_Deallocation
       (Object => Camera_Provider_Wrapper,
        Name   => Camera_Provider_Wrapper_Access);

   procedure Free_Input_Reader is new
     Ada.Unchecked_Deallocation
       (Object => Input_Reader_Wrapper,
        Name   => Input_Reader_Wrapper_Access);

   procedure Free_State is new
     Ada.Unchecked_Deallocation (Object => C_State, Name => C_State_Access);

   function To_Pixel_Value (Value : unsigned_char) return Gade.Camera.Pixel_Value;

   procedure Initialize (This : out Gade_Type) is
   begin
      This.Vptr := System.Null_Address;
      This.State := new C_State;
      Gade.Interfaces.Create (This.State.G);
   exception
      when others =>
         if This.State /= null then
            Free_State (This.State);
         end if;
         raise;
   end Initialize;

   procedure Finalize (This : in out Gade_Type) is
   begin
      Gade.Interfaces.Set_Camera_Provider (This.State.G, null);
      Gade.Interfaces.Set_Input_Reader (This.State.G, null);
      if This.State.Camera_Provider /= null then
         Free_Camera_Provider (This.State.Camera_Provider);
      end if;
      if This.State.Input_Reader /= null then
         Free_Input_Reader (This.State.Input_Reader);
      end if;
      Gade.Interfaces.Finalize (This.State.G);
      Free_State (This.State);
   end Finalize;

   procedure Reset (This : Gade_Type) is
   begin
      Gade.Interfaces.Reset (This.State.G);
   end Reset;

   procedure Load_ROM (This : Gade_Type; ROM_File : chars_ptr) is
   begin
      Gade.Interfaces.Load_ROM (This.State.G, Value (ROM_File));
   end Load_ROM;

   procedure Run_For
     (This              : Gade_Type;
      Requested_Samples : unsigned;
      Generated_Samples : out unsigned;
      Video             : RGB32_Display_Buffer_Access;
      Audio             : Audio_Buffer_Access;
      Frame_Finished    : out unsigned_char)
   is
      Requested : Positive;
      Generated : Natural;
      Finished  : Boolean;
   begin
      if Requested_Samples = 0 then
         Requested := 1;
      else
         Requested := Positive (Requested_Samples);
      end if;

      Gade.Interfaces.Run_For
        (This.State.G, Requested, Generated, Video, Audio, Finished);
      Generated_Samples := unsigned (Generated);
      Frame_Finished := (if Finished then 1 else 0);
   end Run_For;

   type Camera_Provider_Class is null record;

   procedure Set_Capture_Active
     (This : Camera_Provider_Class_Access; Active : unsigned_char);
   pragma Import (C, Set_Capture_Active, "CameraProvider_setCaptureActive");

   procedure Capture_Frame (This : Camera_Provider_Class_Access; Bitmap : System.Address);
   pragma Import (C, Capture_Frame, "CameraProvider_captureFrame");

   overriding
   procedure Set_Capture_Active
     (Wrapper : in out Camera_Provider_Wrapper; Active : Boolean) is
   begin
      Set_Capture_Active (Wrapper.C_Instance, (if Active then 1 else 0));
   end Set_Capture_Active;

   overriding
   procedure Capture_Frame (Wrapper : Camera_Provider_Wrapper; Frame : out Bitmap) is
      Flat  : aliased Flat_Bitmap;
      Index : Natural := Flat'First;
   begin
      Capture_Frame (Wrapper.C_Instance, Flat'Address);
      for Y in Frame'Range (1) loop
         for X in Frame'Range (2) loop
            Frame (Y, X) := To_Pixel_Value (Flat (Index));
            Index := Index + 1;
         end loop;
      end loop;
   end Capture_Frame;

   type Input_Reader_Class is null record;

   function Read_Input (This : Input_Reader_Class_Access) return unsigned_char;
   pragma Import (C, Read_Input, "InputReader_readInput");

   overriding
   function Read_Input (Wrapper : Input_Reader_Wrapper) return State is
      function To_Input_State is new
        Ada.Unchecked_Conversion (Source => unsigned_char, Target => State);
   begin
      return To_Input_State (Read_Input (Wrapper.C_Instance));
   end Read_Input;

   procedure Set_Input_Reader
     (This : in out Gade_Type; Input_Reader : Input_Reader_Class_Access)
   is
      Wrapper : Input_Reader_Wrapper_Access;
   begin
      Gade.Interfaces.Set_Input_Reader (This.State.G, null);
      if This.State.Input_Reader /= null then
         Free_Input_Reader (This.State.Input_Reader);
      end if;

      if Input_Reader = null then
         return;
      end if;

      Wrapper := new Input_Reader_Wrapper;
      Wrapper.C_Instance := Input_Reader;
      This.State.Input_Reader := Wrapper;
      Gade.Interfaces.Set_Input_Reader (This.State.G, Gade.Input.Reader_Access (Wrapper));
   end Set_Input_Reader;

   procedure Set_Camera_Provider
     (This : in out Gade_Type; Camera_Provider : Camera_Provider_Class_Access)
   is
      Wrapper : Camera_Provider_Wrapper_Access;
   begin
      Gade.Interfaces.Set_Camera_Provider (This.State.G, null);
      if This.State.Camera_Provider /= null then
         Free_Camera_Provider (This.State.Camera_Provider);
      end if;

      if Camera_Provider = null then
         return;
      end if;

      Wrapper := new Camera_Provider_Wrapper;
      Wrapper.C_Instance := Camera_Provider;
      This.State.Camera_Provider := Wrapper;
      Gade.Interfaces.Set_Camera_Provider
        (This.State.G, Gade.Camera.Provider_Access (Wrapper));
   end Set_Camera_Provider;

   procedure Next_Frame (This : Gade_Type; Video : RGB32_Display_Buffer_Access) is
      Audio             : aliased Frame_Audio_Buffer;
      Generated_Samples : unsigned;
      Frame_Finished    : unsigned_char;
   begin
      Run_For
        (This,
         unsigned (Maximum_Samples),
         Generated_Samples,
         Video,
         Audio'Unchecked_Access,
         Frame_Finished);
   end Next_Frame;

   function To_Pixel_Value (Value : unsigned_char) return Gade.Camera.Pixel_Value is
   begin
      if Value > unsigned_char (Gade.Camera.Pixel_Value'Last) then
         return Gade.Camera.Pixel_Value'Last;
      else
         return Gade.Camera.Pixel_Value (Value);
      end if;
   end To_Pixel_Value;

end Gade.Interfaces.C;
