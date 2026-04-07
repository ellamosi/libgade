with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

package body Gade.Interfaces.C is

   procedure Initialize (This : out Gade_Type) is
   begin
      Gade.Interfaces.Create (This.G);
      This.Input_Reader := null;
   end Initialize;

   procedure Finalize (This : in out Gade_Type) is
      procedure Free_Input_Reader is new
        Ada.Unchecked_Deallocation
          (Object => Input_Reader_Wrapper,
           Name   => Input_Reader_Wrapper_Access);
   begin
      Gade.Interfaces.Set_Input_Reader (This.G, null);
      if This.Input_Reader /= null then
         Free_Input_Reader (This.Input_Reader);
      end if;
      Gade.Interfaces.Finalize (This.G);
   end Finalize;

   procedure Reset (This : Gade_Type) is
   begin
      Gade.Interfaces.Reset (This.G);
   end Reset;

   procedure Load_ROM (This : Gade_Type; ROM_File : chars_ptr) is
   begin
      Gade.Interfaces.Load_ROM (This.G, Value (ROM_File));
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

      Gade.Interfaces.Run_For (This.G, Requested, Generated, Video, Audio, Finished);
      Generated_Samples := unsigned (Generated);
      Frame_Finished := (if Finished then 1 else 0);
   end Run_For;

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
      procedure Free_Input_Reader is new
        Ada.Unchecked_Deallocation
          (Object => Input_Reader_Wrapper,
           Name   => Input_Reader_Wrapper_Access);
      Wrapper : Input_Reader_Wrapper_Access;
   begin
      Gade.Interfaces.Set_Input_Reader (This.G, null);
      if This.Input_Reader /= null then
         Free_Input_Reader (This.Input_Reader);
      end if;

      if Input_Reader = null then
         return;
      end if;

      Wrapper := new Input_Reader_Wrapper;
      Wrapper.C_Instance := Input_Reader;
      This.Input_Reader := Wrapper;
      Gade.Interfaces.Set_Input_Reader (This.G, Gade.Input.Reader_Access (Wrapper));
   end Set_Input_Reader;

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

end Gade.Interfaces.C;
