with Interfaces.C;             use Interfaces.C;
with Ada.Unchecked_Conversion;

package body Gade.Interfaces.C is

   procedure Initialize (This : out Gade_Type) is
   begin
      Gade.Interfaces.Create(This.G);
   end Initialize;

   procedure Finalize (This: in out Gade_Type) is
   begin
      Gade.Interfaces.Finalize(This.G);
   end Finalize;

   procedure Reset (This: Gade_Type) is
   begin
      Gade.Interfaces.Reset(This.G);
   end Reset;

   procedure Load_ROM
     (This     : Gade_Type;
      ROM_File : chars_ptr) is
   begin
      Gade.Interfaces.Load_ROM(This.G, Value(ROM_File));
   end Load_ROM;

   type Input_Reader_Class is null record;

   function Read_Input
     (This : Input_Reader_Class_Access) return unsigned_char;
   pragma Import (C, Read_Input, "InputReader_readInput");

   type Input_Reader_Wrapper is new Input_Reader_Type with record
      C_Instance : Input_Reader_Class_Access;
   end record;

   function Read_Input (Wrapper: Input_Reader_Wrapper) return Input_State is
      function To_Input_State is
         new Ada.Unchecked_Conversion (Source => unsigned_char,
                                       Target => Input_State);
   begin
      return To_Input_State(Read_Input(Wrapper.C_Instance));
   end Read_Input;

   procedure Set_Input_Reader
     (This         : Gade_Type;
      Input_Reader : Input_Reader_Class_Access) is
      Wrapper : access Input_Reader_Wrapper;
   begin
      Wrapper := new Input_Reader_Wrapper;
      Gade.Interfaces.Set_Input_Reader(This.G, Wrapper);
   end Set_Input_Reader;

   procedure Next_Frame
     (This  : Gade_Type;
      Video : RGB32_Display_Buffer_Access) is
   begin
      Gade.Interfaces.Next_Frame(This.G, Video);
   end Next_Frame;

end Gade.Interfaces.C;
