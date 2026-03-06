with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

package body Gade.Interfaces.C is

   procedure Initialize (This : out Gade_Type) is
   begin
      Gade.Interfaces.Create (This.G);
      This.Logger := null;
   end Initialize;

   procedure Finalize (This : in out Gade_Type) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Object => Logger_Wrapper,
         Name   => Logger_Wrapper_Access);
   begin
      Gade.Interfaces.Set_Logger (This.G, null);
      if This.Logger /= null then
         Free (This.Logger);
      end if;
      Gade.Interfaces.Finalize (This.G);
   end Finalize;

   procedure Reset (This : Gade_Type) is
   begin
      Gade.Interfaces.Reset (This.G);
   end Reset;

   procedure Load_ROM
     (This     : Gade_Type;
      ROM_File : chars_ptr) is
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

      Gade.Interfaces.Run_For
        (This.G,
         Requested,
         Generated,
         Video,
         Audio,
         Finished);
      Generated_Samples := unsigned (Generated);
      Frame_Finished := (if Finished then 1 else 0);
   end Run_For;

   type Input_Reader_Class is null record;

   function Read_Input
     (This : Input_Reader_Class_Access) return unsigned_char;
   pragma Import (C, Read_Input, "InputReader_readInput");

   overriding
   function Read_Input (Wrapper : Input_Reader_Wrapper) return Input_State is
      function To_Input_State is
         new Ada.Unchecked_Conversion (Source => unsigned_char,
                                       Target => Input_State);
   begin
      return To_Input_State (Read_Input (Wrapper.C_Instance));
   end Read_Input;

   procedure Set_Input_Reader
     (This         : Gade_Type;
      Input_Reader : Input_Reader_Class_Access) is
      Wrapper : Input_Reader_Wrapper_Access;
   begin
      Wrapper := new Input_Reader_Wrapper;
      Wrapper.C_Instance := Input_Reader;
      Gade.Interfaces.Set_Input_Reader
        (This.G, Gade.Input_Reader.Input_Reader_Access (Wrapper));
   end Set_Input_Reader;

   type Logger_Class is null record;

   procedure Log
     (This    : Logger_Class_Access;
      Level   : unsigned_char;
      Message : chars_ptr);
   pragma Import (C, Log, "Logger_log");

   function To_C_Level (Level : Log_Level) return unsigned_char;
   function To_C_Level (Level : Log_Level) return unsigned_char is
   begin
      return unsigned_char (Log_Level'Pos (Level));
   end To_C_Level;

   overriding
   procedure Log
     (Wrapper : in out Logger_Wrapper;
      Level   : Log_Level;
      Message : String)
   is
      C_Message : chars_ptr := New_String (Message);
   begin
      Log (Wrapper.C_Instance, To_C_Level (Level), C_Message);
      Free (C_Message);
   end Log;

   procedure Set_Logger
     (This   : in out Gade_Type;
      Logger : Logger_Class_Access) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Object => Logger_Wrapper,
         Name   => Logger_Wrapper_Access);
      Wrapper : Logger_Wrapper_Access;
   begin
      Gade.Interfaces.Set_Logger (This.G, null);
      if This.Logger /= null then
         Free (This.Logger);
      end if;

      if Logger = null then
         return;
      end if;

      Wrapper := new Logger_Wrapper;
      Wrapper.C_Instance := Logger;
      This.Logger := Wrapper;
      Gade.Interfaces.Set_Logger (This.G, Gade.Logging.Logger_Access (Wrapper));
   end Set_Logger;

   procedure Next_Frame
     (This  : Gade_Type;
      Video : RGB32_Display_Buffer_Access)
   is
      Audio             : aliased Audio_Buffer_Type;
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
