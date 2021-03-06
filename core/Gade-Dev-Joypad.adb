package body Gade.Dev.Joypad is

   procedure Read_Button_Matrix
     (M     : in out Joypad_Matrix_Type;
      Input : Input_State) is
   begin
      M.P10_IN :=
        not ((not M.P14_OUT and Input.RIGHT) or (not M.P15_OUT and Input.A));
      M.P11_IN :=
        not ((not M.P14_OUT and Input.LEFT) or (not M.P15_OUT and Input.B));
      M.P12_IN :=
        not ((not M.P14_OUT and Input.UP) or (not M.P15_OUT and Input.SEL));
      M.P13_IN :=
        not ((not M.P14_OUT and Input.DOWN) or (not M.P15_OUT and Input.START));
   end Read_Button_Matrix;

   overriding
   procedure Reset (Device : in out Joypad_Type) is
   begin
      Device.Map.P10_IN := True;
      Device.Map.P11_IN := True;
      Device.Map.P12_IN := True;
      Device.Map.P13_IN := True;
   end Reset;

   overriding
   procedure Read
     (Joypad  : in out Joypad_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Value   : out Byte)
   is
      pragma Unreferenced (GB, Address);
      New_Matrix : Joypad_Matrix_Type;
      New_State  : Input_State;
   begin
      if Joypad.Reader /= null then
         New_Matrix := Joypad.Map;
         New_State := Read_Input (Joypad.Reader.all);
         Read_Button_Matrix (New_Matrix, New_State);
         Joypad.Map.Reg :=
           (Joypad.Map.Reg and 16#F0#) or (New_Matrix.Reg and 16#0F#);
      end if;
      Value := Joypad.Map.Reg;
   end Read;

   overriding
   procedure Write
     (Joypad  : in out Joypad_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : Byte) is
      pragma Unreferenced (GB, Address);
   begin
      Joypad.Map.Reg := (Content and 16#F0#) or (Joypad.Map.Reg and 16#0F#);
   end Write;

   overriding
   procedure Report_Cycles
     (Joypad : in out Joypad_Type;
      GB     : in out Gade.GB.GB_Type;
      Cycles : Positive) is

--        function High_To_Low
--          (Old_Matrix, New_Matrix : Joypad_Matrix_Type) return Boolean is
--           pragma Unreferenced (Old_Matrix);
--        begin
--           return
--             (Joypad.Map.P10_IN and not New_Matrix.P10_IN) or
--             (Joypad.Map.P11_IN and not New_Matrix.P11_IN) or
--             (Joypad.Map.P12_IN and not New_Matrix.P12_IN) or
--             (Joypad.Map.P13_IN and not New_Matrix.P13_IN);
--        end High_To_Low;
   begin
      --  TODO: Tweak types/make procedures for all this masking/addresses
      --  if High_To_Low(Joypad.Matrix, New_Matrix) then
      --     GB.MM.Content(16#FF0F#) := GB.MM.Content(16#FF0F#) or 16#10#;
      --  end if;
      null;
   end Report_Cycles;

   procedure Set_Input_Reader
     (Joypad : in out Joypad_Type;
      Reader : Input_Reader_Access) is
   begin
      Joypad.Reader := Reader;
   end Set_Input_Reader;

end Gade.Dev.Joypad;
