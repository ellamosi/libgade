with Ada.Characters.Handling;

package body Harness_Input is

   use Ada.Characters.Handling;
   use type Interfaces.Unsigned_8;

   overriding
   function Read_Input
     (Reader : Manual_Input_Reader) return Gade.Input_Reader.Input_State is
   begin
      return Reader.State;
   end Read_Input;

   procedure Clear (Reader : in out Manual_Input_Reader) is
   begin
      Reader.State := (others => False);
   end Clear;

   procedure Set_Button
     (Reader  : in out Manual_Input_Reader;
      Button  : Button_Name;
      Pressed : Boolean) is
   begin
      case Button is
         when A =>
            Reader.State.A := Pressed;
         when B =>
            Reader.State.B := Pressed;
         when SELECT_BTN =>
            Reader.State.SEL := Pressed;
         when START =>
            Reader.State.START := Pressed;
         when RIGHT =>
            Reader.State.RIGHT := Pressed;
         when LEFT =>
            Reader.State.LEFT := Pressed;
         when UP =>
            Reader.State.UP := Pressed;
         when DOWN =>
            Reader.State.DOWN := Pressed;
      end case;
   end Set_Button;

   procedure Set_State_From_Mask
     (Reader : in out Manual_Input_Reader;
      Mask   : Interfaces.Unsigned_8) is
      function Is_Set (Bit : Natural) return Boolean;

      function Is_Set (Bit : Natural) return Boolean is
      begin
         return (Mask and Interfaces.Shift_Left (Interfaces.Unsigned_8 (1), Bit))
           /= 0;
      end Is_Set;
   begin
      Reader.State.A := Is_Set (0);
      Reader.State.B := Is_Set (1);
      Reader.State.SEL := Is_Set (2);
      Reader.State.START := Is_Set (3);
      Reader.State.RIGHT := Is_Set (4);
      Reader.State.LEFT := Is_Set (5);
      Reader.State.UP := Is_Set (6);
      Reader.State.DOWN := Is_Set (7);
   end Set_State_From_Mask;

   function Parse_Button
     (Name   : String;
      Button : out Button_Name) return Boolean
   is
      Upper : constant String := To_Upper (Name);
   begin
      if Upper = "A" then
         Button := A;
      elsif Upper = "B" then
         Button := B;
      elsif Upper = "SELECT" or else Upper = "SEL" then
         Button := SELECT_BTN;
      elsif Upper = "START" then
         Button := START;
      elsif Upper = "RIGHT" then
         Button := RIGHT;
      elsif Upper = "LEFT" then
         Button := LEFT;
      elsif Upper = "UP" then
         Button := UP;
      elsif Upper = "DOWN" then
         Button := DOWN;
      else
         return False;
      end if;

      return True;
   end Parse_Button;

end Harness_Input;
