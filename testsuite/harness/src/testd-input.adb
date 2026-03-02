with Ada.Characters.Handling;

package body Testd.Input is

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
         when Button_A =>
            Reader.State.A := Pressed;
         when Button_B =>
            Reader.State.B := Pressed;
         when Button_Select =>
            Reader.State.SEL := Pressed;
         when Button_Start =>
            Reader.State.START := Pressed;
         when Button_Right =>
            Reader.State.RIGHT := Pressed;
         when Button_Left =>
            Reader.State.LEFT := Pressed;
         when Button_Up =>
            Reader.State.UP := Pressed;
         when Button_Down =>
            Reader.State.DOWN := Pressed;
      end case;
   end Set_Button;

   procedure Set_State_From_Mask
     (Reader : in out Manual_Input_Reader;
      Mask   : Interfaces.Unsigned_8) is
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
      if Upper = "SEL" then -- Alias
         Button := Button_Select;
      else
         Button := Button_Name'Value ("Button_" & Upper);
      end if;
      return True;
   exception
      when Constraint_Error => return False;
   end Parse_Button;

end Testd.Input;
