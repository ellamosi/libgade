with Gade.Input_Reader;
with Interfaces;

package Harness_Input is

   type Button_Name is (A, B, SELECT_BTN, START, RIGHT, LEFT, UP, DOWN);

   type Manual_Input_Reader is
     new Gade.Input_Reader.Input_Reader_Type with private;

   overriding
   function Read_Input
     (Reader : Manual_Input_Reader) return Gade.Input_Reader.Input_State;

   procedure Clear (Reader : in out Manual_Input_Reader);

   procedure Set_Button
     (Reader  : in out Manual_Input_Reader;
      Button  : Button_Name;
      Pressed : Boolean);

   procedure Set_State_From_Mask
     (Reader : in out Manual_Input_Reader;
      Mask   : Interfaces.Unsigned_8);

   function Parse_Button
     (Name   : String;
      Button : out Button_Name) return Boolean;

private

   type Manual_Input_Reader is
     new Gade.Input_Reader.Input_Reader_Type with record
      State : Gade.Input_Reader.Input_State := (others => False);
   end record;

end Harness_Input;
