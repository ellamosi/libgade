with Gade.Input;
with Interfaces;

package Testd.Input is

   type Button_Name is
     (Button_A,
      Button_B,
      Button_Select,
      Button_Start,
      Button_Right,
      Button_Left,
      Button_Up,
      Button_Down);

   type Manual_Input_Reader is new Gade.Input.Reader_Interface with private;

   overriding
   function Read_Input (Reader : Manual_Input_Reader) return Gade.Input.State;

   procedure Clear (Reader : in out Manual_Input_Reader);

   procedure Set_Button
     (Reader  : in out Manual_Input_Reader;
      Button  : Button_Name;
      Pressed : Boolean);

   procedure Set_State_From_Mask
     (Reader : in out Manual_Input_Reader; Mask : Interfaces.Unsigned_8);

   function Parse_Button
     (Name : String; Button : out Button_Name) return Boolean;

private

   type Manual_Input_Reader is new Gade.Input.Reader_Interface with record
      State : Gade.Input.State := (others => False);
   end record;

end Testd.Input;
