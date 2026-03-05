with Gade.Interfaces; use Gade.Interfaces;

package body Testd.Commands is

   function To_Image (Buffer : RGB32_Display_Buffer) return Image_IO.Image_Data is
      Result : Image_IO.Image_Data
        (0 .. Display_Height - 1, 0 .. Display_Width - 1);
   begin
      for Y in 0 .. Display_Height - 1 loop
         for X in 0 .. Display_Width - 1 loop
            Result (Y, X) :=
              (Red   => Image_IO.RGB_Value (Buffer (Y, X).Red),
               Green => Image_IO.RGB_Value (Buffer (Y, X).Green),
               Blue  => Image_IO.RGB_Value (Buffer (Y, X).Blue));
         end loop;
      end loop;
      return Result;
   end To_Image;

   function Image_Equal
     (Left  : Image_IO.Image_Data;
      Right : Image_IO.Image_Data) return Boolean
   is
   begin
      if Left'Length (1) /= Right'Length (1)
        or else Left'Length (2) /= Right'Length (2)
      then
         return False;
      end if;

      for Y in Left'Range (1) loop
         for X in Left'Range (2) loop
            if Left (Y, X) /= Right (Y, X) then
               return False;
            end if;
         end loop;
      end loop;

      return True;
   end Image_Equal;

   procedure Ensure_Engine (S : in out Session) is
   begin
      if not S.G_Created then
         Create (S.G);
         Set_Input_Reader (S.G, S.Input_Reader'Unchecked_Access);
         S.G_Created := True;
         S.ROM_Loaded := False;
         S.Frame_Count := 0;
      end if;
   end Ensure_Engine;

   procedure Run_Frame (S : in out Session) is
      Requested_Samples : constant := 1000;
      Generated_Samples : Natural;
      Frame_Finished    : Boolean := False;
   begin
      while not Frame_Finished loop
         Run_For (S.G, Requested_Samples, Generated_Samples,
                  S.V_Buff'Unchecked_Access, S.A_Buff'Unchecked_Access,
                  Frame_Finished);
      end loop;
      S.Frame_Count := S.Frame_Count + 1;
   end Run_Frame;

   function Ensure_ROM_Loaded (S : Session) return Boolean is
   begin
      if not S.ROM_Loaded then
         Reply_ERR ("BAD_STATE", "ROM not loaded");
         return False;
      end if;

      return True;
   end Ensure_ROM_Loaded;

end Testd.Commands;
