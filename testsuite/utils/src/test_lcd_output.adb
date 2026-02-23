with Ada.Text_IO;           use Ada.Text_IO;
with Image_IO;              use Image_IO;
with Image_IO.Holders;
with Image_IO.Operations;
with Test_Directories;      use Test_Directories;

with Gade.Interfaces;   use Gade.Interfaces;
with Gade.Video_Buffer; use Gade.Video_Buffer;
with Gade.Audio_Buffer; use Gade.Audio_Buffer;
with Ada.Command_Line;

package body Test_LCD_Output is

   function To_Image (Buffer : RGB32_Display_Buffer) return Image_Data;
   function Image_Equal (Left : Image_Data; Right : Image_Data) return Boolean;
   procedure Run_Frame
     (G        : in out Gade_Type;
      V_Buffer : RGB32_Display_Buffer_Access;
      A_Buffer : Audio_Buffer_Access);
   procedure Write_LCD_Output (Image : Image_Data);

   --------------
   -- To_Image --
   --------------

   function To_Image (Buffer : RGB32_Display_Buffer) return Image_Data is
      Result : Image_Data (0 .. Display_Height - 1, 0 .. Display_Width - 1);
   begin
      for Y in 0 .. Display_Height - 1 loop
         for X in 0 .. Display_Width - 1 loop
            Result (Y, X) :=
              (Red   => RGB_Value (Buffer (Y, X).Red),
               Green => RGB_Value (Buffer (Y, X).Green),
               Blue  => RGB_Value (Buffer (Y, X).Blue));
         end loop;
      end loop;

      return Result;
   end To_Image;

   -----------------
   -- Image_Equal --
   -----------------

   function Image_Equal (Left : Image_Data; Right : Image_Data) return Boolean is
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

   ---------------
   -- Run_Frame --
   ---------------

   procedure Run_Frame
     (G        : in out Gade_Type;
      V_Buffer : RGB32_Display_Buffer_Access;
      A_Buffer : Audio_Buffer_Access)
   is
      Requested_Samples : constant := 1000;
      Generated_Samples : Positive;
      FF : Boolean;
   begin
      FF := False;
      while not FF loop
         Run_For (G, Requested_Samples, Generated_Samples, V_Buffer, A_Buffer, FF);
      end loop;
   end Run_Frame;

   procedure Write_LCD_Output (Image : Image_Data) is
   begin
      Image_IO.Operations.Write_BMP (Test_Dir & "/test.bmp", Image);
   end Write_LCD_Output;

   procedure Run_LCD_Test
     (ROM_File  : String;
      Test_Name : String := "Untitled";
      Frames    : Positive := 100;
      Extra_Frames : Natural := 300)
   is
      G      : Gade_Type;
      V_Buff : aliased RGB32_Display_Buffer;
      A_Buff : aliased Audio_Buffer_Type;

      V_Buff_Ptr : constant RGB32_Display_Buffer_Access :=
        V_Buff'Unchecked_Access;
      A_Buff_Ptr : constant Audio_Buffer_Access :=
        A_Buff'Unchecked_Access;

      Ref_Handle   : Image_IO.Holders.Handle;
      Passed       : Boolean := False;
      Generated    : Image_Data (0 .. Display_Height - 1, 0 .. Display_Width - 1);
   begin
      Image_IO.Operations.Read (Test_Dir & "/ref.bmp", Ref_Handle);
      if Image_IO.Holders.Is_Empty (Ref_Handle) then
         Put_Line (Test_Name & " test FAILED");
         Ada.Command_Line.Set_Exit_Status (1);
         return;
      end if;

      Create (G);
      Load_ROM (G, Test_Dir & '/' & ROM_File);

      for I in 1 .. Frames + Extra_Frames loop
         Run_Frame (G, V_Buff_Ptr, A_Buff_Ptr);
         if I >= Frames then
            Generated := To_Image (V_Buff);
            if Image_Equal (Generated, Image_IO.Holders.Value (Ref_Handle)) then
               Passed := True;
               exit;
            end if;
         end if;
      end loop;

      Write_LCD_Output (Generated);

      if Passed then
         Put_Line (Test_Name & " test OK");
         Ada.Command_Line.Set_Exit_Status (0);
      else
         Put_Line (Test_Name & " test FAILED");
         Ada.Command_Line.Set_Exit_Status (1);
      end if;
   end Run_LCD_Test;

end Test_LCD_Output;
