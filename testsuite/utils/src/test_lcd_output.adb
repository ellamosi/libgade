with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Bitmap;                use Bitmap;
with Bitmap.Buffer;         use Bitmap.Buffer;
with Bitmap.Memory_Mapped;  use Bitmap.Memory_Mapped;
with Bitmap.File_IO;        use Bitmap.File_IO;
with Compare_Files;         use Compare_Files;
with Test_Directories;      use Test_Directories;

with Gade.Interfaces;   use Gade.Interfaces;
with Gade.Video_Buffer; use Gade.Video_Buffer;

package body Test_LCD_Output is

   ---------------------
   -- Allocate_Bitmap --
   ---------------------

   function Allocate_Bitmap return not null Any_Bitmap_Buffer is
      type Pixel_Data is new Bitmap.UInt24_Array
        (1 .. Display_Width * Display_Height) with Pack;
      BM : constant Any_Memory_Mapped_Bitmap_Buffer :=
        new Memory_Mapped_Bitmap_Buffer;
      Data : constant access Pixel_Data := new Pixel_Data;
   begin
      BM.Actual_Width := Display_Width;
      BM.Actual_Height := Display_Height;
      BM.Actual_Color_Mode := RGB_888;
      BM.Currently_Swapped := False;
      BM.Addr := Data.all'Address;
      return Any_Bitmap_Buffer (BM);
   end Allocate_Bitmap;

   -----------------
   -- Copy_Buffer --
   -----------------

   procedure Copy_Buffer
     (From : RGB32_Display_Buffer;
      To   : not null Any_Bitmap_Buffer)
   is
      Pix : Bitmap_Color;
   begin
      for Y in 0 .. Display_Height - 1 loop
         for X in 0 .. Display_Width - 1 loop
            Pix.Red   := UInt8(From(Y, X).Red);
            Pix.Green := UInt8(From(Y, X).Green);
            Pix.Blue  := UInt8(From(Y, X).Blue);
            To.Set_Pixel((X, Y), Pix);
         end loop;
      end loop;
   end Copy_Buffer;

   -------------
   -- Run_ROM --
   -------------

   procedure Run_ROM
     (ROM_File : String;
      Buffer   : access RGB32_Display_Buffer;
      Frames   : Positive)
   is
      G : Gade_Type;
   begin
      Create(G);
      Load_ROM(G, Test_Dir & '/' & ROM_File);
      for i in 1..Frames loop
         Next_Frame(G, Buffer);
      end loop;
   end Run_ROM;

   ----------------------
   -- Write_LCD_Output --
   ----------------------

   procedure Write_LCD_Output (Buffer : RGB32_Display_Buffer) is
      Output_File : Ada.Streams.Stream_IO.File_Type;
      BM          : constant not null Any_Bitmap_Buffer := Allocate_Bitmap;
   begin
      Copy_Buffer(Buffer, BM);
      Create (Output_File, Out_File, Test_Dir & "/test.bmp");
      Write_BMP_File (Output_File, BM.all);
      Close (Output_File);
   end Write_LCD_Output;

   ---------------------
   -- Test_LCD_Output --
   ---------------------

   procedure Run_LCD_Test
     (ROM_File  : String;
      Test_Name : String := "Untitled";
      Frames    : Positive := 100)
   is
      Buffer   : aliased RGB32_Display_Buffer;
   begin
      Run_ROM (ROM_File, Buffer'Access, Frames);
      Write_LCD_Output(Buffer);
      if Binnary_Equal (Test_Dir & "/test.bmp", Test_Dir & "/ref.bmp") then
         Put_Line(Test_Name & " test OK");
      else
         Put_Line(Test_Name & " test FAILED");
      end if;
   end Run_LCD_Test;

end Test_LCD_Output;
