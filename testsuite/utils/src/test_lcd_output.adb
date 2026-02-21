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
with Gade.Audio_Buffer; use Gade.Audio_Buffer;
with Ada.Command_Line;

package body Test_LCD_Output is

   function Allocate_Bitmap return not null Any_Bitmap_Buffer;
   procedure Copy_Buffer
     (From : RGB32_Display_Buffer;
      To   : not null Any_Bitmap_Buffer);
   procedure Run_ROM
     (ROM_File : String;
      V_Buffer : RGB32_Display_Buffer_Access;
      A_Buffer : Audio_Buffer_Access;
      Frames   : Positive);
   procedure Write_LCD_Output (Buffer : RGB32_Display_Buffer);

   ---------------------
   -- Allocate_Bitmap --
   ---------------------

   function Allocate_Bitmap return not null Any_Bitmap_Buffer is
      type Pixel_Data is new Bitmap.UInt24_Array
        (1 .. Display_Width * Display_Height) with Pack;
      type Pixel_Data_Access is access Pixel_Data;
      BM : constant Any_Memory_Mapped_Bitmap_Buffer :=
        new Memory_Mapped_Bitmap_Buffer;
      Data : constant Pixel_Data_Access := new Pixel_Data;
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
            Pix.Red   := UInt8 (From (Y, X).Red);
            Pix.Green := UInt8 (From (Y, X).Green);
            Pix.Blue  := UInt8 (From (Y, X).Blue);
            To.Set_Pixel ((X, Y), Pix);
         end loop;
      end loop;
   end Copy_Buffer;

   -------------
   -- Run_ROM --
   -------------

   procedure Run_ROM
     (ROM_File : String;
      V_Buffer : RGB32_Display_Buffer_Access;
      A_Buffer : Audio_Buffer_Access;
      Frames   : Positive)
   is
      G : Gade_Type;
      Requested_Samples : constant := 1000;
      Generated_Samples : Positive;
      FF : Boolean;

      F      : Ada.Streams.Stream_IO.File_Type;
      Stream : Ada.Streams.Stream_IO.Stream_Access;
   begin
      Ada.Streams.Stream_IO.Create (F);
      Stream := Ada.Streams.Stream_IO.Stream (F);
      Create (G);
      Load_ROM (G, Test_Dir & '/' & ROM_File);
      for i in 1 .. Frames loop
         FF := False;
         while not FF loop
            Run_For (G, Requested_Samples, Generated_Samples, V_Buffer, A_Buffer, FF);
            for j in 0 .. Generated_Samples - 1 loop
               Sample'Write (Stream, A_Buffer (j).Left);
            end loop;
         end loop;
      end loop;
   end Run_ROM;

   procedure Write_LCD_Output (Buffer : RGB32_Display_Buffer) is
      Output_File : Ada.Streams.Stream_IO.File_Type;
      BM          : constant not null Any_Bitmap_Buffer := Allocate_Bitmap;
   begin
      Copy_Buffer (Buffer, BM);
      Create (Output_File, Out_File, Test_Dir & "/test.bmp");
      Write_BMP_File (Output_File, BM.all);
      Close (Output_File);
   end Write_LCD_Output;

   procedure Run_LCD_Test
     (ROM_File  : String;
      Test_Name : String := "Untitled";
      Frames    : Positive := 100)
   is
      V_Buff : aliased RGB32_Display_Buffer;
      A_Buff : aliased Audio_Buffer_Type;

      V_Buff_Ptr : constant RGB32_Display_Buffer_Access :=
        V_Buff'Unchecked_Access;
      A_Buff_Ptr : constant Audio_Buffer_Access :=
        A_Buff'Unchecked_Access;
   begin
      Run_ROM (ROM_File, V_Buff_Ptr, A_Buff_Ptr, Frames);
      Write_LCD_Output (V_Buff);
      if Binnary_Equal (Test_Dir & "/test.bmp", Test_Dir & "/ref.bmp") then
         Put_Line (Test_Name & " test OK");
         Ada.Command_Line.Set_Exit_Status (0);
      else
         Put_Line (Test_Name & " test FAILED");
         Ada.Command_Line.Set_Exit_Status (1);
      end if;
   end Run_LCD_Test;

end Test_LCD_Output;
