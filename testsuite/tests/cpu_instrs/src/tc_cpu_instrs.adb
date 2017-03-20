with Test_Directories;  use Test_Directories;
with Ada.Text_IO;       use Ada.Text_IO;
with Compare_Files;
with Gade.Interfaces;   use Gade.Interfaces;
with Gade.Video_Buffer; use Gade.Video_Buffer;

with Image_IO;            use Image_IO;
with Image_IO.Image;      use Image_IO.Image;
with Image_IO.Image_Base; use Image_IO.Image_Base;
with Image_IO.PBM;        use Image_IO.PBM;

procedure TC_CPU_Instrs is
   G       : Gade_Type;
   Buffer  : aliased RGB32_Display_Buffer;


   ROM_File      : constant String := "cpu_instrs.gb";
   Reference_PBM : constant String := "ref.ppm";

   type Image_Type is new Image_Access;

   procedure Set_Size
     (Image     : in out Image_Type;
      Max_Color : in     Pixel_Value_Type;
      Columns   : in     Dimension;
      Rows      : in     Dimension) is
   begin
      Image := Allocate_Image(Coordinate(Columns),
                              Coordinate(Rows),
                              True, Max_Color);
   end Set_Size;

   procedure Set_Pixel
    (Image  : in out Image_Type;
     Column : in     Coordinate;
     Row    : in     Coordinate;
     Value  : in     Color_Channels_Type) is
   begin
      Image.Set_Red(Coordinate(Row), Coordinate(Column), Sample(Value.Red));
      Image.Set_Green(Coordinate(Row), Coordinate(Column), Sample(Value.Green));
      Image.Set_Blue(Coordinate(Row), Coordinate(Column), Sample(Value.Blue));
    end Set_Pixel;

   procedure Read_PBM_Mem is new Read_PBM (Image_Type => Image_Type,
                                           Set_Size   => Set_Size,
                                           Set_Pixel  => Set_Pixel);

   Reference : Image_Type;
   S         : Sample;

   function Images_Different return Boolean is
      Diff : Boolean;
   begin
      for Row in Display_Vertical_Range loop
         for Col in Display_Horizontal_Range loop
            S := Red_Sample
              (Reference.all,
               Coordinate(Integer(Row) + 1),
               Coordinate(Integer(Col) + 1));
            Diff := Natural(S) /= Natural(Buffer(Row, Col).Red);
            S := Green_Sample
              (Reference.all,
               Coordinate(Integer(Row) + 1),
               Coordinate(Integer(Col) + 1));
            Diff := Diff or Natural(S) /= Natural(Buffer(Row, Col).Green);
            S := Blue_Sample
              (Reference.all,
               Coordinate(Integer(Row) + 1),
               Coordinate(Integer(Col) + 1));
            Diff := Diff or Natural(S) /= Natural(Buffer(Row, Col).Blue);
            if Diff then return True; end if;
         end loop;
      end loop;
      return False;
   end Images_Different;
begin
   Create(G);
   Load_ROM(G, Test_Dir & '/' & ROM_File);
   for i in 1..3500 loop
      Next_Frame(G, Buffer'Access);
   end loop;

   Read_PBM_Mem (Test_Dir & '/' & Reference_PBM, Reference);

   if Images_Different then
      Put_Line ("Resulting frame differs");
   else
      Put_Line ("CPU Instructions Test OK");
   end if;
end TC_CPU_Instrs;
