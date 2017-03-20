package body Image_IO.Image_Generic is

  function Get_Pixel (im: in Image;
     R, C: in Coordinate)
     return Image_Base.Pixel'Class is
  begin
    return im.a(C, R);
  end Get_Pixel;

  procedure Set_Pixel (im: in out Image;
                       R, C: in Coordinate;
                       px: in Image_Base.Pixel'Class) is
  begin
    im.a(C, R) := Pixel_Type(px);
  end Set_Pixel;

  function Create_Pixel (im: in Image) return Image_Base.Pixel_Access is
    p: Image_Base.Pixel_Access;
  begin
    p:= new Pixel_Type;
    return p;
  end Create_Pixel;

  function Gray_Sample  (im: in Image;
                         R, C: in Coordinate) return Sample is
  begin
    return im.a(C, R).Gray_Sample;
  end Gray_Sample;

  function Red_Sample   (im: in Image;
                         R, C: in Coordinate) return Sample is
  begin
    return im.a(C, R).Red_Sample;
  end Red_Sample;

  function Green_Sample (im: in Image;
                         R, C: in Coordinate) return Sample is
  begin
    return im.a(C, R).Green_Sample;
  end Green_Sample;

  function Blue_Sample  (im: in Image;
                         R, C: in Coordinate) return Sample is
  begin
    return im.a(C, R).Blue_Sample;
  end Blue_Sample;

  procedure Set_Gray  (im: in out Image;
                       R, C: in Coordinate;
                       s: in Sample) is
  begin
    im.a(C, R).Set_Gray(s);
  end Set_Gray;

  procedure Set_Red   (im: in out Image;
                       R, C: in Coordinate;
                       s: in Sample) is
  begin
    im.a(C, R).Set_Red(s);
  end Set_Red;

  procedure Set_Green (im: in out Image;
                       R, C: in Coordinate;
                       s: in Sample) is
  begin
    im.a(C, R).Set_Green(s);
  end Set_Green;

  procedure Set_Blue  (im: in out Image;
                       R, C: in Coordinate;
                       s: in Sample) is
  begin
    im.a(C, R).Set_Blue(s);
  end Set_Blue;

end Image_IO.Image_Generic;
