package body Image_IO.Pixel_Generic is

  function Gray_Sample(px: Pixel_Grayscale) return Sample is
  begin
    return Channel_Type'Pos(px.gr);
  end Gray_Sample;

  procedure Set_Gray  (px: in out Pixel_Grayscale; s: in Sample) is
  begin
    px.gr := Channel_Type'Val(s);
  end Set_Gray;

  function Red_Sample(px: Pixel_Color) return Sample is
  begin
    return Channel_Type'Pos(px.r);
  end Red_Sample;

  procedure Set_Red  (px: in out Pixel_Color; s: in Sample) is
  begin
    px.r := Channel_Type'Val(s);
  end Set_Red;

  function Green_Sample(px: Pixel_Color) return Sample is
  begin
    return Channel_Type'Pos(px.g);
  end Green_Sample;

  procedure Set_Green  (px: in out Pixel_Color; s: in Sample) is
  begin
    px.g := Channel_Type'Val(s);
  end Set_Green;

  function Blue_Sample(px: Pixel_Color) return Sample is
  begin
    return Channel_Type'Pos(px.b);
  end Blue_Sample;

  procedure Set_Blue  (px: in out Pixel_Color; s: in Sample) is
  begin
    px.b := Channel_Type'Val(s);
  end Set_Blue;

end Image_IO.Pixel_Generic;
