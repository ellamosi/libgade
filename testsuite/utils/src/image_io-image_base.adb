package body Image_IO.Image_Base is

  function Gray_Sample  (px: Pixel) return Sample is
  begin
    raise Call_Error;
    return 0; -- Fa content al compilador
  end Gray_Sample;

  function Red_Sample   (px: Pixel) return Sample is
  begin
    raise Call_Error;
    return 0;
  end Red_Sample;

  function Green_Sample (px: Pixel) return Sample is
  begin
    raise Call_Error;
    return 0;
  end Green_Sample;

  function Blue_Sample  (px: Pixel) return Sample is
  begin
    raise Call_Error;
    return 0;
  end Blue_Sample;

  procedure Set_Gray  (px: in out Pixel; s: in Sample) is
  begin
    raise Call_Error;
  end Set_Gray;

  procedure Set_Red   (px: in out Pixel; s: in Sample) is
  begin
    raise Call_Error;
  end Set_Red;

  procedure Set_Green (px: in out Pixel; s: in Sample) is
  begin
    raise Call_Error;
  end Set_Green;

  procedure Set_Blue  (px: in out Pixel; s: in Sample) is
  begin
    raise Call_Error;
  end Set_Blue;

end Image_IO.Image_Base;
