package Image_IO.Image_Base is

  -- Tipus Base de Pixel
  type Pixel is abstract tagged null record;

  function Gray_Sample  (px: Pixel) return Sample;

  function Red_Sample   (px: Pixel) return Sample;
  function Green_Sample (px: Pixel) return Sample;
  function Blue_Sample  (px: Pixel) return Sample;

  procedure Set_Gray  (px: in out Pixel; s: in Sample);

  procedure Set_Red   (px: in out Pixel; s: in Sample);
  procedure Set_Green (px: in out Pixel; s: in Sample);
  procedure Set_Blue  (px: in out Pixel; s: in Sample);

  type Pixel_Access is access Pixel'Class;

  -- Tipus Base de Imatge
  type Image(Width, Height: Coordinate) is abstract tagged null record;

  procedure Set_Pixel
    (im: in out Image;
     R, C: in Coordinate;
     px: in Pixel'Class) is abstract;

  function Get_Pixel
    (im: in Image;
     R, C: in Coordinate) return Pixel'Class is abstract;

  function Create_Pixel(im: in Image) return Pixel_Access is abstract;

  function Gray_Sample  (im: in Image;
                         R, C: in Coordinate) return Sample is abstract;

  function Red_Sample   (im: in Image;
                         R, C: in Coordinate) return Sample is abstract;
  function Green_Sample (im: in Image;
                         R, C: in Coordinate) return Sample is abstract;
  function Blue_Sample  (im: in Image;
                         R, C: in Coordinate) return Sample is abstract;

  procedure Set_Gray  (px: in out Image;
                       R, C: in Coordinate;
                       s: in Sample) is abstract;

  procedure Set_Red   (px: in out Image;
                       R, C: in Coordinate;
                       s: in Sample) is abstract;
  procedure Set_Green (px: in out Image;
                       R, C: in Coordinate;
                       s: in Sample) is abstract;
  procedure Set_Blue  (px: in out Image;
                       R, C: in Coordinate;
                       s: in Sample) is abstract;

end Image_IO.Image_Base;
