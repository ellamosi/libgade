with Image_IO.Image_Base;

generic
  type Pixel_Type is new Image_Base.Pixel with Private;
package Image_IO.Image_Generic is
  type Image(Width, Height: Coordinate) is new Image_Base.Image with private;

  overriding
  function Get_Pixel
    (im: in Image;
     R, C: in Coordinate)
     return Image_Base.Pixel'Class;

  overriding
  procedure Set_Pixel
    (im: in out Image;
     R, C: in Coordinate;
     px: in Image_Base.Pixel'Class);

  pragma Inline(Get_Pixel, Set_Pixel);

  overriding
  function Create_Pixel
    (im: in Image) return Image_Base.Pixel_Access;


  overriding
  function Gray_Sample  (im: in Image;
                         R, C: in Coordinate) return Sample;

  overriding
  function Red_Sample   (im: in Image;
                         R, C: in Coordinate) return Sample;
  overriding
  function Green_Sample (im: in Image;
                         R, C: in Coordinate) return Sample;
  overriding
  function Blue_Sample  (im: in Image;
                         R, C: in Coordinate) return Sample;

  pragma Inline(Gray_Sample, Red_Sample, Green_Sample, Blue_Sample);

  overriding
  procedure Set_Gray  (im: in out Image;
                       R, C: in Coordinate;
                       s: in Sample);

  overriding
  procedure Set_Red   (im: in out Image;
                       R, C: in Coordinate;
                       s: in Sample);
  overriding
  procedure Set_Green (im: in out Image;
                       R, C: in Coordinate;
                       s: in Sample);
  overriding
  procedure Set_Blue  (im: in out Image;
                       R, C: in Coordinate;
                       s: in Sample);

  pragma Inline(Set_Gray, Set_Red, Set_Green, Set_Blue);

private
  type Pixel_Matrix is
    array(Coordinate range <>, Coordinate range <>) of Pixel_Type;
  pragma Pack(Pixel_Matrix);

  type Image(Width, Height: Coordinate) is new Image_Base.Image(Width, Height) with
    record
      a: Pixel_Matrix(1..Width, 1..Height);
    end record;
end Image_IO.Image_Generic;
