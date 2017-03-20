with Image_IO.Image_Base;
use  Image_IO.Image_Base;

generic
  type Channel_Type is (<>);
package Image_IO.Pixel_Generic is
  type Pixel_Grayscale is new Pixel with
    record
      gr: Channel_Type;
    end record;

  overriding
  function Gray_Sample  (px: Pixel_Grayscale) return Sample;

  overriding
  procedure Set_Gray  (px: in out Pixel_Grayscale; s: in Sample);

  pragma Inline(Gray_Sample, Set_Gray);

  type Pixel_Color is new Pixel with
    record
      r, g, b: Channel_Type;
    end record;

  overriding
  function Red_Sample   (px: Pixel_Color) return Sample;
  overriding
  function Green_Sample (px: Pixel_Color) return Sample;
  overriding
  function Blue_Sample  (px: Pixel_Color) return Sample;

  overriding
  procedure Set_Red   (px: in out Pixel_Color; s: in Sample);
  overriding
  procedure Set_Green (px: in out Pixel_Color; s: in Sample);
  overriding
  procedure Set_Blue  (px: in out Pixel_Color; s: in Sample);

  pragma Inline(Red_Sample, Green_Sample, Blue_Sample,
                Set_Red, Set_Green, Set_Blue);

end Image_IO.Pixel_Generic;
