with Image_IO.Image_Generic, Image_IO.Pixel_Generic, Image_IO.Image_Base;
use  Image_IO.Image_Base;

with Interfaces;
use Interfaces;

package Image_IO.Image is

  package Pixel_1_bit  is new Pixel_Generic(Unsigned_1);
  package Pixel_2_bit  is new Pixel_Generic(Unsigned_2);
  package Pixel_4_bit  is new Pixel_Generic(Unsigned_4);
  package Pixel_8_bit  is new Pixel_Generic(Unsigned_8);
  package Pixel_16_bit is new Pixel_Generic(Unsigned_16);
  package Pixel_32_bit is new Pixel_Generic(Unsigned_32);

  package Image_Grayscale_1_bit  is
    new Image_Generic(Pixel_1_bit.Pixel_Grayscale);
  package Image_Grayscale_2_bit  is
    new Image_Generic(Pixel_2_bit.Pixel_Grayscale);
  package Image_Grayscale_4_bit  is
    new Image_Generic(Pixel_4_bit.Pixel_Grayscale);
  package Image_Grayscale_8_bit  is
    new Image_Generic(Pixel_8_bit.Pixel_Grayscale);
  package Image_Grayscale_16_bit is
    new Image_Generic(Pixel_16_bit.Pixel_Grayscale);
  package Image_Grayscale_32_bit is
    new Image_Generic(Pixel_32_bit.Pixel_Grayscale);

  package Image_Color_1_bit  is new Image_Generic(Pixel_1_bit.Pixel_Color);
  package Image_Color_2_bit  is new Image_Generic(Pixel_2_bit.Pixel_Color);
  package Image_Color_4_bit  is new Image_Generic(Pixel_4_bit.Pixel_Color);
  package Image_Color_8_bit  is new Image_Generic(Pixel_8_bit.Pixel_Color);
  package Image_Color_16_bit is new Image_Generic(Pixel_16_bit.Pixel_Color);
  package Image_Color_32_bit is new Image_Generic(Pixel_32_bit.Pixel_Color);

  type Image_Access is access Image_Base.Image'Class;

  function Allocate_Image(Width, Height: Coordinate;
                          Color: Boolean;
                          Max_Channel_Value: Pixel_Value_Type)
                          return Image_Access;

end Image_IO.Image;
