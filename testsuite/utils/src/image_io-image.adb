package body Image_IO.Image is

  function Allocate_Image(Width, Height: Coordinate;
                          Color: Boolean;
                          Max_Channel_Value: Pixel_Value_Type)
                          return Image_Access is
    n : Unsigned_32;
    img : Image_Access;
  begin
    n := Unsigned_32(Max_Channel_Value);
    case n is
      when Unsigned_32(Unsigned_1'First)..Unsigned_32(Unsigned_1'Last) =>
        if Color then
          img := new Image_Color_1_bit.Image(Width, Height);
        else
          img := new Image_Grayscale_1_bit.Image(Width, Height);
        end if;
      when Unsigned_32(Unsigned_1'Last)+1..Unsigned_32(Unsigned_2'Last) =>
        if Color then
          img := new Image_Color_2_bit.Image(Width, Height);
        else
          img := new Image_Grayscale_2_bit.Image(Width, Height);
        end if;
      when Unsigned_32(Unsigned_2'Last)+1..Unsigned_32(Unsigned_4'Last) =>
        if Color then
          img := new Image_Color_4_bit.Image(Width, Height);
        else
          img := new Image_Grayscale_4_bit.Image(Width, Height);
        end if;
      when Unsigned_32(Unsigned_4'Last)+1..Unsigned_32(Unsigned_8'Last) =>
        if Color then
          img := new Image_Color_8_bit.Image(Width, Height);
        else
          img := new Image_Grayscale_8_bit.Image(Width, Height);
        end if;
      when Unsigned_32(Unsigned_8'Last)+1..Unsigned_32(Unsigned_16'Last) =>
        if Color then
          img := new Image_Color_16_bit.Image(Width, Height);
        else
          img := new Image_Grayscale_16_bit.Image(Width, Height);
        end if;
      when Unsigned_32(Unsigned_16'Last)+1..Unsigned_32(Unsigned_32'Last) =>
        if Color then
          img := new Image_Color_32_bit.Image(Width, Height);
        else
          img := new Image_Grayscale_32_bit.Image(Width, Height);
        end if;
    end case;
    return img;
  end Allocate_Image;

end Image_IO.Image;
