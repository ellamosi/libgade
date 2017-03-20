package Image_IO.PBM is

  -- Els fitxers PBM poden tenir les dades codificades com a text (ASCII), o
  -- en binari.
  type Pixel_Value_Storage_Type is (ASCII_Data, Raw_Bits);

  Max_Value_For_Raw_Bits_File : constant := 255;

  -- Obre un fitxer PBM i en llegeix els continguts
  generic
    type Image_Type is limited private;

    -- Es crida primer per fixar la informació llegida de la capçalera.
    with procedure Set_Size
      (Image     : in out Image_Type;
       Max_Color : in     Pixel_Value_Type;
       Columns   : in     Dimension;
       Rows      : in     Dimension);

    -- Cridat per cada pixel de la imatge
    with procedure Set_Pixel
      (Image  : in out Image_Type;
       Column : in     Coordinate;
       Row    : in     Coordinate;
       Value  : in     Color_Channels_Type);

  procedure Read_PBM
    (Name  : in     String;
     Image : in out Image_Type);

  Not_A_PBM_File_Error : exception;

end Image_IO.PBM;
