with Ada.IO_Exceptions;

package Image_IO is

  type Sample is new Natural;
  type Pixel_Value_Type is new Natural;

  type Color_Channels_Type is
    record
      Red   : Pixel_Value_Type;
      Green : Pixel_Value_Type;
      Blue  : Pixel_Value_Type;
    end record;

  -- Can be overriden by the different file formats, if they differ from the
  -- defaults.
  type Dimension is new Positive;

  -- Indexs de les imatges, el primer pixel és: (1, 1)
  type Coordinate is new Dimension;

  type Column_Index_Type is new Coordinate;
  type Row_Index_Type is new Coordinate;

  -- Tipus per als valors dels channels
  type Unsigned_1  is mod 2**1;
  for Unsigned_1'Size use 1;

  type Unsigned_2  is mod 2**2;
  for Unsigned_2'Size use 2;

  type Unsigned_4  is mod 2**4;
  for Unsigned_4'Size use 4;

  -- La resta fem servir els tipus Unsigned de mida 8, 16 i 32 definits pel
  -- llenguatge en Interfaces

  type Bit_Depth is (Depth_1, Depth_2, Depth_4, Depth_8, Depth_16, Depth_32);

  -- Definim tot de tipus d'excepcions comunes per la llibreria de maneig
  -- d'imatges, molts dels quals són re-anomenaments dels tipus d'error
  -- estandard que es faran servir en condicions de significat similar.
  Status_Error : exception renames Ada.IO_Exceptions.Status_Error;
  Mode_Error   : exception renames Ada.IO_Exceptions.Mode_Error;
  Name_Error   : exception renames Ada.IO_Exceptions.Name_Error;
  Use_Error    : exception renames Ada.IO_Exceptions.Use_Error;
  Device_Error : exception renames Ada.IO_Exceptions.Device_Error;
  End_Error    : exception renames Ada.IO_Exceptions.End_Error;
  Data_Error   : exception renames Ada.IO_Exceptions.Data_Error;

  -- Indica ús incorrecte d'algun procediment o funció, per exemple intentar
  -- llegir un component de color d'una imatge en blanc i negre
  Call_Error   : exception;

end Image_IO;
