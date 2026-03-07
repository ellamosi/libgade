with Interfaces.C; use Interfaces.C;

package Gade.Cartridge_Info is

   type Controller_Type is
     (None,
      MBC1,
      MBC2,
      MBC3,
      MBC5,
      MBC6,
      MBC7,
      MM01,
      Pocket_Camera,
      Bandai_TAMA5,
      Huds_on_Huc_1,
      Huds_on_Huc_3);

   type Cartridge_Info_Type is record
      File_Name  : char_array (0 .. 255);
      Controller : Controller_Type;
      Name       : char_array (0 .. 14);
      ROM_Size   : Natural; -- In Bytes
      RAM_Size   : Natural; -- In Bytes
      Battery    : Boolean;
   end record;

end Gade.Cartridge_Info;
