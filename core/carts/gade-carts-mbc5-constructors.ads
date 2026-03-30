with Gade.Carts.Mem.ROM; use Gade.Carts.Mem.ROM;
with Gade.Logging;
private with Gade.Carts.Mixins.MBC.Constructors;

package Gade.Carts.MBC5.Constructors is

   function Create
     (ROM_Content : ROM_Content_Access;
      Header      : Cart_Header;
      RAM_Path    : String;
      Logger      : Gade.Logging.Logger_Access) return MBC5_Cart_NN_Access;

private

   package MBC_Constructors is new MBC_Mixin.Constructors;

   procedure Initialize
     (C           : out MBC5_Cart'Class;
      ROM_Content : ROM_Content_Access;
      Header      : Cart_Header;
      RAM_Path    : String;
      Logger      : Gade.Logging.Logger_Access);

end Gade.Carts.MBC5.Constructors;
