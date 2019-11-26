with Gade.Carts.Mem.ROM; use Gade.Carts.Mem.ROM;
private with Gade.Carts.Mixins.MBC.Constructors;

package Gade.Carts.MBC1.Constructors is

   function Create
     (ROM_Content : ROM_Content_Access;
      Header      : Cart_Header;
      RAM_Path    : String)
      return MBC1_Cart_NN_Access;

private

   package MBC_Constructors is new MBC_Mixin.Constructors;

   procedure Initialize
     (C           : out MBC1_Cart'Class;
      ROM_Content : ROM_Content_Access;
      Header      : Cart_Header;
      RAM_Path    : String);

end Gade.Carts.MBC1.Constructors;
