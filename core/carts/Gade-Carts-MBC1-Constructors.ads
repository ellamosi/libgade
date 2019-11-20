with Gade.Carts.Memory_Contents; use Gade.Carts.Memory_Contents;
private with Gade.Carts.Mixins.MBC.Constructors;

package Gade.Carts.MBC1.Constructors is

   function Create
     (ROM_Content : ROM_Content_Access;
      Header      : Cart_Header_Access; -- Could be made a non acces argument, type needs to be separated
      RAM_Path    : String)
      return MBC1_Cart_NN_Access;

private

   package MBC_Constructors is new MBC_Mixin.Constructors;

   procedure Initialize
     (C           : out MBC1_Cart'Class;
      ROM_Content : ROM_Content_Access;
      Header      : Cart_Header_Access;
      RAM_Path    : String); --  This is going to need more args!

end Gade.Carts.MBC1.Constructors;
