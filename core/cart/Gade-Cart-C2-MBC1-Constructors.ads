with Gade.Cart.Memory_Contents; use Gade.Cart.Memory_Contents;

package Gade.Cart.C2.MBC1.Constructors is

   function Create
     (Content  : ROM_Content_Access;
      RAM_Path : String)
      return MBC1_Cart_NN_Access;

private

   procedure Initialize
     (C        : out MBC1_Cart'Class;
      Content  : ROM_Content_Access;
      RAM_Path : String); --  This is going to need more args!

end Gade.Cart.C2.MBC1.Constructors;
