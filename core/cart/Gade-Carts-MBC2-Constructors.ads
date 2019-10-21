with Gade.Carts.Memory_Contents; use Gade.Carts.Memory_Contents;

package Gade.Carts.MBC2.Constructors is

   function Create
     (Content  : ROM_Content_Access;
      Header   : Cart_Header_Access; -- Could be made a non acces argument, type needs to be separated
      RAM_Path : String)
      return MBC2_Cart_NN_Access;

private

   procedure Initialize
     (C        : out MBC2_Cart'Class;
      Content  : ROM_Content_Access;
      Header   : Cart_Header_Access;
      RAM_Path : String); --  This is going to need more args!

end Gade.Carts.MBC2.Constructors;
