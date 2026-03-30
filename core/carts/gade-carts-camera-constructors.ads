with Gade.Carts.Mem.ROM; use Gade.Carts.Mem.ROM;
with Gade.Logging;

package Gade.Carts.Camera.Constructors is

   function Create
     (ROM_Content : ROM_Content_Access;
      Header      : Cart_Header;
      RAM_Path    : String;
      Logger      : Gade.Logging.Logger_Access) return Camera_Cart_NN_Access;

end Gade.Carts.Camera.Constructors;
