with Gade.Logging;

package Gade.Carts.Constructors is

   procedure Initialize
     (C          : out Cart;
      Save_Path  : String;
      Persistent : Boolean;
      Logger     : Gade.Logging.Logger_Access);

end Gade.Carts.Constructors;
