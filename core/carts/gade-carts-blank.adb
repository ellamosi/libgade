package body Gade.Carts.Blank is

   Singleton_Instance : constant Blank_Cart_NN_Access := new Blank_Cart;

   function Singleton return Blank_Cart_NN_Access is
   begin
      return Singleton_Instance;
   end Singleton;

end Gade.Carts.Blank;
