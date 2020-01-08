package Gade.Carts.Blank is

   type Blank_Cart is new Cart with private;

   type Blank_Cart_Access is access Blank_Cart'Class;

   subtype Blank_Cart_NN_Access is not null Blank_Cart_Access;

   function Singleton return Blank_Cart_NN_Access;

   overriding
   procedure Finalize (C : in out Blank_Cart) is null;

private

   type Blank_Cart is new Cart with null record;

end Gade.Carts.Blank;
