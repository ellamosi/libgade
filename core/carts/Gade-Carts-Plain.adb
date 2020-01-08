package body Gade.Carts.Plain is

   overriding
   procedure Finalize (C : in out Plain_Cart) is
   begin
      ROM_RAM_Cart (C).Finalize;
      Cart (C).Finalize;
   end Finalize;

end Gade.Carts.Plain;
