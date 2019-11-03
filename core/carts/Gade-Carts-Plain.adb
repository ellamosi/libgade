package body Gade.Carts.Plain is

   overriding
   procedure Reset (C : in out Plain_Cart) is
   begin
      C.Reset_ROM;
      C.Reset_RAM;
   end Reset;

end Gade.Carts.Plain;
