package body Gade.Carts.Mixins.ROM_RAM is

   overriding
   procedure Reset (C : in out ROM_RAM_Cart) is
   begin
      C.Reset_ROM;
      C.Reset_RAM;
   end Reset;

end Gade.Carts.Mixins.ROM_RAM;
