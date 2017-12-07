private package Gade.Cartridge.ROM is

   type ROM_Only_Handler_Type is new ROM_Handler_Type with private;

   overriding
   procedure Reset (Handler : in out ROM_Only_Handler_Type) is null;

private

   type ROM_Only_Handler_Type is new ROM_Handler_Type with null record;

end Gade.Cartridge.ROM;
