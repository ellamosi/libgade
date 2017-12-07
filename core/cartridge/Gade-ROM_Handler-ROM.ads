package Gade.ROM_Handler.ROM is

   type ROM_Only_Handler_Type is new ROM_Handler_Type with private;

private

   type ROM_Only_Handler_Type is new ROM_Handler_Type with null record;

end Gade.ROM_Handler.ROM;
