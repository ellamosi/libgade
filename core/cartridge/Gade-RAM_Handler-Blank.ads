package Gade.RAM_Handler.Blank is

   type Blank_RAM_Handler_Type is new RAM_Handler_Type with private;

private

   type Blank_RAM_Handler_Type is new RAM_Handler_Type with null record;

end Gade.RAM_Handler.Blank;
