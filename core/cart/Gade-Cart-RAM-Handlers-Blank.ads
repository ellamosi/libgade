package Gade.Cart.RAM.Handlers.Blank is

   type Blank_RAM_Handler_Type is new RAM_Handler_Type with private;

   type Blank_RAM_Handler_Access is access all Blank_RAM_Handler_Type;

   function Singleton return Blank_RAM_Handler_Access;

   function Create return Blank_RAM_Handler_Access renames Singleton;

   overriding
   procedure Read
     (Handler : in out Blank_RAM_Handler_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : out Byte);

   overriding
   procedure Write
     (Handler : in out Blank_RAM_Handler_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : Byte);

private

   type Blank_RAM_Handler_Type is new RAM_Handler_Type with null record;

end Gade.Cart.RAM.Handlers.Blank;
