package Gade.Cart.ROM.Handlers.ROM_Only is

   type ROM_Only_Handler_Type is new ROM_Handler_Type with private;

   type ROM_Only_Handler_Access is access ROM_Only_Handler_Type;

   function Create
     (ROM_Content : Gade.Cart.ROM.ROM_Content_Access)
      return ROM_Only_Handler_Access;

   overriding
   procedure Write
     (Handler : in out ROM_Only_Handler_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : Byte) is null;

private

   type ROM_Only_Handler_Type is new ROM_Handler_Type with null record;

   procedure Initialize
     (Handler     : out ROM_Only_Handler_Type'Class;
      ROM_Content : Gade.Cart.ROM.ROM_Content_Access);

end Gade.Cart.ROM.Handlers.ROM_Only;
