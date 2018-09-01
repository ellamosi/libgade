package Gade.Cart.Spaces.ROM.Plain is

   type Handler_Type is new ROM.Handler_Type with private;
   type Handler_Access is access Handler_Type;

   function Create
     (ROM_Content : Gade.Cart.ROM.ROM_Content_Access)
      return Handler_Access;

   overriding
   procedure Write
     (Handler : in out Handler_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : Byte) is null;

private

   type Handler_Type is new ROM.Handler_Type with null record;

   procedure Initialize
     (Handler     : out Handler_Type'Class;
      ROM_Content : Gade.Cart.ROM.ROM_Content_Access);

end Gade.Cart.Spaces.ROM.Plain;
