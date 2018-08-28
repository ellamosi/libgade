package Gade.Cart.ROM_Space.ROM_Only is

   type ROM_Only_Space_Type is new ROM_Space_Type with private;

   type ROM_Only_Space_Access is access ROM_Only_Space_Type;

   function Create
     (ROM_Content : Gade.Cart.ROM.ROM_Content_Access)
      return ROM_Only_Space_Access;

   overriding
   procedure Write
     (Handler : in out ROM_Only_Space_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : Byte) is null;

private

   type ROM_Only_Space_Type is new ROM_Space_Type with null record;

   procedure Initialize
     (Space       : out ROM_Only_Space_Type'Class;
      ROM_Content : Gade.Cart.ROM.ROM_Content_Access);

end Gade.Cart.ROM_Space.ROM_Only;
