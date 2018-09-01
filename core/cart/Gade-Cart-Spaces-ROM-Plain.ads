package Gade.Cart.Spaces.ROM.Plain is

   type Plain_ROM_Space_Type is new ROM_Space_Type with private;

   type Plain_ROM_Space_Access is access Plain_ROM_Space_Type;

   function Create
     (ROM_Content : Gade.Cart.ROM.ROM_Content_Access)
      return Plain_ROM_Space_Access;

   overriding
   procedure Write
     (Handler : in out Plain_ROM_Space_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : Byte) is null;

private

   type Plain_ROM_Space_Type is new ROM_Space_Type with null record;

   procedure Initialize
     (Space       : out Plain_ROM_Space_Type'Class;
      ROM_Content : Gade.Cart.ROM.ROM_Content_Access);

end Gade.Cart.Spaces.ROM.Plain;
