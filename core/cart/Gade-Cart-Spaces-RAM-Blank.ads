package Gade.Cart.Spaces.RAM.Blank is

   type Blank_RAM_Space_Type is new RAM_Space_Type with private;

   type Blank_RAM_Space_Access is access all Blank_RAM_Space_Type;

   function Singleton return Blank_RAM_Space_Access;

   function Create return Blank_RAM_Space_Access renames Singleton;

   overriding
   procedure Read
     (Space   : in out Blank_RAM_Space_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : out Byte);

   overriding
   procedure Write
     (Space   : in out Blank_RAM_Space_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : Byte);

private

   type Blank_RAM_Space_Type is new RAM_Space_Type with null record;

end Gade.Cart.Spaces.RAM.Blank;
