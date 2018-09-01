package Gade.Cart.Spaces is

   --  Cartridge spaces define the top level handlers for the two cartridge
   --  memory mapped address spaces:
   --
   --    0000 .. 7FFF : Typically used to access the cartridge's ROM
   --    A000 .. BFFF : Typically used to access the cartridge's RAM for saves
   --
   --  While exotic cartridge types could potentially use these spaces for
   --  alternative hardware, it's convenient to just refer them by their
   --  most common usage, rather than something more precise but abstract
   --  (such as Cart.Spaces.Low and Cart.Spaces.High).

end Gade.Cart.Spaces;

