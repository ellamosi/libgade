package Gade.Cart.Spaces.RAM.Blank is

   type Handler_Type is new RAM.Handler_Type with private;
   type Handler_Access is access all Handler_Type;

   function Singleton return Handler_Access;

   function Create return Handler_Access renames Singleton;

   overriding
   procedure Read
     (Handler : in out Handler_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : out Byte);

   overriding
   procedure Write
     (Handler : in out Handler_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : Byte);

private

   type Handler_Type is new RAM.Handler_Type with null record;

end Gade.Cart.Spaces.RAM.Blank;
