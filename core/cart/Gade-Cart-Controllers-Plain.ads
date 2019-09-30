with Gade.GB;
with Gade.Cart.Spaces;
with Gade.Cart.Spaces.RAM;
with Gade.Cart.ROM;

package Gade.Cart.Controllers.Plain is

   --  Prototype of new way of encapsulating MBC logic

   type Cart_Handler is new Controllers.Cart_Handler with private;

   type Cart_Handler_Access is access Cart_Handler;

   --  TODO: This is going to need the cart's details
   function Create
     (ROM_Content : Cart.ROM.Content_Access;
      RAM_Size    : RAM_Size_Type;
      RAM_Path    : String) return Cart_Handler_Access;

private

   package ROM_Space is

      type Handler_Type is new Spaces.ROM.Handler_Type with private;
      type Handler_Access is access Handler_Type;

      function Create (Content : Cart.ROM.Content_Access) return Handler_Access;

      overriding
      procedure Write
        (Handler : in out Handler_Type;
         GB      : in out Gade.GB.GB_Type;
         Address : Word;
         Content : Byte) is null;

   private

      type Handler_Type is new Spaces.ROM.Handler_Type with null record;

      procedure Initialize
        (Handler     : out Handler_Type'Class;
         ROM_Content : Cart.ROM.Content_Access);

   end ROM_Space;

   package RAM_Space is

      type Handler_Type is new Spaces.RAM.Handler_Type with private;
      type Handler_Access is access Handler_Type;

      function Create
        (Size : RAM_Size_Type;
         Path : String) return Handler_Access;

   private

      type Handler_Type is new Spaces.RAM.Handler_Type with null record;

      procedure Initialize
        (Handler : out Handler_Type'Class;
         Size    : RAM_Size_Type;
         Path    : String);

   end RAM_Space;

   type Cart_Handler is new Controllers.Cart_Handler with record
      ROM_Handler : ROM_Space.Handler_Access;
      RAM_Handler : RAM_Space.Handler_Access;
      --  ROM         : Banked.ROM.Handler_Access;
      --  RAM         : Banked.RAM.Mem.Handler_Access;
   end record;

end Gade.Cart.Controllers.Plain;
