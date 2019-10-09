with Gade.Cart.Spaces.ROM;
with Gade.Cart.Spaces.RAM;

package Gade.Cart.Controllers is

   type Cart_Handler is abstract tagged null record;

   type Cart_Handler_Access is access Cart_Handler;

   procedure Read_ROM
     (Handler : in out Cart_Handler;
      Address : Word;
      Content : out Byte);

   procedure Write_ROM
     (Handler : in out Cart_Handler;
      Address : Word;
      Content : Byte);

   procedure Read_RAM
     (Handler : in out Cart_Handler;
      Address : Word;
      Content : out Byte);

   procedure Write_RAM
     (Handler : in out Cart_Handler;
      Address : Word;
      Content : Byte);

private

   --  FIXME: This is a prototype in development to attempt to further reduce
   --  some common elements of type definitions while maintaining concrete
   --  types and non dispatching calls
   generic
--        type Cart_Components;
--        type Cart_Components_Access is access Cart_Components;

      type ROM_Space_Handler is new Gade.Cart.Spaces.ROM.Handler_Type with private;
      type ROM_Space_Handler_Access is access ROM_Space_Handler;

      type RAM_Space_Handler is new Gade.Cart.Spaces.RAM.Handler_Type with private;
      type RAM_Space_Handler_Access is access RAM_Space_Handler;
   package Base_Cart_Handler is

      type Cart_Handler is new Controllers.Cart_Handler with private;

   private

      type Cart_Handler is new Controllers.Cart_Handler with record
         ROM_Space  : ROM_Space_Handler_Access;
         RAM_Space  : RAM_Space_Handler_Access;
      end record;

   end Base_Cart_Handler;

end Gade.Cart.Controllers;
