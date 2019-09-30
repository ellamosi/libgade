with Gade.GB;
with Gade.Cart.Spaces.ROM.MBC;
with Gade.Cart.Spaces.RAM;
with Gade.Cart.ROM;
with Gade.Cart.Banked.RAM;
with Gade.Cart.Banked.RAM.MBC2;

package Gade.Cart.Controllers.MBC2 is

   type Cart_Handler is new Controllers.Cart_Handler with private;

   type Cart_Handler_Access is access Cart_Handler;

   --  TODO: This is going to need the cart's details
   function Create
     (ROM_Content : Cart.ROM.Content_Access;
      RAM_Path    : String) return Cart_Handler_Access;

private

   package ROM_Space is

      type Handler_Type is new Spaces.ROM.MBC.Handler_Type with private;
      type Handler_Access is access Handler_Type;

      function Create
        (Cart_Handler : Cart_Handler_Access;
         ROM_Content  : Cart.ROM.Content_Access)
         return Handler_Access;

      overriding
      procedure Reset (Handler : in out Handler_Type);

   private

      Select_Mask : constant := 16#0F#;

      subtype Bank_Select_Address is
         Spaces.ROM.MBC.Bank_Select_Address range 16#2000# .. 16#3FFF#;

      type Handler_Type is new Spaces.ROM.MBC.Handler_Type with record
         Cart_Handler : Cart_Handler_Access;
      end record;

      procedure Initialize
        (Handler      : out Handler_Type'Class;
         Cart_Handler : Cart_Handler_Access;
         ROM_Content  : Cart.ROM.Content_Access);

      overriding
      procedure Select_Bank
        (Handler : in out Handler_Type;
         Address : Spaces.ROM.MBC.Bank_Select_Address;
         Value   : Byte);

      overriding
      procedure Enable_RAM
        (Handler : in out Handler_Type;
         Address : Spaces.ROM.MBC.RAM_Enable_Address;
         Value   : Byte);

   end ROM_Space;

   package RAM_Space is
      use Spaces.RAM;

      type Handler_Type is new Spaces.RAM.Handler_Type with private;
      type Handler_Access is access Handler_Type;

      function Create (Path : String) return Handler_Access;

      overriding
      procedure Reset (Handler : in out Handler_Type);

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

      overriding
      procedure Set_Enabled
        (Handler : in out Handler_Type;
         Enabled : Boolean);

      overriding
      procedure Save (Handler : Handler_Type);

   private

      type Handler_Type is new Spaces.RAM.Handler_Type with record
         Current : Cart.Banked.RAM.Handler_Access;
         Memory  : Cart.Banked.RAM.MBC2.Handler_Access;
         Enabled : Boolean;
      end record;

      procedure Initialize
        (Handler : out Handler_Type'Class;
         Path    : String);

      procedure Enable (Handler : in out Handler_Type);

      procedure Disable (Handler : in out Handler_Type);

   end RAM_Space;

   type Cart_Handler is new Controllers.Cart_Handler with record
      ROM_Handler : ROM_Space.Handler_Access;
      RAM_Handler : RAM_Space.Handler_Access;
      --  ROM         : Banked.ROM.Handler_Access;
      --  RAM         : Banked.RAM.Mem.Handler_Access;
   end record;

end Gade.Cart.Controllers.MBC2;
