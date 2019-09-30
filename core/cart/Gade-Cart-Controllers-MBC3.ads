with Gade.Cart.Banked.RAM.RTC;
with Gade.Cart.Spaces.ROM.MBC;
with Gade.Cart.Spaces.RAM.Banked;
with Gade.Cart.ROM;
with Gade.Cart.RAM;
with Gade.Cart.RTC;

package Gade.Cart.Controllers.MBC3 is

   type Cart_Handler is new Controllers.Cart_Handler with private;

   type Cart_Handler_Access is access Cart_Handler;

   --  TODO: This is going to need the cart's details
   function Create
     (ROM_Content : Cart.ROM.Content_Access;
      RAM_Size    : RAM_Size_Type;
      RAM_Path    : String) return Cart_Handler_Access;

   --  TODO: Remove
   procedure Test;

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
      use Spaces.ROM.MBC;

      subtype ROM_Bank_Select_Address is
        Bank_Select_Address range 16#2000# .. 16#3FFF#;

      subtype RAM_Bank_Select_Address is
        Spaces.ROM.MBC.Bank_Select_Address range 16#4000# .. 16#5FFF#;

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
         Address : Bank_Select_Address;
         Value   : Byte);

      overriding
      procedure Enable_RAM
        (Handler : in out Handler_Type;
         Address : RAM_Enable_Address;
         Value   : Byte);

      overriding
      procedure Write_Special
        (Handler : in out Handler_Type;
         Value   : Byte);

      procedure Select_ROM_Bank
        (Handler : in out Handler_Type;
         Value   : Byte);

      procedure Select_RAM_Bank
        (Handler : in out Handler_Type;
         Value   : Byte);

   end ROM_Space;

   package RAM_Space is
      use RAM;

      type Handler_Type is new Spaces.RAM.Banked.Handler_Type with private;
      type Handler_Access is access Handler_Type;

      function Create
        (Size : RAM_Size_Type;
         Path : String) return Handler_Access;

      overriding
      procedure Switch_Banks
        (Handler : in out Handler_Type;
         Index   : Bank_Index);

      overriding
      procedure Save (Handler : Handler_Type);

   private

      type Handler_Type is new Spaces.RAM.Banked.Handler_Type with record
         RTC_Bank     : Banked.RAM.RTC.Handler_Access;
         Cart_Handler : Cart_Handler_Access;
      end record;

      procedure Initialize
        (Handler : out Handler_Type'Class;
         Size    : RAM_Size_Type;
         Path    : String);

   end RAM_Space;

   package Base is new Controllers.Base_Cart_Handler
     (ROM_Space_Handler        => ROM_Space.Handler_Type,
      ROM_Space_Handler_Access => ROM_Space.Handler_Access,
      RAM_Space_Handler        => RAM_Space.Handler_Type,
      RAM_Space_Handler_Access => RAM_Space.Handler_Access);

   type RTC_Access is access RTC.Clock;

   type Cart_Handler is new Controllers.Cart_Handler with record
      ROM_Handler : ROM_Space.Handler_Access;
      RAM_Handler : RAM_Space.Handler_Access;
      RTC         : RTC_Access;
      --  ROM         : Banked.ROM.Handler_Access;
      --  RAM         : Banked.RAM.Mem.Handler_Access;
   end record;

end Gade.Cart.Controllers.MBC3;
