with Gade.Cart.Spaces.ROM.MBC;
with Gade.Cart.Spaces.RAM.Banked;
with Gade.Cart.ROM;

package Gade.Cart.Controllers.MBC1 is

   type Cart_Handler is new Controllers.Cart_Handler with private;

   type Cart_Handler_Access is access Cart_Handler;

   --  TODO: This is going to need the cart's details
   function Create
     (ROM_Content : Cart.ROM.Content_Access;
      RAM_Size    : RAM_Size_Type;
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
      use Spaces.ROM.MBC;

      type Banking_Mode_Type is (ROM, RAM);
      for Banking_Mode_Type use
        (ROM => 0,
         RAM => 1);

      Banking_Mode_Mask : constant := 16#01#;

      type Low_Bank_Select_Type  is mod 2**5;
      type High_Bank_Select_Type is mod 2**2;

      Low_Select_Mask  : constant := 16#1F#;
      High_Select_Mask : constant := 16#03#;

      subtype Low_Bank_Select_Address is
        Bank_Select_Address range 16#2000# .. 16#3FFF#;

      subtype High_Bank_Select_Address is
        Bank_Select_Address range 16#4000# .. 16#5FFF#;

      type Handler_Type is new Spaces.ROM.MBC.Handler_Type with record
         Cart_Handler     : Cart_Handler_Access;
         Banking_Mode     : Banking_Mode_Type;
         Low_Bank_Select  : Low_Bank_Select_Type;
         High_Bank_Select : High_Bank_Select_Type;
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

      procedure Change_Banking_Mode
        (Handler  : in out Handler_Type;
         New_Mode : Banking_Mode_Type);

      procedure Select_Low_Bank
        (Handler : in out Handler_Type;
         Value   : Byte);

      procedure Select_High_Bank
        (Handler : in out Handler_Type;
         Value   : Byte);

      procedure Select_ROM_Bank (Handler : in out Handler_Type);

      procedure Select_RAM_Bank (Handler : in out Handler_Type);

   end ROM_Space;

   package RAM_Space is

      type Handler_Type is new Spaces.RAM.Banked.Handler_Type with private;
      type Handler_Access is access Handler_Type;

      function Create
        (Size : RAM_Size_Type;
         Path : String) return Handler_Access;

   private

      type Handler_Type is new Spaces.RAM.Banked.Handler_Type with null record;

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

end Gade.Cart.Controllers.MBC1;
