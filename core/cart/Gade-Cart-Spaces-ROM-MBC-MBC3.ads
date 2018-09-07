with Gade.Cart.Spaces.RAM;

package Gade.Cart.Spaces.ROM.MBC.MBC3 is

   type Handler_Type is new MBC.Handler_Type with private;
   type Handler_Access is access Handler_Type;

   function Create
     (ROM_Content : Cart.ROM.Content_Access;
      RAM_Handler : Spaces.RAM.Handler_Access)
      return Handler_Access;

   overriding
   procedure Reset (Handler : in out Handler_Type);

private

   subtype ROM_Bank_Select_Address is
     Bank_Select_Address range 16#2000# .. 16#3FFF#;

   subtype RAM_Bank_Select_Address is
     Bank_Select_Address range 16#4000# .. 16#5FFF#;

   type Handler_Type is new MBC.Handler_Type with record
      null;
   end record;

   procedure Initialize
     (Handler     : out Handler_Type'Class;
      ROM_Content : Cart.ROM.Content_Access;
      RAM_Handler : Spaces.RAM.Handler_Access);

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

end Gade.Cart.Spaces.ROM.MBC.MBC3;
