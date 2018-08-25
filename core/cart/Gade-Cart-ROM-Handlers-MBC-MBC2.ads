with Gade.Cart.RAM.Handlers;

package Gade.Cart.ROM.Handlers.MBC.MBC2 is

   type MBC2_ROM_Handler_Type is new MBC_ROM_Handler_Type with private;

   type MBC2_ROM_Handler_Access is access MBC2_ROM_Handler_Type;

   function Create
     (ROM_Content : Gade.Cart.ROM.ROM_Content_Access;
      RAM_Handler : Gade.Cart.RAM.Handlers.RAM_Handler_Access)
      return MBC2_ROM_Handler_Access;

   overriding
   procedure Reset
     (Handler : in out MBC2_ROM_Handler_Type);

private

   Bank_Select_Index_Mask : constant := 16#0F#;
   Bank_Select_Flag_Mask  : constant := 16#10#;
   RAM_Enable_Mask        : constant := 16#10#;

   type MBC2_ROM_Handler_Type is new MBC_ROM_Handler_Type with null record;

   procedure Initialize
     (Handler     : out MBC2_ROM_Handler_Type'Class;
      ROM_Content : Gade.Cart.ROM.ROM_Content_Access;
      RAM_Handler : Gade.Cart.RAM.Handlers.RAM_Handler_Access);

   overriding
   procedure Select_Bank
     (Handler : in out MBC2_ROM_Handler_Type;
      Address : Bank_Select_Address;
      Value   : Byte);

   overriding
   procedure Enable_RAM
     (Handler : in out MBC2_ROM_Handler_Type;
      Address : RAM_Enable_Address;
      Value   : Byte);

end Gade.Cart.ROM.Handlers.MBC.MBC2;
