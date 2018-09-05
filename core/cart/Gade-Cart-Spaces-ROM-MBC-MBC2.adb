with Gade.Cart.ROM; use Gade.Cart.ROM;

package body Gade.Cart.Spaces.ROM.MBC.MBC2 is

   function Create
     (ROM_Content : Cart.ROM.Content_Access;
      RAM_Handler : Spaces.RAM.Handler_Access)
      return Handler_Access
   is
      Handler : constant Handler_Access := new Handler_Type;
   begin
      MBC2.Initialize (Handler.all, ROM_Content, RAM_Handler);
      return Handler;
   end Create;

   procedure Initialize
     (Handler     : out Handler_Type'Class;
      ROM_Content : Cart.ROM.Content_Access;
      RAM_Handler : Spaces.RAM.Handler_Access)
   is
   begin
      MBC.Handler_Type (Handler).Initialize (ROM_Content, RAM_Handler);
      Handler.Reset;
   end Initialize;

   overriding
   procedure Reset (Handler : in out Handler_Type) is
   begin
      Handler.Switch_Banks (1, 1);
   end Reset;

   overriding
   procedure Enable_RAM
     (Handler : in out Handler_Type;
      Address : RAM_Enable_Address;
      Value   : Byte)
   is
      pragma Unreferenced (Value);

      Accept_Mask  : constant Word := 16#0100#;
      Enable_Mask  : constant Word := 16#00FF#;
      Enable_Value : constant Word := 16#000A#;
   begin
      if (Address and Accept_Mask) = 0 then
         case Address and Enable_Mask is
            when Enable_Value => Handler.RAM_Handler.Set_Enabled (True);
            when others       => Handler.RAM_Handler.Set_Enabled (False);
         end case;
      end if;
   end Enable_RAM;

   overriding
   procedure Select_Bank
     (Handler : in out Handler_Type;
      Address : MBC.Bank_Select_Address;
      Value   : Byte)
   is
      pragma Unreferenced (Value);

      Accept_Mask  : constant Word := 16#0100#;

      Index : Cart.ROM.Bank_Index;
   begin
      if (Address and Accept_Mask) = 1 and Address in Bank_Select_Address then
         Index := Cart.ROM.Bank_Index (Address and Select_Mask);
         Handler.Switch_Banks (1, Index);
      end if;
   end Select_Bank;

end Gade.Cart.Spaces.ROM.MBC.MBC2;
