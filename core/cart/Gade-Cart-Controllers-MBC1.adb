with Gade.Cart.RAM;

package body Gade.Cart.Controllers.MBC1 is

   ------------
   -- Create --
   ------------

   function Create
     (ROM_Content : Cart.ROM.Content_Access;
      RAM_Size    : RAM_Size_Type;
      RAM_Path    : String)
      return Cart_Handler_Access
   is
      Handler : constant Cart_Handler_Access := new Cart_Handler;
   begin
      Handler.ROM_Handler := ROM_Space.Create (Handler, ROM_Content);
      Handler.RAM_Handler := RAM_Space.Create (RAM_Size, RAM_Path);
      return Handler;
   end Create;

   ---------------
   -- ROM_Space --
   ---------------

   package body ROM_Space is

      function Create
        (Cart_Handler : Cart_Handler_Access;
         ROM_Content  : Cart.ROM.Content_Access)
         return Handler_Access
      is
         Handler : constant Handler_Access := new Handler_Type;
      begin
         ROM_Space.Initialize (Handler.all, Cart_Handler, ROM_Content);
         return Handler;
      end Create;

      procedure Initialize
        (Handler      : out Handler_Type'Class;
         Cart_Handler : Cart_Handler_Access;
         ROM_Content  : Cart.ROM.Content_Access)
      is
      begin
         Spaces.ROM.MBC.Initialize (Handler, ROM_Content);
         Handler.Cart_Handler := Cart_Handler;
         Handler.Reset;
      end Initialize;

      overriding
      procedure Reset (Handler : in out Handler_Type) is
      begin
         Handler.Banking_Mode := ROM_Space.ROM;
         Handler.Low_Bank_Select := 1;
         Handler.High_Bank_Select := 0;
         Handler.Select_ROM_Bank;
         Handler.Select_RAM_Bank;
      end Reset;

      overriding
      procedure Enable_RAM
        (Handler : in out Handler_Type;
         Address : RAM_Enable_Address;
         Value   : Byte)
      is
         pragma Unreferenced (Address);
         Mask    : constant := 16#0F#;
         Enable  : constant := 16#0A#;
         Disable : constant := 16#00#;

         Masked_Value : constant Byte := Value and Mask;
      begin
         --  TODO: Review disabling values
         case Masked_Value is
         when Enable  => Handler.Cart_Handler.RAM_Handler.Set_Enabled (True);
         when Disable => Handler.Cart_Handler.RAM_Handler.Set_Enabled (False);
         when others  => null;
         end case;
      end Enable_RAM;

      overriding
      procedure Select_Bank
        (Handler : in out Handler_Type;
         Address : Bank_Select_Address;
         Value   : Byte)
      is
      begin
         case Address is
         when Low_Bank_Select_Address  => Handler.Select_Low_Bank (Value);
         when High_Bank_Select_Address => Handler.Select_High_Bank (Value);
         end case;
      end Select_Bank;

      overriding
      procedure Write_Special
        (Handler : in out Handler_Type;
         Value   : Byte)
      is
         New_Mode : constant Banking_Mode_Type :=
           Banking_Mode_Type'Val (Value and Banking_Mode_Mask);
      begin
         Handler.Change_Banking_Mode (New_Mode);
      end Write_Special;

      procedure Change_Banking_Mode
        (Handler  : in out Handler_Type;
         New_Mode : Banking_Mode_Type)
      is
         Mode_Changes : constant Boolean := New_Mode /= Handler.Banking_Mode;
      begin
         if Mode_Changes then
            Handler.Banking_Mode := New_Mode;
            Handler.Select_ROM_Bank;
            Handler.Select_RAM_Bank;
         end if;
      end Change_Banking_Mode;

      procedure Select_Low_Bank
        (Handler : in out Handler_Type;
         Value   : Byte)
      is
      begin
         Handler.Low_Bank_Select := Low_Bank_Select_Type (Value and Low_Select_Mask);
         if Handler.Low_Bank_Select = 0 then Handler.Low_Bank_Select := 1; end if;
         Handler.Select_ROM_Bank;
      end Select_Low_Bank;

      procedure Select_High_Bank
        (Handler : in out Handler_Type;
         Value   : Byte)
      is
      begin
         Handler.High_Bank_Select := High_Bank_Select_Type (Value and High_Select_Mask);
         Handler.Select_ROM_Bank;
         Handler.Select_RAM_Bank;
      end Select_High_Bank;

      procedure Select_ROM_Bank (Handler : in out Handler_Type) is
         Low_Bank_Index, High_Bank_Index : Cart.ROM.Bank_Index;
         Low_Part : constant Natural := Natural (Handler.Low_Bank_Select);
         High_Part : constant Natural := Natural (Handler.High_Bank_Select);
      begin
         High_Bank_Index := Cart.ROM.Bank_Index (High_Part * 2**5 + Low_Part);
         case Handler.Banking_Mode is
         when ROM => Low_Bank_Index := 0;
         when RAM => Low_Bank_Index := Cart.ROM.Bank_Index (High_Part * 2**5);
         end case;
         Handler.Switch_Banks (0, Low_Bank_Index);
         Handler.Switch_Banks (1, High_Bank_Index);
      end Select_ROM_Bank;

      procedure Select_RAM_Bank (Handler : in out Handler_Type) is
         Index : Cart.RAM.Bank_Index;
      begin
         case Handler.Banking_Mode is
         when ROM => Index := 0;
         when RAM => Index := Cart.RAM.Bank_Index (Handler.High_Bank_Select);
         end case;
         Handler.Cart_Handler.RAM_Handler.Switch_Banks (Index);
      end Select_RAM_Bank;

   end ROM_Space;

   ---------------
   -- RAM_Space --
   ---------------

   package body RAM_Space is

      function Create
        (Size : RAM_Size_Type; Path : String) return Handler_Access
      is
         Handler : constant Handler_Access := new Handler_Type;
      begin
         RAM_Space.Initialize (Handler.all, Size, Path);
         return Handler;
      end Create;

      procedure Initialize
        (Handler : out Handler_Type'Class;
         Size    : RAM_Size_Type;
         Path    : String)
      is
      begin
         Spaces.RAM.Banked.Initialize (Handler, Size, Path);
      end Initialize;

   end RAM_Space;

end Gade.Cart.Controllers.MBC1;
