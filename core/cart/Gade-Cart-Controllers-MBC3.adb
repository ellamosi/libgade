with Ada.Text_IO; use Ada.Text_IO;

package body Gade.Cart.Controllers.MBC3 is

   --  TODO: remove
   procedure Test is
      use ROM_Space;

      CHA : constant Cart_Handler_Access := null;
      HA : Handler_Access;
      pragma Unreferenced (HA);
      RCA : constant Cart.ROM.Content_Access := new Cart.ROM.Content (0 .. Cart.ROM.Bank_Size * 2 - 1);
   begin
      HA := ROM_Space.Create (CHA, RCA);
   end Test;

   function Create
     (ROM_Content : Cart.ROM.Content_Access;
      RAM_Size    : RAM_Size_Type;
      RAM_Path    : String) return Cart_Handler_Access
   is
      Handler : constant Cart_Handler_Access := new Cart_Handler;
   begin
      Handler.ROM_Handler := ROM_Space.Create (Handler, ROM_Content);
      Handler.RAM_Handler := RAM_Space.Create (RAM_Size, RAM_Path);
      return Handler;
   end Create;

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
         Put_Line ("MBC3 Initialize");

         Spaces.ROM.MBC.Initialize (Handler, ROM_Content);
         Handler.Cart_Handler := Cart_Handler;
         Handler.Reset;
      end Initialize;

      overriding procedure Reset
        (Handler : in out Handler_Type)
      is
      begin
         null;
         --  Not visible because not a child package
         --  Handler.Cart_Handler.ROM_Handler.Reset;
         --  Handler.Switch_Banks (1, 1);
         --  Handler.Addressable_Banks (1).Set_Bank (1);
      end Reset;

      overriding procedure Select_Bank
        (Handler : in out Handler_Type;
         Address : Bank_Select_Address;
         Value   : Byte)
      is
      begin
         case Address is
            when ROM_Bank_Select_Address => Handler.Select_ROM_Bank (Value);
            when RAM_Bank_Select_Address => Handler.Select_RAM_Bank (Value);
         end case;
      end Select_Bank;

      overriding procedure Enable_RAM
        (Handler : in out Handler_Type;
         Address : RAM_Enable_Address;
         Value   : Byte)
      is
         pragma Unreferenced (Address);
         Mask       : constant := 16#0F#;
         Pattern    : constant := 16#0A#;
         RAM_Enable : constant Boolean := (Value and Mask) = Pattern;
      begin
         Handler.Cart_Handler.RAM_Handler.Set_Enabled (RAM_Enable);
      end Enable_RAM;

      overriding procedure Write_Special
        (Handler : in out Handler_Type;
         Value   : Byte)
      is
         pragma Unreferenced (Handler, Value);
      begin
         --  Latch
         Put_Line ("Latch: Unimplemented");
      end Write_Special;

      procedure Select_ROM_Bank
        (Handler : in out Handler_Type;
         Value   : Byte)
      is
         use ROM;

         Bank_Mask : constant Byte := 16#7F#;
         Index : Cart.ROM.Bank_Index;
      begin
         --  FIXME: Index casting should not be needed?
         Index := Cart.ROM.Bank_Index (Value and Bank_Mask);
         if Index = 0 then Index := 1; end if;
         Handler.Cart_Handler.ROM_Handler.Switch_Banks (1, Index);
      end Select_ROM_Bank;

      procedure Select_RAM_Bank
        (Handler : in out Handler_Type;
         Value   : Byte)
      is
         Bank_Mask : constant Byte := 16#0F#;
         Index : Cart.RAM.Bank_Index;
      begin
         Index := Cart.RAM.Bank_Index (Value and Bank_Mask);
         Handler.Cart_Handler.RAM_Handler.Switch_Banks (Index);
      end Select_RAM_Bank;

   end ROM_Space;

   package body RAM_Space is
      function Create
        (Size : RAM_Size_Type;
         Path : String) return Handler_Access
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
         Handler.RTC_Bank := new Cart.Banked.RAM.RTC.Handler_Type;
         Cart.Banked.RAM.RTC.Initialize (Handler.RTC_Bank.all, Path);
      end Initialize;

      overriding
      procedure Switch_Banks
        (Handler : in out Handler_Type;
         Index   : Bank_Index)
      is
         use Gade.Cart.RTC;
      begin
         case Index is
         when 16#00# .. 16#07# =>
            Handler.Memory_Bank.Set_Bank (Index);
            Handler.Current_Bank :=
              Cart.Banked.RAM.Handler_Access (Handler.Memory_Bank);
         when 16#08# .. 16#0C# =>
            Handler.RTC_Bank.Set_Register (Register'Val (Index - 16#08#));
            Handler.Current_Bank :=
              Cart.Banked.RAM.Handler_Access (Handler.RTC_Bank);
         when others => null;
         end case;
      end Switch_Banks;

      overriding
      procedure Save (Handler : Handler_Type) is
      begin
         Handler.Memory_Bank.Save;
      end Save;
   end RAM_Space;

end Gade.Cart.Controllers.MBC3;
