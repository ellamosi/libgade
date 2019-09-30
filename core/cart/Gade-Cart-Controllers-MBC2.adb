with Gade.Cart.Banked.RAM.Blank;

package body Gade.Cart.Controllers.MBC2 is

   ------------
   -- Create --
   ------------

   function Create
     (ROM_Content : Cart.ROM.Content_Access;
      RAM_Path    : String) return Cart_Handler_Access
   is
      Handler : constant Cart_Handler_Access := new Cart_Handler;
   begin
      Handler.ROM_Handler := ROM_Space.Create (Handler, ROM_Content);
      Handler.RAM_Handler := RAM_Space.Create (RAM_Path);
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
         Initialize (Handler.all, Cart_Handler, ROM_Content);
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
         Handler.Switch_Banks (0, 0);
         Handler.Switch_Banks (1, 1);
      end Reset;

      overriding
      procedure Enable_RAM
        (Handler : in out Handler_Type;
         Address : Spaces.ROM.MBC.RAM_Enable_Address;
         Value   : Byte)
      is
         Accept_Mask  : constant Word := 16#0100#;
         Enable_Mask  : constant Byte := 16#0F#;
         Enable_Value : constant Byte := 16#0A#;
         RAM_Enable   : Boolean;
      begin
         if (Address and Accept_Mask) = 0 then
            RAM_Enable := (Value and Enable_Mask) = Enable_Value;
            Handler.Cart_Handler.RAM_Handler.Set_Enabled (RAM_Enable);
         end if;
      end Enable_RAM;

      overriding
      procedure Select_Bank
        (Handler : in out Handler_Type;
         Address : Spaces.ROM.MBC.Bank_Select_Address;
         Value   : Byte)
      is
         Accept_Mask : constant Word := 16#0100#;
         Index       : Cart.ROM.Bank_Index;
      begin
         if (Address and Accept_Mask) /= 0 and Address in Bank_Select_Address then
            Index := Cart.ROM.Bank_Index (Value and Select_Mask);
            Handler.Switch_Banks (1, Index);
         end if;
      end Select_Bank;

   end ROM_Space;

   ---------------
   -- RAM_Space --
   ---------------

   package body RAM_Space is
      use Cart.Banked.RAM;

      package Base_Bank renames Cart.Banked.RAM;
      package Blank_Bank renames Cart.Banked.RAM.Blank;
      package Memory_Bank renames Cart.Banked.RAM.MBC2;

      function Create (Path : String) return Handler_Access is
         Handler : constant Handler_Access := new Handler_Type;
      begin
         Initialize (Handler.all, Path);
         return Handler;
      end Create;

      procedure Initialize
        (Handler : out Handler_Type'Class;
         Path    : String)
      is
      begin
         Handler.Memory := new Memory_Bank.Handler_Type;
         Handler.Memory.Initialize (Path);
         Handler.Reset;
      end Initialize;

      overriding
      procedure Reset (Handler : in out Handler_Type) is
      begin
         Handler.Disable;
      end Reset;

      overriding
      procedure Read
        (Handler : in out Handler_Type;
         GB      : in out Gade.GB.GB_Type;
         Address : Word;
         Content : out Byte)
      is
         pragma Unreferenced (GB);
         Bank_Addr : constant Bank_Address := To_Bank_Address (Address);
      begin
         Handler.Current.Read (Bank_Addr, Content);
      end Read;

      overriding
      procedure Write
        (Handler : in out Handler_Type;
         GB      : in out Gade.GB.GB_Type;
         Address : Word;
         Content : Byte)
      is
         pragma Unreferenced (GB);
         Bank_Addr : constant Bank_Address := To_Bank_Address (Address);
      begin
         Handler.Current.Write (Bank_Addr, Content);
      end Write;

      overriding
      procedure Set_Enabled
        (Handler : in out Handler_Type;
         Enabled : Boolean)
      is
         State_Changed : constant Boolean := Handler.Enabled /= Enabled;
      begin
         if State_Changed and Enabled then
            Handler.Enable;
         elsif State_Changed and not Enabled then
            Handler.Disable;
         end if;
      end Set_Enabled;

      procedure Enable (Handler : in out Handler_Type) is
      begin
         Handler.Enabled := True;
         Handler.Current := Base_Bank.Handler_Access (Handler.Memory);
      end Enable;

      procedure Disable (Handler : in out Handler_Type) is
      begin
         Handler.Enabled := False;
         Handler.Current := Base_Bank.Handler_Access (Blank_Bank.Singleton);
      end Disable;

      overriding
      procedure Save (Handler : Handler_Type) is
      begin
         Handler.Memory.Save;
      end Save;

   end RAM_Space;

end Gade.Cart.Controllers.MBC2;
