private with Gade.Carts.Mixins.MBC;
with Gade.Camera;

package Gade.Carts.Camera is

   type Camera_Cart is new Cart with private;

   type Camera_Cart_Access is access Camera_Cart;

   subtype Camera_Cart_NN_Access is not null Camera_Cart_Access;

   overriding
   procedure Read_RAM
     (C : in out Camera_Cart; Address : External_RAM_IO_Address; V : out Byte);

   overriding
   procedure Report_Cycles (C : in out Camera_Cart; Cycles : M_Cycle_Count);

   overriding
   procedure Reset (C : in out Camera_Cart);

   overriding
   procedure Set_Camera_Provider
     (C : in out Camera_Cart; Provider : Gade.Camera.Provider_Access);

   overriding
   procedure Write_RAM
     (C : in out Camera_Cart; Address : External_RAM_IO_Address; V : Byte);

private

   Last_Register         : constant := 16#35#;
   RAM_Index_Mask        : constant Byte := 16#0F#;
   RAM_Select_Bit        : constant Byte := 16#10#;
   Register_0_Read_Mask  : constant Byte := 16#06#;
   Register_0_Write_Mask : constant Byte := 16#07#;
   Register_Mirror_Size  : constant := 16#80#;
   ROM_Bank_Count        : constant := 64;
   ROM_Index_Mask        : constant Byte := 16#3F#;
   RAM_Bank_Count        : constant := 16;

   subtype Camera_Register_Index is Natural range 0 .. Last_Register;

   type Camera_Registers is array (Camera_Register_Index) of Byte;

   package MBC_Mixin is new
     Gade.Carts.Mixins.MBC
       (Base_Cart => Cart,
        ROM_Banks => ROM_Bank_Count,
        RAM_Banks => RAM_Bank_Count);
   use MBC_Mixin;

   subtype RAM_Bank_Index is ROM_RAM_Mixin.Banked_RAM_Mixin.Banked_RAM_Spaces.Bank_Index;

   type Camera_Cart is new MBC_Cart with record
      Capture_Active      : Boolean;
      Capture_Cycles_Left : M_Cycle_Count;
      Provider            : Gade.Camera.Provider_Access := Gade.Camera.Default_Provider;
      RAM_Write_Enabled   : Boolean;
      Registers           : Camera_Registers;
      Registers_Selected  : Boolean;
      Selected_RAM_Bank   : RAM_Bank_Index;
   end record;

   subtype ROM_Bank_Select_Address is Bank_Select_Address range 16#2000# .. 16#3FFF#;

   subtype RAM_Bank_Select_Address is Bank_Select_Address range 16#4000# .. 16#5FFF#;

   overriding
   procedure Enable_RAM (C : in out Camera_Cart; Enable : Boolean);

   overriding
   procedure Select_Bank
     (C : in out Camera_Cart; Address : Bank_Select_Address; Value : Byte);

   procedure Read_Register
     (C : in out Camera_Cart; Address : External_RAM_IO_Address; V : out Byte);

   procedure Select_RAM_Bank (C : in out Camera_Cart; Value : Byte);

   procedure Select_ROM_Bank (C : in out Camera_Cart; Value : Byte);

   procedure Write_Register
     (C : in out Camera_Cart; Address : External_RAM_IO_Address; V : Byte);

end Gade.Carts.Camera;
