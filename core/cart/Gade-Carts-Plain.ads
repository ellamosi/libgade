private with Gade.Carts.Banks;
private with Gade.Carts.Banks.ROM;
private with Gade.Carts.Banks.RAM;

package Gade.Carts.Plain is

   type Plain_Cart is new Cart with private;

   type Plain_Cart_Access is access Plain_Cart'Class;

   subtype Plain_Cart_NN_Access is not null Plain_Cart_Access;

   overriding
   procedure Read_ROM
     (C       : in out Plain_Cart;
      Address : External_ROM_IO_Address;
      V       : out Byte);

   overriding
   procedure Read_RAM
     (C       : in out Plain_Cart;
      Address : External_RAM_IO_Address;
      V       : out Byte);

   overriding
   procedure Write_RAM
     (C       : in out Plain_Cart;
      Address : External_RAM_IO_Address;
      V       : Byte);

private

   ROM_Bank_Size    : constant      := 16#8000#;
   RAM_Bank_Size    : constant      := 16#2000#;
   RAM_Address_Mask : constant Word := 16#1FFF#;

   type Path_Access is access String;

   package ROM_Space_Banks is new Gade.Carts.Banks (ROM_Bank_Size);
   package RAM_Space_Banks is new Gade.Carts.Banks (RAM_Bank_Size);

   package ROM_Banks is new ROM_Space_Banks.ROM;
   package RAM_Banks is new RAM_Space_Banks.RAM;
   use ROM_Banks;

   type Plain_Cart is new Cart with record
      ROM      : ROM_Bank;
      RAM      : RAM_Space_Banks.Bank_Access;
      RAM_Path : Path_Access;
   end record;

   function Rebase (Address : External_RAM_IO_Address)
      return RAM_Space_Banks.Bank_Address;

end Gade.Carts.Plain;
