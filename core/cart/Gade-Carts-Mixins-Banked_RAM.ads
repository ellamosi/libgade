with Gade.Carts.Memory_Contents; use Gade.Carts.Memory_Contents;
private with Gade.Carts.Banks;
private with Gade.Carts.Banks.Mem;

generic
   type Base_Cart is abstract new Cart with private;
   type Bank_Index is range <>;
package Gade.Carts.Mixins.Banked_RAM is

   type Banked_RAM_Cart is abstract new Base_Cart with private;

   overriding
   procedure Read_RAM
     (C       : in out Banked_RAM_Cart;
      Address : External_RAM_IO_Address;
      V       : out Byte);

   overriding
   procedure Write_RAM
     (C       : in out Banked_RAM_Cart;
      Address : External_RAM_IO_Address;
      V       : Byte);

   procedure Select_RAM_Bank
     (C : in out Banked_RAM_Cart;
      I : Bank_Index);

private

   Bank_Address_Mask : constant Word := 16#3FFF#;

   subtype Bank_Address is Word range 16#0000# .. 16#1FFF#;

   package Banks is new Gade.Carts.Banks (16#2000#, Bank_Index);
   package RAM_Banks is new Banks.Mem (RAM_Content, RAM_Content_Access);

   type RAM_Bank_Access is access RAM_Banks.Memory_Bank;
   type RAM_Bank_Set is array (Bank_Index) of RAM_Bank_Access;

   type Path_Access is access constant String;

   type Banked_RAM_Cart is abstract new Base_Cart with record
      Current_RAM_Bank : RAM_Bank_Access;
      RAM_Banks        : RAM_Bank_Set;
      RAM_Content      : RAM_Content_Access;
      --  Should potentially by a bank concern: ?
      RAM_Address_Mask : Word;
      Path             : Path_Access;
   end record;

   function Rebase (Address : External_RAM_IO_Address;
                    Mask    : Word) return Bank_Address;
   pragma Inline (Rebase);

end Gade.Carts.Mixins.Banked_RAM;
