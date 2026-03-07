generic
package Gade.Carts.Banks.RAM.MBC2 is

   type MBC2_RAM_Bank is new Bank with private;

   type MBC2_RAM_Bank_Access is access all MBC2_RAM_Bank;

   subtype MBC2_RAM_Bank_NN_Access is not null MBC2_RAM_Bank_Access;

   overriding
   procedure Read
     (B       : in out MBC2_RAM_Bank;
      Address : Bank_Address;
      V       : out Byte);

   overriding
   procedure Write
     (B       : in out MBC2_RAM_Bank;
      Address : Bank_Address;
      V       : Byte);

private

   Value_Mask : constant Byte := 16#F0#;

   type MBC2_RAM_Bank is new RAM_Bank with null record;

end Gade.Carts.Banks.RAM.MBC2;
