with Gade.Carts.Memory_Contents; use Gade.Carts.Memory_Contents;

generic
   type Content_Type is new Memory_Content;
   type Content_Access is access Content_Type;
package Gade.Carts.Banks.Mem is

   type Memory_Bank is new Bank with private;

   overriding
   procedure Read (B : in out Memory_Bank; Address : Bank_Address; V : out Byte);

   overriding
   procedure Write (B : in out Memory_Bank; Address : Bank_Address; V : Byte);

   overriding
   procedure Set_Bank (B : in out Memory_Bank; I : Bank_Index);

private

   type Memory_Bank is new Bank with record
      Content : Content_Access;
      Offset  : Memory_Content_Offset;
   end record;

   function "+"
     (Left  : Bank_Address;
      Right : Memory_Content_Offset) return Memory_Content_Address;
   pragma Inline ("+");

end Gade.Carts.Banks.Mem;
