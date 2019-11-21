generic
   Size : in Positive;
package Gade.Carts.Banks is

   subtype Bank_Address is Word range 0 .. Word (Size - 1);

   type Bank is abstract tagged private;

   type Bank_Access is access all Bank'Class;
   subtype Bank_NN_Access is not null Bank_Access;

   procedure Read
     (B       : in out Bank;
      Address : Bank_Address;
      V       : out Byte) is abstract;

   procedure Write (B : in out Bank; Address : Bank_Address; V : Byte) is null;

private

   type Bank is abstract tagged null record;

   generic
      type Base_Bank is abstract new Bank with private;
      type Address is range <>;
      type Content is array (Address range <>) of Byte;
      type Content_Access is access all Content;
      type Content_NN_Access is not null access all Content;
   package Memory_Bank_Mixin is

      type Memory_Bank is abstract new Base_Bank with record
         Content      : Content_Access;
         Offset       : Address;
         Address_Mask : Bank_Address;
      end record;

      procedure Initialize
        (B       : out Memory_Bank'Class;
         Content : Content_NN_Access;
         Offset  : Address);

      overriding
      procedure Read
        (B    : in out Memory_Bank;
         Addr : Bank_Address;
         V    : out Byte);

      function Decode
        (B    : Memory_Bank'Class;
         Addr : Bank_Address)
         return Address;

   private

      function Address_Mask (Mem : Content) return Bank_Address;

   end Memory_Bank_Mixin;

end Gade.Carts.Banks;
