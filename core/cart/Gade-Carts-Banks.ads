with Gade.Carts.Memory_Contents; use Gade.Carts.Memory_Contents;

generic
   Size : in Word; --  TODO: Potentially adjust type
package Gade.Carts.Banks is

   subtype Bank_Address is Word range 16#0000# .. Size - 1;

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
      type Content_Type is new Memory_Content;
      type Content_NN_Access is not null access all Content_Type;
   package Memory_Bank_Mixin is

      type Base_Memory_Bank is abstract new Base_Bank with record
         Content : Content_NN_Access;
      end record;

      procedure Initialize_Base
        (B       : out Base_Memory_Bank'Class;
         Content : Content_NN_Access);

      type Memory_Bank is abstract new Base_Memory_Bank with record
         Offset : Memory_Content_Offset;
      end record;

      procedure Initialize_Full
        (B       : out Memory_Bank'Class;
         Content : Content_NN_Access;
         Offset  : Memory_Content_Offset);

      overriding
      procedure Read
        (B       : in out Memory_Bank;
         Address : Bank_Address;
         V       : out Byte);

      type Partial_Memory_Bank is abstract new Base_Memory_Bank with record
         Address_Mask : Bank_Address;
      end record;

      procedure Initialize_Partial
        (B       : out Partial_Memory_Bank'Class;
         Content : Content_NN_Access);

      overriding
      procedure Read
        (B       : in out Partial_Memory_Bank;
         Address : Bank_Address;
         V       : out Byte);

   end Memory_Bank_Mixin;

   function "+"
     (Left  : Bank_Address;
      Right : Memory_Content_Offset) return Memory_Content_Address;
   pragma Inline ("+");

   function Address_Mask (Content_Size : Natural) return Bank_Address;

end Gade.Carts.Banks;