with Gade.Cart.RAM; use Gade.Cart.RAM;

package Gade.Cart.Banked.RAM.Mem is

   type Memory_RAM_Bank_Type is new RAM_Bank_Type with private;

   type Memory_RAM_Bank_Access is access Memory_RAM_Bank_Type;

   procedure Initialize
     (Bank : out Memory_RAM_Bank_Type;
      Size : RAM_Size_Type;
      Path : String);

   overriding
   procedure Read
     (Bank    : Memory_RAM_Bank_Type;
      Address : RAM_Bank_Address;
      Value   : out Byte);

   overriding
   procedure Write
     (Bank    : in out Memory_RAM_Bank_Type;
      Address : RAM_Bank_Address;
      Value   : Byte);

   procedure Set_Bank
     (Bank  : in out Memory_RAM_Bank_Type;
      Index : Bank_Index_Type);

   procedure Save
     (Bank : Memory_RAM_Bank_Type);

private

   RAM_Size : constant array (RAM_Size_Type) of Byte_Count_Type :=
     (None        =>          0,
      RAM_16kbit  =>   2 * 1024,
      RAM_64kbit  =>   8 * 1024,
      RAM_256kbit =>  32 * 1024,
      RAM_1Mbit   => 128 * 1024,
      RAM_512kbit =>  64 * 1024);

   type Path_Access is access constant String;

   type Memory_RAM_Bank_Type is new RAM_Bank_Type with record
      Size         : RAM_Size_Type;
      Content      : Content_Access;
      Offset, Mask : Address_Type;
      Path         : Path_Access;
      Bank_Count   : Bank_Count_Type;
   end record;

   function RAM_Address
     (Bank    : Memory_RAM_Bank_Type;
      Address : RAM_Bank_Address)
      return Address_Type;

end Gade.Cart.Banked.RAM.Mem;

