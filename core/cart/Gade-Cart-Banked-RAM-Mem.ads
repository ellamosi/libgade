with Gade.Cart.RAM; use Gade.Cart.RAM;

package Gade.Cart.Banked.RAM.Mem is

   type Handler_Type is new RAM.Handler_Type with private;
   type Handler_Access is access Handler_Type;

   procedure Initialize
     (Handler : out Handler_Type;
      Size    : RAM_Size_Type;
      Path    : String);

   overriding
   procedure Read
     (Handler : in out Handler_Type;
      Address : Bank_Address;
      Value   : out Byte);

   overriding
   procedure Write
     (Handler : in out Handler_Type;
      Address : Bank_Address;
      Value   : Byte);

   procedure Set_Bank
     (Handler : in out Handler_Type;
      Index   : Bank_Index);

   procedure Save
     (Handler : Handler_Type);

private

   RAM_Size : constant array (RAM_Size_Type) of Byte_Count :=
     (None        =>          0,
      RAM_16kbit  =>   2 * 1024,
      RAM_64kbit  =>   8 * 1024,
      RAM_256kbit =>  32 * 1024,
      RAM_1Mbit   => 128 * 1024,
      RAM_512kbit =>  64 * 1024);

   RAM_16kbit_Mask : constant := 16#7FF#;

   type Handler_Type is new RAM.Handler_Type with record
      Size         : RAM_Size_Type;
      RAM          : Content_Access;
      Offset, Mask : Address;
      Path         : Path_Access;
      N_Banks      : Bank_Count;
   end record;

   function RAM_Address
     (Handler : Handler_Type;
      Addr    : Bank_Address)
      return Address;

end Gade.Cart.Banked.RAM.Mem;
