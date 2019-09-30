limited with Gade.GB;
with Gade.Dev;             use Gade.Dev;
with Gade.Cart.Banked.RAM; use Gade.Cart.Banked.RAM;
with Gade.Cart.RAM;        use Gade.Cart.RAM;

package Gade.Cart.Spaces.RAM is

   subtype External_RAM_IO_Address is Word range 16#A000# .. 16#BFFF#;

   type Handler_Type is abstract new Memory_Mapped_Device with private;
   type Handler_Access is access all Handler_Type'Class;

   procedure Initialize
     (Handler : out Handler_Type'Class;
      Size    : RAM_Size_Type;
      Path    : String);

   overriding
   procedure Reset (Space : in out Handler_Type) is null;

   overriding
   procedure Read
     (Handler : in out Handler_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : out Byte);

   overriding
   procedure Write
     (Handler : in out Handler_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : Byte);

   procedure Switch_Banks
     (Handler : in out Handler_Type;
      Index   : Bank_Index) is null;

   procedure Set_Enabled
     (Handler : in out Handler_Type;
      Enabled : Boolean) is null;

   procedure Save (Space : Handler_Type) is null;

   function To_Bank_Address (Address : Word) return Bank_Address;

private

   type Handler_Type is abstract new Memory_Mapped_Device with record
     Current_Bank : Gade.Cart.Banked.RAM.Handler_Access;
   end record;

end Gade.Cart.Spaces.RAM;
