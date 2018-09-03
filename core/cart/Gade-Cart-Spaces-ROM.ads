limited with Gade.GB;
with Gade.Dev;             use Gade.Dev;
with Gade.Cart.ROM;        use Gade.Cart.ROM;
with Gade.Cart.Banked.ROM; use Gade.Cart.Banked.ROM;

package Gade.Cart.Spaces.ROM is

   subtype External_ROM_IO_Address is Word range 16#0000# .. 16#7FFF#;

   type Handler_Type is abstract new Memory_Mapped_Device with private;
   type Handler_Access is access all Handler_Type'Class;

   overriding
   procedure Reset
     (Handler : in out Handler_Type) is null;

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
      Content : Byte) is abstract;

private

   subtype Bank0_Address is External_ROM_IO_Address range 16#0000# .. 16#3FFF#;
   subtype Bank1_Address is External_ROM_IO_Address range 16#4000# .. 16#7FFF#;

   Addressable_Bank_Count : constant := 2;

   subtype Addressable_Bank_Range is Bank_Index
     range 0 .. Addressable_Bank_Count - 1;

   type Addressable_ROM_Banks is array (Addressable_Bank_Range) of
     Memory_ROM_Bank_Access;

   type Handler_Type is abstract new Memory_Mapped_Device with record
      Addressable_Banks : Addressable_ROM_Banks;
   end record;

   procedure Initialize
     (Handler : out Handler_Type'Class;
      Content : Content_Access);

end Gade.Cart.Spaces.ROM;
