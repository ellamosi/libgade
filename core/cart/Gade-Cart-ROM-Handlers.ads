limited with Gade.GB;
with Gade.Dev; use Gade.Dev;

package Gade.Cart.ROM.Handlers is

   subtype External_ROM_IO_Address is Word range 16#0000# .. 16#7FFF#;

   type ROM_Handler_Type is abstract new Memory_Mapped_Device with private;

   type ROM_Handler_Access is access all ROM_Handler_Type'Class;

   overriding
   procedure Reset
     (Handler : in out ROM_Handler_Type) is null;

   overriding
   procedure Read
     (Handler : in out ROM_Handler_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : out Byte);

   overriding
   procedure Write
     (Handler : in out ROM_Handler_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : Byte) is abstract;

private

   Bank_Count_For_Size : constant array (ROM_Size_Type) of ROM_Bank_Count :=
     (ROM_256kbit =>   2,
      ROM_512kbit =>   4,
      ROM_1Mbit   =>   8,
      ROM_2Mbit   =>  16,
      ROM_4Mbit   =>  32,
      ROM_8Mbit   =>  64,
      ROM_16Mbit  => 128,
      ROM_32Mbit  => 256,
      ROM_64Mbit  => 512,
      ROM_9Mbit   =>  72, -- These (72, 80, 96) don't exist??
      ROM_10Mbit  =>  80,
      ROM_12Mbit  =>  96);

   subtype Bank0_Address is External_ROM_IO_Address range 16#0000# .. 16#3FFF#;
   subtype Bank1_Address is External_ROM_IO_Address range 16#4000# .. 16#7FFF#;

   Addressable_Bank_Count : constant := 2;

   subtype Addressable_Bank_Range is ROM_Bank_Range range
     0 .. Addressable_Bank_Count - 1;

   type Addressable_ROM_Banks is array (Addressable_Bank_Range) of
     ROM_Bank_Access;

   type ROM_Handler_Type is abstract new Memory_Mapped_Device with record
      ROM_Content       : ROM_Content_Access;
      Addressable_Banks : Addressable_ROM_Banks;
   end record;

   procedure Initialize
     (Handler     : out ROM_Handler_Type'Class;
      ROM_Content : ROM_Content_Access);

end Gade.Cart.ROM.Handlers;
