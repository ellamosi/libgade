limited with Gade.GB;
with Gade.Dev;          use Gade.Dev;
with Gade.External.ROM; use Gade.External.ROM;
with Gade.RAM_Handler;  use Gade.RAM_Handler;

private package Gade.ROM_Handler is

   subtype External_ROM_IO_Address is Word range 16#0000# .. 16#7FFF#;

   type ROM_Handler_Type is abstract new Memory_Mapped_Device with private;

   type ROM_Handler_Access is access ROM_Handler_Type'Class;

   procedure Create
     (Handler     : out ROM_Handler_Type;
      ROM         : ROM_Access;
      RAM_Handler : RAM_Handler_Access);

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
      Content : Byte) is null;

private

   subtype Addressable_Bank_Range is ROM_Bank_Range range 0 .. 1;

   type ROM_Banks is array (Addressable_Bank_Range) of ROM_Bank_Access;

   subtype Bank0_Address is External_ROM_IO_Address range 16#0000# .. 16#3FFF#;
   subtype Bank1_Address is External_ROM_IO_Address range 16#4000# .. 16#7FFF#;

   type ROM_Handler_Type is abstract new Memory_Mapped_Device with record
      ROM         : ROM_Access;
      Banks       : ROM_Banks;
   end record;

   procedure Set_ROM_Bank
     (Handler          : in out ROM_Handler_Type;
      Addressable_Bank : Addressable_Bank_Range;
      ROM_Bank         : ROM_Bank_Range);

end Gade.ROM_Handler;
