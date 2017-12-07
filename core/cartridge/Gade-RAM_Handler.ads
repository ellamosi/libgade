limited with Gade.GB;
with Gade.Dev;          use Gade.Dev;
with Gade.External.RAM; use Gade.External.RAM;

private package Gade.RAM_Handler is

   subtype External_RAM_IO_Address is Word range 16#A000# .. 16#BFFF#;

   type RAM_Handler_Type is abstract new Memory_Mapped_Device with private;

   type RAM_Handler_Access is access RAM_Handler_Type'Class;

   procedure Create
     (Handler : out RAM_Handler_Type) is null;

   overriding
   procedure Reset (Handler : in out RAM_Handler_Type) is null;

   overriding
   procedure Read
     (Handler : in out RAM_Handler_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : out Byte);

   overriding
   procedure Write
     (Handler : in out RAM_Handler_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : Byte) is null;

   procedure Switch_Banks
     (Handler : in out RAM_Handler_Type;
      Bank    : RAM_Bank_Range) is null;

   procedure Set_Enabled
     (Handler : in out RAM_Handler_Type;
      Enabled : Boolean) is null;

private

   type RAM_Handler_Type is new Memory_Mapped_Device with null record;

end Gade.RAM_Handler;
