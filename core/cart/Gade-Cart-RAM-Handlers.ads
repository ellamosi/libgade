limited with Gade.GB;
with Gade.Dev; use Gade.Dev;

package Gade.Cart.RAM.Handlers is

   subtype External_RAM_IO_Address is Word range 16#A000# .. 16#BFFF#;

   type RAM_Handler_Type is abstract new Memory_Mapped_Device with private;

   type RAM_Handler_Access is access all RAM_Handler_Type'Class;

   overriding
   procedure Reset (Handler : in out RAM_Handler_Type) is null;

   overriding
   procedure Read
     (Handler : in out RAM_Handler_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : out Byte) is abstract;

   overriding
   procedure Write
     (Handler : in out RAM_Handler_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : Byte) is abstract;

   procedure Switch_Banks
     (Handler : in out RAM_Handler_Type;
      Bank    : RAM_Bank_Range) is null;

   procedure Set_Enabled
     (Handler : in out RAM_Handler_Type;
      Enabled : Boolean) is null;

   procedure Save
     (Handler : RAM_Handler_Type) is null;

private

   RAM_Bank_Count : constant array (RAM_Size_Type) of RAM_Bank_Count_Type :=
     (None        =>  0,
      RAM_16kbit  =>  1,
      RAM_64kbit  =>  1,
      RAM_256kbit =>  4,
      RAM_1Mbit   => 16,
      RAM_512kbit =>  8);

   type RAM_Handler_Type is abstract new Memory_Mapped_Device with null record;

   function To_Bank_Address (Address : Word) return RAM_Bank_Address;

end Gade.Cart.RAM.Handlers;
