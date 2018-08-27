limited with Gade.GB;
with Gade.Dev; use Gade.Dev;
with Gade.Cart.Banks.RAM; use Gade.Cart.Banks.RAM;

with Gade.Cart.RAM; use Gade.Cart.RAM;

package Gade.Cart.RAM_Space is

   subtype External_RAM_IO_Address is Word range 16#A000# .. 16#BFFF#;

   type RAM_Space_Type is abstract new Memory_Mapped_Device with private;

   type RAM_Space_Access is access all RAM_Space_Type'Class;

   overriding
   procedure Reset (Space : in out RAM_Space_Type) is null;

   overriding
   procedure Read
     (Space   : in out RAM_Space_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : out Byte) is abstract;

   overriding
   procedure Write
     (Space   : in out RAM_Space_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : Byte) is abstract;

   procedure Switch_Banks
     (Space : in out RAM_Space_Type;
      Bank  : RAM_Bank_Range) is null;

   procedure Set_Enabled
     (Space   : in out RAM_Space_Type;
      Enabled : Boolean) is null;

   procedure Save
     (Space : RAM_Space_Type) is null;

private

   type RAM_Space_Type is abstract new Memory_Mapped_Device with null record;

   function To_Bank_Address (Address : Word) return RAM_Bank_Address;

end Gade.Cart.RAM_Space;
