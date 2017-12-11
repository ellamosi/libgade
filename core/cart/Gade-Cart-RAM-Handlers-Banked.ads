with Gade.Cart.Banks.RAM.Mem;
with Gade.Cart.RAM; use Gade.Cart.RAM;

package Gade.Cart.RAM.Handlers.Banked is

   subtype MBC1_Bank_Range is RAM_Bank_Range range 0 .. 3;

   type Banked_RAM_Handler_Type is new RAM_Handler_Type with private;

   type Banked_RAM_Handler_Access is access Banked_RAM_Handler_Type;

   function Create
     (Size : RAM_Size_Type;
      Path : String) return Banked_RAM_Handler_Access;

   overriding
   procedure Reset (Handler : in out Banked_RAM_Handler_Type);

   overriding
   procedure Read
     (Handler : in out Banked_RAM_Handler_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : out Byte);

   overriding
   procedure Write
     (Handler : in out Banked_RAM_Handler_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : Byte);

   overriding
   procedure Switch_Banks
     (Handler : in out Banked_RAM_Handler_Type;
      Bank    : RAM_Bank_Range);

   overriding
   procedure Set_Enabled
     (Handler : in out Banked_RAM_Handler_Type;
      Enabled : Boolean);

   procedure Enable
     (Handler : in out Banked_RAM_Handler_Type);

   procedure Disable
     (Handler : in out Banked_RAM_Handler_Type);

   overriding
   procedure Save
     (Handler : Banked_RAM_Handler_Type);

private
   use Gade.Cart.Banks.RAM.Mem;

   type RAM_Path_Access is access constant String;

   type Banked_RAM_Handler_Type is new RAM_Handler_Type with record
      Current_Bank : RAM_Bank_Access;
      RAM_Content  : RAM_Content_Access;
      Memory_Bank  : Memory_RAM_Bank_Access;
      Path         : RAM_Path_Access;
      Enabled      : Boolean;
   end record;

   procedure Initialize
     (Handler : out Banked_RAM_Handler_Type'Class;
      Size    : RAM_Size_Type;
      Path    : String);

end Gade.Cart.RAM.Handlers.Banked;
