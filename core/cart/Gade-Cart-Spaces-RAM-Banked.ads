with Gade.Cart.Banked.RAM.Mem; use Gade.Cart.Banked.RAM.Mem;

package Gade.Cart.Spaces.RAM.Banked is

   subtype MBC1_Bank_Range is RAM_Bank_Range range 0 .. 3;

   type Banked_RAM_Space_Type is new RAM_Space_Type with private;

   type Banked_RAM_Space_Access is access Banked_RAM_Space_Type;

   function Create
     (Size : RAM_Size_Type;
      Path : String) return Banked_RAM_Space_Access;

   overriding
   procedure Reset (Space : in out Banked_RAM_Space_Type);

   overriding
   procedure Read
     (Space   : in out Banked_RAM_Space_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : out Byte);

   overriding
   procedure Write
     (Space   : in out Banked_RAM_Space_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : Byte);

   overriding
   procedure Switch_Banks
     (Space   : in out Banked_RAM_Space_Type;
      Bank    : RAM_Bank_Range);

   overriding
   procedure Set_Enabled
     (Space   : in out Banked_RAM_Space_Type;
      Enabled : Boolean);

   procedure Enable
     (Space : in out Banked_RAM_Space_Type);

   procedure Disable
     (Space : in out Banked_RAM_Space_Type);

   overriding
   procedure Save
     (Space : Banked_RAM_Space_Type);

private

   type Banked_RAM_Space_Type is new RAM_Space_Type with record
      Current_Bank : RAM_Bank_Access;
      Memory_Bank  : Memory_RAM_Bank_Access;
      Enabled      : Boolean;
   end record;

   procedure Initialize
     (Space : out Banked_RAM_Space_Type'Class;
      Size  : RAM_Size_Type;
      Path  : String);

end Gade.Cart.Spaces.RAM.Banked;
