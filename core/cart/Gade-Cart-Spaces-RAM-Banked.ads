with Gade.Cart.Banked.RAM.Mem; use Gade.Cart.Banked.RAM.Mem;

package Gade.Cart.Spaces.RAM.Banked is

   type Handler_Type is new RAM.Handler_Type with private;
   type Handler_Access is access Handler_Type;

   function Create
     (Size : RAM_Size_Type;
      Path : String) return Handler_Access;

   overriding
   procedure Reset (Handler : in out Handler_Type);

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

   overriding
   procedure Switch_Banks
     (Handler : in out Handler_Type;
      Index   : Bank_Index);

   overriding
   procedure Set_Enabled
     (Handler : in out Handler_Type;
      Enabled : Boolean);

   procedure Enable
     (Handler : in out Handler_Type);

   procedure Disable
     (Handler : in out Handler_Type);

   overriding
   procedure Save
     (Handler : Handler_Type);

private

   type Handler_Type is new RAM.Handler_Type with record
      Current_Bank : RAM_Bank_Access;
      Memory_Bank  : Memory_RAM_Bank_Access;
      Enabled      : Boolean;
   end record;

   procedure Initialize
     (Handler : out Handler_Type'Class;
      Size    : RAM_Size_Type;
      Path    : String);

end Gade.Cart.Spaces.RAM.Banked;

