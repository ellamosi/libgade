with Gade.Cart.Banked.RAM.Mem; use Gade.Cart.Banked.RAM.Mem;

package Gade.Cart.Spaces.RAM.Banked is

   type Handler_Type is new RAM.Handler_Type with record
      Memory_Bank  : Cart.Banked.RAM.Mem.Handler_Access;
      Enabled      : Boolean;
   end record;

   type Handler_Access is access Handler_Type;

   function Create
     (Size : RAM_Size_Type;
      Path : String) return Handler_Access;

   overriding
   procedure Reset (Handler : in out Handler_Type);

   overriding
   procedure Switch_Banks
     (Handler : in out Handler_Type;
      Index   : Bank_Index);

   overriding
   procedure Set_Enabled
     (Handler : in out Handler_Type;
      Enabled : Boolean);

   overriding
   procedure Save (Handler : Handler_Type);

   procedure Initialize
     (Handler : out Handler_Type'Class;
      Size    : RAM_Size_Type;
      Path    : String);

   procedure Enable (Handler : in out Handler_Type);

   procedure Disable (Handler : in out Handler_Type);

end Gade.Cart.Spaces.RAM.Banked;
