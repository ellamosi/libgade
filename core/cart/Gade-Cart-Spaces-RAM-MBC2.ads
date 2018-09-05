with Gade.Cart.Banked.RAM.MBC2; use Gade.Cart.Banked.RAM.MBC2;

package Gade.Cart.Spaces.RAM.MBC2 is

   type Handler_Type is new RAM.Handler_Type with private;
   type Handler_Access is access Handler_Type;

   function Create (Path : String) return Handler_Access;

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
   procedure Set_Enabled
     (Handler : in out Handler_Type;
      Enabled : Boolean);

   overriding
   procedure Save (Handler : Handler_Type);

private

   type Handler_Type is new RAM.Handler_Type with record
      Current : Cart.Banked.RAM.Handler_Access;
      Memory  : Cart.Banked.RAM.MBC2.Handler_Access;
      Enabled : Boolean;
   end record;

   procedure Initialize
     (Handler : out Handler_Type'Class;
      Path    : String);

   procedure Enable (Handler : in out Handler_Type);

   procedure Disable (Handler : in out Handler_Type);

end Gade.Cart.Spaces.RAM.MBC2;
