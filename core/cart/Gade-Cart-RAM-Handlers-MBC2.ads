with Gade.Cart.RAM;            use Gade.Cart.RAM;
with Gade.Cart.Banks.MBC2_RAM; use Gade.Cart.Banks.MBC2_RAM;

package Gade.Cart.RAM.Handlers.MBC2 is

   type MBC2_RAM_Handler_Type is new RAM_Handler_Type with private;

   type MBC2_RAM_Handler_Access is access MBC2_RAM_Handler_Type;

   function Create (Path : String) return MBC2_RAM_Handler_Access;

   overriding
   procedure Reset (Handler : in out MBC2_RAM_Handler_Type);

   overriding
   procedure Read
     (Handler : in out MBC2_RAM_Handler_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : out Byte);

   overriding
   procedure Write
     (Handler : in out MBC2_RAM_Handler_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : Byte);

   overriding
   procedure Set_Enabled
     (Handler : in out MBC2_RAM_Handler_Type;
      Enabled : Boolean);

   overriding
   procedure Toggle_Enabled
     (Handler : in out MBC2_RAM_Handler_Type);

   overriding
   procedure Save
     (Handler : MBC2_RAM_Handler_Type);

private

   type RAM_Path_Access is access constant String;

   type MBC2_RAM_Handler_Type is new RAM_Handler_Type with record
      RAM_Content  : MBC2_RAM_Content_Access;
      Path         : RAM_Path_Access;
      Enabled      : Boolean;
   end record;

   procedure Initialize
     (Handler : out MBC2_RAM_Handler_Type'Class;
      Path    : String);

end Gade.Cart.RAM.Handlers.MBC2;
