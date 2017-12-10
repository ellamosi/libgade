with Gade.Cart.Banks.RAM.Blank;

package body Gade.Cart.RAM.Handlers.Blank is

   Blank_Handler : aliased Blank_RAM_Handler_Type := (null record);
   Blank_Handler_Access : constant Blank_RAM_Handler_Access :=
     Blank_Handler'Access;

   function Singleton return Blank_RAM_Handler_Access is
   begin
      return Blank_Handler_Access;
   end Singleton;

   overriding procedure Read
     (Handler : in out Blank_RAM_Handler_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : out Byte)
   is
      pragma Unreferenced (Handler, GB);
   begin
      Gade.Cart.Banks.RAM.Blank.Singleton.Read (Address, Content);
   end Read;

   overriding procedure Write
     (Handler : in out Blank_RAM_Handler_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : Byte)
   is
      pragma Unreferenced (Handler, GB);
   begin
      Gade.Cart.Banks.RAM.Blank.Singleton.Write (Address, Content);
   end Write;

end Gade.Cart.RAM.Handlers.Blank;
