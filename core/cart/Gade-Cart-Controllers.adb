pragma Ada_2012;
package body Gade.Cart.Controllers is

   --------------
   -- Read_ROM --
   --------------

   procedure Read_ROM
     (Handler : in out Cart_Handler; Address : Word; Content : out Byte)
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Read_ROM unimplemented");
      raise Program_Error with "Unimplemented procedure Read_ROM";
   end Read_ROM;

   ---------------
   -- Write_ROM --
   ---------------

   procedure Write_ROM
     (Handler : in out Cart_Handler; Address : Word; Content : Byte)
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Write_ROM unimplemented");
      raise Program_Error with "Unimplemented procedure Write_ROM";
   end Write_ROM;

   --------------
   -- Read_RAM --
   --------------

   procedure Read_RAM
     (Handler : in out Cart_Handler; Address : Word; Content : out Byte)
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Read_RAM unimplemented");
      raise Program_Error with "Unimplemented procedure Read_RAM";
   end Read_RAM;

   ---------------
   -- Write_RAM --
   ---------------

   procedure Write_RAM
     (Handler : in out Cart_Handler; Address : Word; Content : Byte)
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Write_RAM unimplemented");
      raise Program_Error with "Unimplemented procedure Write_RAM";
   end Write_RAM;

end Gade.Cart.Controllers;
