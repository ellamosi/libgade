with Ada.Text_IO; use Ada.Text_IO;

package body Gade.Carts.Mixins.Toggled_RAM is

   overriding
   procedure Read_RAM
     (C       : in out Toggled_RAM_Cart;
      Address : External_RAM_IO_Address;
      V       : out Byte)
   is
   begin
      if C.RAM_Enabled then
         Base_Cart (C).Read_RAM (Address, V);
      else
         V := Blank_Value;
      end if;
   end Read_RAM;

   overriding
   procedure Write_RAM
     (C       : in out Toggled_RAM_Cart;
      Address : External_RAM_IO_Address;
      V       : Byte)
   is
   begin
      if C.RAM_Enabled then Base_Cart (C).Write_RAM (Address, V); end if;
   end Write_RAM;

   procedure Enable_RAM (C : in out Toggled_RAM_Cart; Enable : Boolean) is
   begin
      Put_Line ("Enable_RAM: " & Enable'Img);
      C.RAM_Enabled := Enable;
   end Enable_RAM;

end Gade.Carts.Mixins.Toggled_RAM;
