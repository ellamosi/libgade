with Ada.Text_IO;
package body Gade.Carts.Mixins.Banked is

   function Bit_Size (N : Positive) return Natural is
      Remaining_Bits : Positive;
      Bit_Count      : Natural;
   begin
      Remaining_Bits := N;
      Bit_Count := 0;
      while Remaining_Bits > 1 loop
         Remaining_Bits := Remaining_Bits / 2;
         Bit_Count := Bit_Count + 1;
      end loop;
      return Bit_Count;
   end Bit_Size;

   package body Banked_Space_Carts is

      overriding
      procedure Finalize (C : in out Banked_Space_Cart) is
      begin
         Ada.Text_IO.Put_Line ("Mixins.Banked.Finalize");
         Bank_Pools.Finalize (C.Banks);
      end Finalize;

   end Banked_Space_Carts;

end Gade.Carts.Mixins.Banked;
