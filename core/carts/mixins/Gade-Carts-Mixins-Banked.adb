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

end Gade.Carts.Mixins.Banked;
