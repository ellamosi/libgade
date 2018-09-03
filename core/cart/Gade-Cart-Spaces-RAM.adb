package body Gade.Cart.Spaces.RAM is

   function To_Bank_Address (Address : Word) return Bank_Address is
   begin
      return Address and Address_Mask;
   end To_Bank_Address;

end Gade.Cart.Spaces.RAM;
