package body Gade.Cart.Spaces.RAM is

   function To_Bank_Address (Address : Word) return RAM_Bank_Address is
   begin
      return Address and Bank_Address_Mask;
   end To_Bank_Address;

end Gade.Cart.Spaces.RAM;

