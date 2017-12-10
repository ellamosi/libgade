with Gade.Cart.Banks.RAM; use Gade.Cart.Banks.RAM;

package Gade.Cart.RAM is

   Max_RAM_Banks : constant := 16; --  128 kB for MBC5

   type RAM_Bank_Count_Type is range 0 .. Max_RAM_Banks;

   subtype RAM_Bank_Range is RAM_Bank_Count_Type range 0 .. Max_RAM_Banks - 1;

   type RAM_Content_Type is array (RAM_Bank_Range range <>)
     of RAM_Bank_Content_Access;

   type RAM_Content_Access is access RAM_Content_Type;

end Gade.Cart.RAM;
