with Gade.Cart.Banks.ROM; use Gade.Cart.Banks.ROM;

package Gade.Cart.ROM is

   Max_ROM_Banks : constant := 512; --  8 MB for MBC5

   type ROM_Bank_Count is range 0 .. Max_ROM_Banks;

   subtype ROM_Bank_Range is ROM_Bank_Count range 0 .. Max_ROM_Banks - 1;

   type ROM_Content_Type is array (ROM_Bank_Range range <>) of ROM_Bank_Access;

   type ROM_Content_Access is access ROM_Content_Type;

   function Load (Path : String) return ROM_Content_Access;

private

   function Trim
     (Content    : ROM_Content_Type;
      Bank_Count : ROM_Bank_Count) return ROM_Content_Access;

end Gade.Cart.ROM;
