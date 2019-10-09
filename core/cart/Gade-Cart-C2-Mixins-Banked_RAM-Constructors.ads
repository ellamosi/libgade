generic
package Gade.Cart.C2.Mixins.Banked_RAM.Constructors is

   procedure Initialize
     (C    : in out Banked_RAM_Cart'Class;
      Size : RAM_Size_Type;
      Path : String); --  This needs config and cart info objects

private

   Banks_For_RAM_Size : constant array (RAM_Size_Type)
     of Bank_Count :=
       (None        =>  0,
        RAM_16kbit  =>  1,
        RAM_64kbit  =>  1,
        RAM_256kbit =>  4,
        RAM_1Mbit   => 16,
        RAM_512kbit =>  8);

end Gade.Cart.C2.Mixins.Banked_RAM.Constructors;
