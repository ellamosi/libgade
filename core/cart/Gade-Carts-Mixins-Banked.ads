with Gade.Carts.Banks;
with Gade.Carts.Banks.Pools;
with Gade.Carts.Memory_Contents; use Gade.Carts.Memory_Contents;

package Gade.Carts.Mixins.Banked is

   subtype Bank_Index_Type is Bank_Count range 0 .. Bank_Count'Last - 1;

private

   subtype Unbanked_Bank_Index is Bank_Count range 0 .. 0;

   function Bit_Size (N : Positive) return Natural;

   generic
      type Base_Cart is abstract new Cart with private;
      type Accessible_Bank_Index is range <>;
      type Bank_Index is range <>;
      type Address_Space is new Word;
   package Banked_Spaces is

      Address_Space_Size    : constant Natural := Address_Space'Range_Length;
      Total_Bank_Count      : constant Bank_Count := Bank_Index'Range_Length;
      Accessible_Bank_Count : constant Bank_Count := Accessible_Bank_Index'Range_Length;

      Address_Space_Bit_Size : constant Positive := Bit_Size (Address_Space_Size);
      Accessible_Bank_Index_Bit_Size : constant Natural := Bit_Size (Positive (Accessible_Bank_Count));

      Bank_Size : constant Word := Word (Address_Space_Size) / Word (Accessible_Bank_Count);

      Space_Address_Mask     : constant Word := Word (Address_Space_Size - 1);
      Bank_Address_Mask      : constant Word := Bank_Size - 1;
      Accessible_Index_Shift : constant Natural := Address_Space_Bit_Size - Accessible_Bank_Index_Bit_Size;

      package Address_Space_Banks is new Gade.Carts.Banks (Bank_Size);
      use Address_Space_Banks;

      package Bank_Pools is new Address_Space_Banks.Pools (Bank_Index);
      use Bank_Pools;

      --  Accessible_Banks / Bank_Access should be defined in a more concrete level
      --  of the implementation, to be able to prevent dispatching when accessing
      --  ROM banks, which are more performance sensitive and do not require it.
      type Banked_Space_Cart is abstract new Base_Cart with record
         Banks : Bank_Pool;
      end record;
   end Banked_Spaces;

end Gade.Carts.Mixins.Banked;
