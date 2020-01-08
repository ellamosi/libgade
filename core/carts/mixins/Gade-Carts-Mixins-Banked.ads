with Gade.Carts.Banks;
with Gade.Carts.Banks.Pools;

package Gade.Carts.Mixins.Banked is

   generic
      Banks            : in Positive;
      Accessible_Banks : in Positive;
      type Address_Space is new Word;
      type Content_Size is range <>;
   package Banked_Spaces is

      type Bank_Count is new Natural range 0 .. Banks;

      --  The only reason why the index types are not defined indepentently is
      --  because of generic restrictions for non static declarations
      type Bank_Index is new Natural range 0 .. Banks - 1;
      type Accessible_Bank_Index is new Natural range 0 .. Accessible_Banks - 1;

      Address_Space_Size : constant Positive := Address_Space'Range_Length;
      Bank_Size          : constant Positive := Address_Space_Size / Accessible_Banks;
      Max_Content_Size   : constant Content_Size := Content_Size (Banks * Bank_Size);

      package Address_Space_Banks is new Gade.Carts.Banks (Bank_Size);
      package Bank_Pools is new Address_Space_Banks.Pools (Bank_Index);
      use Address_Space_Banks, Bank_Pools;

   end Banked_Spaces;

private

   function Bit_Size (N : Positive) return Natural;

   generic
      type Base_Cart is abstract new Cart with private;
      with package BS is new Banked_Spaces (<>);
   package Banked_Space_Carts is
      use BS;

      Address_Space_Bit_Size : constant Positive := Bit_Size (Address_Space_Size);
      Accessible_Bank_Index_Bit_Size : constant Natural := Bit_Size (Accessible_Banks);

      Space_Address_Mask     : constant Word := Word (Address_Space_Size - 1);
      Bank_Address_Mask      : constant Word := Word (Bank_Size - 1);
      Accessible_Index_Shift : constant Natural := Address_Space_Bit_Size - Accessible_Bank_Index_Bit_Size;

      --  Accessible_Banks / Bank_Access should be defined in a more concrete level
      --  of the implementation, to be able to prevent dispatching when accessing
      --  ROM banks, which are more performance sensitive and do not require it.
      type Banked_Space_Cart is abstract new Base_Cart with record
         Banks : Bank_Pools.Bank_Pool;
      end record;

      overriding
      procedure Finalize (C : in out Banked_Space_Cart);

   private

   end Banked_Space_Carts;

end Gade.Carts.Mixins.Banked;
