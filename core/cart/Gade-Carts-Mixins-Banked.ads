with Gade.Carts.Banks;
with Gade.Carts.Banks.Pools;
with Gade.Carts.Memory_Contents; use Gade.Carts.Memory_Contents;

package Gade.Carts.Mixins.Banked is

   subtype Bank_Index_Type is Bank_Count range 0 .. Bank_Count'Last - 1;

   generic
      Banks            : in Bank_Count;
      Accessible_Banks : in Bank_Count;
      type Address_Space is new Word;
   package Banked_Spaces is

      type Bank_Index is new Bank_Index_Type range 0 .. Banks - 1;
      type Accessible_Bank_Index is
        new Bank_Index_Type range 0 .. Accessible_Banks - 1;

      Address_Space_Size    : constant Natural := Address_Space'Range_Length;
      Total_Bank_Count      : constant Bank_Count := Bank_Index'Range_Length;
      Accessible_Bank_Count : constant Bank_Count := Accessible_Bank_Index'Range_Length;
      Bank_Size             : constant Word := Word (Address_Space_Size) / Word (Accessible_Bank_Count);
      Max_Content_Size      : constant Content_Byte_Count :=
        Content_Byte_Count (Total_Bank_Count) * Content_Byte_Count (Bank_Size);

      package Address_Space_Banks is new Gade.Carts.Banks (Bank_Size);
      package Bank_Pools is new Address_Space_Banks.Pools (Bank_Index);
      use Address_Space_Banks, Bank_Pools;

   end Banked_Spaces;

private

   subtype Unbanked_Bank_Index is Bank_Count range 0 .. 0;

   function Bit_Size (N : Positive) return Natural;

   generic
      type Base_Cart is abstract new Cart with private;
      with package BS is new Banked_Spaces (<>);
--        type Accessible_Bank_Index is range <>;
--        type Bank_Index is range <>;
--        type Address_Space is new Word;
   package Banked_Space_Carts is
      use BS;

--        Address_Space_Size    : constant Natural := Address_Space'Range_Length;
--        Total_Bank_Count      : constant Bank_Count := Bank_Index'Range_Length;
--        Accessible_Bank_Count : constant Bank_Count := Accessible_Bank_Index'Range_Length;
--        Bank_Size             : constant Word := Word (Address_Space_Size) / Word (Accessible_Bank_Count);

      Address_Space_Bit_Size : constant Positive := Bit_Size (Address_Space_Size);
      Accessible_Bank_Index_Bit_Size : constant Natural := Bit_Size (Positive (Accessible_Bank_Count));

      Space_Address_Mask     : constant Word := Word (Address_Space_Size - 1);
      Bank_Address_Mask      : constant Word := Bank_Size - 1;
      Accessible_Index_Shift : constant Natural := Address_Space_Bit_Size - Accessible_Bank_Index_Bit_Size;

      --  Accessible_Banks / Bank_Access should be defined in a more concrete level
      --  of the implementation, to be able to prevent dispatching when accessing
      --  ROM banks, which are more performance sensitive and do not require it.
      type Banked_Space_Cart is abstract new Base_Cart with record
         Banks : Bank_Pools.Bank_Pool;
      end record;

   end Banked_Space_Carts;

end Gade.Carts.Mixins.Banked;
