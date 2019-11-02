with Gade.Carts.Memory_Contents; use Gade.Carts.Memory_Contents;
with Gade.Carts.Banks;

generic
   Banks            : in Bank_Count;
   Accessible_Banks : in Bank_Count;
   type Address_Space is new Word;
package Gade.Carts.Banked_Memory_Spaces is

   subtype Bank_Index_Type is Bank_Count range 0 .. Bank_Count'Last - 1;
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

end Gade.Carts.Banked_Memory_Spaces;
