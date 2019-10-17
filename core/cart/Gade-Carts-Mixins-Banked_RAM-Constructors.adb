with Gade.Carts.Banks.Mem.Constructors;

package body Gade.Carts.Mixins.Banked_RAM.Constructors is

   procedure Initialize
     (C    : in out Banked_RAM_Cart'Class;
      Size : RAM_Size_Type;
      Path : String)
   is
      N_Banks : constant Bank_Count := Banks_For_RAM_Size (Size);
      Content_Size : constant Content_Byte_Count :=
        Content_Size_For_RAM_Size (Size);

      subtype Content_Range is Memory_Content_Address range 0 .. Content_Size - 1;
   begin
      C.RAM_Content := new RAM_Content (Content_Range);
      C.RAM_Address_Mask := Address_Mask_For_RAM_Size (Size);
      C.Path := new String'(Path);
      if Size = None then
         null;
      else
         Initialize_Banks (C.RAM_Banks, C.RAM_Content, N_Banks);
         Initialize_Open_Banks (C.RAM_Banks, N_Banks);
      end if;

      C.Current_RAM_Bank := C.RAM_Banks (0);
   end Initialize;

   procedure Initialize_Banks
     (Banks   : out RAM_Bank_Set;
      Content : RAM_Content_Access;
      N_Banks : Bank_Count)
   is
      package RAM_Banks_Constructors is new RAM_Banks.Constructors;
      use RAM_Banks_Constructors;

      subtype Bank_Range is Bank_Index range 0 .. Bank_Index (N_Banks - 1);

      Offset : Memory_Content_Offset;
      Bank   : RAM_Bank_Access;
   begin
      Offset := 0;
      for I in Bank_Range loop
         Bank := new RAM_Banks.Memory_Bank;
         Initialize (Bank.all, Content, Offset);
         Offset := Offset + Bank_Size;
         Banks (I) := Bank;
      end loop;
   end Initialize_Banks;

   procedure Initialize_Open_Banks
     (Banks   : in out RAM_Bank_Set;
      N_Banks : Bank_Count)
   is
   begin
      for I in Bank_Index (N_Banks) + 1 .. Bank_Index'Last loop
         --  Re-use existing banks
         Banks (I) := Banks (I mod Bank_Index (N_Banks));
      end loop;
   end Initialize_Open_Banks;

end Gade.Carts.Mixins.Banked_RAM.Constructors;
