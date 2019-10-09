with Gade.Cart.Banks.Mem.Constructors;

package body Gade.Cart.C2.Mixins.Banked_ROM.Constructors is

   procedure Initialize
     (C       : in out Banked_ROM_Cart'Class;
      Content : ROM_Content_Access)
   is
   begin
      Initialize_Banks (C.ROM_Banks, Content);
      Initialize_Current_Banks (C.Current_ROM_Banks, C.ROM_Banks);
   end Initialize;

   procedure Initialize_Banks
     (Banks   : out ROM_Bank_Set;
      Content : ROM_Content_Access)
   is
      package ROM_Banks_Constructors is new ROM_Banks.Constructors;
      use ROM_Banks_Constructors;

      Size    : constant Content_Byte_Count := Content.all'Length;
      N_Banks : constant Bank_Count := Bank_Count (Size / Bank_Size);

      Offset : Memory_Content_Offset;
      Bank   : ROM_Bank_Access;
   begin
      for I in Bank_Index loop
         Bank := new ROM_Banks.Memory_Bank;
         Offset := Memory_Content_Offset (Bank_Count (I) mod N_Banks) * Bank_Size;
         Initialize (Bank.all, Content, Offset);
         Banks (I) := Bank;
      end loop;
   end Initialize_Banks;

   procedure Initialize_Current_Banks
     (Current_Banks : out Current_ROM_Bank_Set;
      Banks         : ROM_Bank_Set)
   is
      I : Bank_Index;
   begin
      for Location in ROM_Bank_Location loop
         I := Bank_Index (ROM_Bank_Location'Pos (Location));
         Current_Banks (Location) := Banks (I);
      end loop;
   end Initialize_Current_Banks;

end Gade.Cart.C2.Mixins.Banked_ROM.Constructors;
