with Gade.Carts.Banks.ROM.Constructors;
with Gade.Carts.Bank_Pools.Constructors;

package body Gade.Carts.Mixins.Banked_ROM.Constructors is

   procedure Initialize
     (C       : in out Banked_ROM_Cart'Class;
      Content : ROM_Content_NN_Access)
   is
   begin
      Initialize_Banks (C.Banks, Content);
      Initialize_Accessible_Banks (C.Accessible_Banks, C.Banks);
   end Initialize;

   procedure Initialize_Banks
     (Pool    : out Bank_Pool;
      Content : ROM_Content_NN_Access)
   is
      package ROM_Bank_Constructors is new ROM_Banks.Constructors;
      package ROM_Bank_Pool_Constructors is new ROM_Bank_Pools.Constructors;
      use ROM_Bank_Constructors, ROM_Bank_Pool_Constructors;

      Size    : constant Content_Byte_Count := Content.all'Length;
      N_Banks : constant Bank_Count := Bank_Count (Size / Bank_Size);

      Present_Banks : Bank_Array := (others => null);

      Offset : Memory_Content_Offset;
   begin
      for I in 0 .. Bank_Index (N_Banks - 1) loop
         Offset := Memory_Content_Offset (I) * Bank_Size;
         Present_Banks (I) := Bank_Access (Create (Content, Offset));
      end loop;
      Initialize (Pool, Present_Banks);
   end Initialize_Banks;

   procedure Initialize_Accessible_Banks
     (Accessible_Banks : out Accessible_Bank_Array;
      Pool             : Bank_Pool)
   is
      I : Bank_Index;
   begin
      for Location in ROM_Bank_Location loop
         I := Bank_Index (ROM_Bank_Location'Pos (Location));
         Accessible_Banks (Location) := ROM_Bank_Access (Select_Bank (Pool, I));
      end loop;
   end Initialize_Accessible_Banks;

end Gade.Carts.Mixins.Banked_ROM.Constructors;
