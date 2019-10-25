package body Gade.Carts.Mixins.Banked.ROM.Constructors is

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
      use ROM_Bank_Constructors, ROM_Bank_Pool_Constructors;

      Size    : constant Content_Byte_Count := Content.all'Length;
      N_Banks : constant Bank_Count := Bank_Count (Size / Content_Byte_Count (Bank_Size));

      Present_Banks : Bank_Array := (others => null);

      Offset : Memory_Content_Offset;
   begin
      for I in 0 .. ROM_Bank_Index (N_Banks - 1) loop
         Offset := Memory_Content_Offset (I) * Content_Byte_Count (Bank_Size);
         Present_Banks (I) := Bank_Access (Create (Content, Offset));
      end loop;
      Initialize (Pool, Present_Banks);
   end Initialize_Banks;

   procedure Initialize_Accessible_Banks
     (Accessible_Banks : out Accessible_Bank_Array;
      Pool             : Bank_Pool)
   is
      I : ROM_Bank_Index;
   begin
      for AI in Accessible_ROM_Bank_Index loop
         I := ROM_Bank_Index (AI);
         Accessible_Banks (AI) := ROM_Bank_Access (Select_Bank (Pool, I));
      end loop;
   end Initialize_Accessible_Banks;

end Gade.Carts.Mixins.Banked.ROM.Constructors;
