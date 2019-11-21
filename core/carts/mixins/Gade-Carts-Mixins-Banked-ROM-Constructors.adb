package body Gade.Carts.Mixins.Banked.ROM.Constructors is

   procedure Initialize
     (C       : in out Banked_ROM_Cart'Class;
      Content : ROM_Content_NN_Access)
   is
   begin
      Initialize_Banks (C.Banks, Content);
      Reset (C);
   end Initialize;

   procedure Initialize_Banks
     (Pool    : out Bank_Pool;
      Content : ROM_Content_NN_Access)
   is
      use ROM_Bank_Constructors, ROM_Bank_Pool_Constructors;

      Size : constant ROM_Content_Size := Content.all'Length;

      N_Banks       : Bank_Count;
      Present_Banks : Bank_Array := (others => null);
      Offset        : ROM_Address;
   begin
      N_Banks := Bank_Count (Size / ROM_Content_Size (Bank_Size));
      N_Banks := Bank_Count'Max (1, N_Banks);
      for I in 0 .. Bank_Index (N_Banks - 1) loop
         Offset := ROM_Address (Natural (I) * Bank_Size);
         Present_Banks (I) := Bank_Access (Create (Content, Offset));
      end loop;
      Initialize (Pool, Present_Banks);
   end Initialize_Banks;

end Gade.Carts.Mixins.Banked.ROM.Constructors;
