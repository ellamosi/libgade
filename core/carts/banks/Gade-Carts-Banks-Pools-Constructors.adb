package body Gade.Carts.Banks.Pools.Constructors is

   procedure Initialize
     (Pool : out Bank_Pool;
      BF   : in out Bank_Factory'Class)
   is
   begin
      for I in Bank_Index'Range loop
         Pool.Banks (I) := BF.Create_Bank (I);
      end loop;
   end Initialize;

   package body Default_Bank_Factories is

      procedure Initialize
        (Bank_Factory : out Default_Bank_Factory'Class;
         Content      : Content_Access)
      is
      begin
         Bank_Factory.Content := Content;
         Bank_Factory.Banks := (others => null);
         if Content = null then
            --  Set the first bank to blank and let it be mirrored to the rest
            Bank_Factory.N_Banks := 1;
            Bank_Factory.Banks (0) := Bank_Access (Blank_Banks.Singleton);
         else
            --  Count the actual (non mirrored) memory banks
            Bank_Factory.N_Banks := Bank_Count (Content'Length / Size);
         end if;
      end Initialize;

      overriding
      function Create_Bank
        (F : in out Default_Bank_Factory;
         I : Bank_Index) return Bank_NN_Access
      is
         Offset         : Address;
         Original_Index : Bank_Index;
      begin
         if F.Banks (I) = null and Bank_Count (I) >= F.N_Banks then
            --  Mirror existing Bank (recursion triggers creation of the actual
            --  one as necessary)
            Original_Index := Bank_Index (Bank_Count (I) mod F.N_Banks);
            F.Banks (I) := F.Create_Bank (Original_Index);
         elsif F.Banks (I) = null and Bank_Count (I) < F.N_Banks then
            --  Create actual RAM Bank
            Offset := Address (Natural (I) * Size);
            --  F.Banks (I) := Bank_Access (Create (F.Content, Offset));
            F.Banks (I) := Create_Offset_Bank (F.Content, Offset);
         end if;
         return F.Banks (I);
      end Create_Bank;

   end Default_Bank_Factories;

end Gade.Carts.Banks.Pools.Constructors;
