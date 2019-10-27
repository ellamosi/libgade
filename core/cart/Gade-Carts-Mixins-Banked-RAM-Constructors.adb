with Gade.Carts.Memory_Contents; use Gade.Carts.Memory_Contents;

package body Gade.Carts.Mixins.Banked.RAM.Constructors is

--     function Create_Default_Bank_Factory
--       (Content : RAM_Content_NN_Access) return Bank_Factory'Class
--     is
--        F : Standard_Bank_Factory := (Content => Content, Banks => (others => null));
--     begin
--        --  Do init
--        return F;
--     end Create_Default_Bank_Factory;
--
--     function Create_Blank_Bank_Factory return Bank_Factory'Class is
--     begin
--        return Address_Space_Banks.Blank.Singleton;
--     end Create_Blank_Bank_Factory;

   overriding
   function Create_Bank
     (F : in out Default_Bank_Factory;
      I : Bank_Index) return Bank_NN_Access
   is
      use RAM_Bank_Constructors;

      Offset : Memory_Content_Offset;
      Original_Index : Bank_Index;
   begin
      if F.Banks (I) = null and Bank_Count (I) >= F.N_Banks then
         Original_Index := Bank_Index (Bank_Count (I) mod F.N_Banks);
         F.Banks (I) := F.Create_Bank (Original_Index);
      elsif F.Banks (I) = null and Bank_Count (I) < F.N_Banks then
         Offset := Memory_Content_Offset (I) * Content_Byte_Count (Bank_Size);
         F.Banks (I) := Bank_Access (Create (F.Content, Offset));
      end if;
      return F.Banks (I);
   end Create_Bank;

   overriding
   function Create_Bank
     (F : in out Blank_Bank_Factory;
      I : Bank_Index) return Bank_NN_Access
   is
      pragma Unreferenced (F, I);
   begin
      return Bank_NN_Access (Blank_Banks.Singleton);
   end Create_Bank;

   procedure Initialize
     (C       : in out Banked_RAM_Cart'Class;
      Content : RAM_Content_Access;
      Path    : String)
   is
      BF : Default_Bank_Factory;
   begin
      if Content /= null then
         BF.N_Banks := Bank_Count (Content'Length / Bank_Size);
         BF.Content := Content;
         BF.Banks := (others => null);
      end if;
      Initialize (C, Content, Path, BF);
   end Initialize;

   procedure Initialize
     (C       : in out Banked_RAM_Cart'Class;
      Content : RAM_Content_Access;
      Path    : String;
      BF      : in out Bank_Factory'Class)
   is
      Blank_BF : Blank_Bank_Factory;
   begin
      C.Content := Content;
      C.Path := new String'(Path);
      if Content /= null then
         Bank_Pool_Constructors.Initialize (C.Banks, BF);
      else
         Bank_Pool_Constructors.Initialize (C.Banks, Blank_BF);
      end if;
      --  The following might belong to reset
      C.Accessible_Index := 0;
      C.Enabled := Enabled_Default;
      if Enabled_Default then
         C.Accessible_Bank := Select_Bank (C.Banks, C.Accessible_Index);
      else
         C.Accessible_Bank := Bank_Access (Blank_Banks.Singleton);
      end if;
   end Initialize;

--     procedure Initialize_Banks
--       (Banks   : out Bank_Pool;
--        Content : RAM_Content_Access;
--        BF      : in out Bank_Factory'Class)
--     is
--        --  Reported_Banks : constant Bank_Count := ;
--        Present_Banks  : Bank_Array := (others => null);
--        N_Banks : Bank_Count := Content'Length / Bank_Size;
--     begin
--        if Content /= null then
--           Present_Banks (0) := Bank_Access (Blank_Banks.Singleton);
--        else
--
--
--           for I in 0 .. Bank_Index (Actual_Banks - 1) loop
--              Present_Banks (I) := Bank_Access (BF.Create_Bank (Content, I));
--           end loop;
--        end if;
--
--     end Initialize_Banks;

end Gade.Carts.Mixins.Banked.RAM.Constructors;
