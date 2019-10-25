with Gade.Carts.Memory_Contents; use Gade.Carts.Memory_Contents;

package body Gade.Carts.Mixins.Banked.RAM.Constructors is

   procedure Initialize
     (C    : in out Banked_RAM_Cart'Class;
      Size : RAM_Size_Type;
      Path : String)
   is
   begin
      if Size /= None then C.Content := Create (Size); end if;
      C.Path := new String'(Path);
      Initialize_Banks (C.Banks, C.Content, Size);
      --  The following might belong to reset
      C.Accessible_Index := 0;
      C.Enabled := Enabled_Default;
      if Enabled_Default then
         C.Accessible_Bank := Select_Bank (C.Banks, C.Accessible_Index);
      else
         C.Accessible_Bank := Bank_Access (Blank_Banks.Singleton);
      end if;
   end Initialize;

   procedure Initialize_Banks
     (Banks   : out Bank_Pool;
      Content : RAM_Content_Access;
      Size    : RAM_Size_Type)
   is
      use RAM_Bank_Constructors;

      Reported_Banks : constant Bank_Count := Banks_For_RAM_Size (Size);
      Present_Banks  : Bank_Array := (others => null);
      Offset         : Memory_Content_Offset;
      Actual_Banks   : Bank_Count;
   begin
      if Size = None then
         Present_Banks (0) := Bank_Access (Blank_Banks.Singleton);
      else
         --  Only trust the header information to an extent, cap the banks
         --  to the maximum addressable ones by the controller.
         Actual_Banks := Bank_Count'Min (Total_Bank_Count, Reported_Banks);
         for I in 0 .. RAM_Bank_Index (Actual_Banks - 1) loop
            Offset := Memory_Content_Offset (I) * Content_Byte_Count (Bank_Size);
            Present_Banks (I) := Bank_Access (Create (Content, Offset));
         end loop;
      end if;
      Bank_Pool_Constructors.Initialize (Banks, Present_Banks);
   end Initialize_Banks;

end Gade.Carts.Mixins.Banked.RAM.Constructors;
