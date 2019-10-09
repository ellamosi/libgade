package body Gade.Cart.C2.Mixins.Banked_RAM.Constructors is

   procedure Initialize
     (C    : in out Banked_RAM_Cart'Class;
      Size : RAM_Size_Type;
      Path : String)
   is
      Bank : RAM_Bank_Access;
   begin
      for I in Bank_Index loop
         Bank := new RAM_Banks.Memory_Bank;
         Offset := Memory_Content_Offset (Bank_Count (I) mod N_Banks) * Bank_Size;
         Initialize (Bank.all, Content, Offset);
         Banks (I) := Bank;
      end loop;

      pragma Compile_Time_Warning (Standard.True, "Initialize unimplemented");
      raise Program_Error with "Unimplemented procedure Initialize";
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

end Gade.Cart.C2.Mixins.Banked_RAM.Constructors;
