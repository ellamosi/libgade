package body Gade.Cart.Spaces.ROM is

   procedure Initialize
     (Handler : out Handler_Type'Class;
      Content : Content_Access)
   is
      Bank : Memory_ROM_Bank_Access;
      Size : constant Byte_Count_Type := Content.all'Length;
      Bank_Count : constant Bank_Count_Type :=
        Bank_Count_Type (Size / Bank_Size);
      Bank_Index : Bank_Index_Type;
   begin
      for Addressable_Bank_Index in Addressable_Bank_Range loop
         Bank_Index := Addressable_Bank_Index mod Bank_Count;
         Bank := new Memory_ROM_Bank_Type;
         Initialize (Bank.all, Content);
         Bank.Set_Bank (Bank_Index);
         Handler.Addressable_Banks (Addressable_Bank_Index) := Bank;
      end loop;
   end Initialize;

   overriding
   procedure Read
     (Handler : in out Handler_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : out Byte)
   is
      pragma Unreferenced (GB);
      Bank_Address : constant ROM_Bank_Address := Address and Bank_Address_Mask;
   begin
      case External_ROM_IO_Address (Address) is
         when Bank0_Address =>
            Handler.Addressable_Banks (0).Read (Bank_Address, Content);
         when Bank1_Address =>
            Handler.Addressable_Banks (1).Read (Bank_Address, Content);
      end case;
   end Read;

end Gade.Cart.Spaces.ROM;

