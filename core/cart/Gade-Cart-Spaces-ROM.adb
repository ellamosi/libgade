package body Gade.Cart.Spaces.ROM is

   procedure Initialize
     (Handler : out Handler_Type'Class;
      Content : Content_Access)
   is
      Bank : Banked.ROM.Handler_Access;
      Size : constant Byte_Count := Content.all'Length;
      N_Banks : constant Bank_Count := Bank_Count (Size / Bank_Size);
      Index : Bank_Index;
   begin
      for Addressable_Bank_Index in Addressable_Bank_Range loop
         Index := Addressable_Bank_Index mod N_Banks;
         Bank := new Banked.ROM.Handler_Type;
         Initialize (Bank.all, Content);
         Bank.Set_Bank (Index);
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
      Bank_Addr : constant Bank_Address := Address and Address_Mask;
   begin
      case External_ROM_IO_Address (Address) is
         when Bank0_Address =>
            Handler.Addressable_Banks (0).Read (Bank_Addr, Content);
         when Bank1_Address =>
            Handler.Addressable_Banks (1).Read (Bank_Addr, Content);
      end case;
   end Read;

end Gade.Cart.Spaces.ROM;
