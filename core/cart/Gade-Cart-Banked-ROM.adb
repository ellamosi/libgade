package body Gade.Cart.Banked.ROM is

   procedure Initialize
     (Bank    : out Memory_ROM_Bank_Type;
      Content : Content_Access)
   is
   begin
      Bank.Content := Content;
   end Initialize;

   procedure Read
     (Bank  : Memory_ROM_Bank_Type;
      Addr  : ROM_Bank_Address;
      Value : out Byte)
   is
   begin
      Value := Bank.Content (Address (Addr) + Bank.Offset);
   end Read;

   procedure Set_Bank
     (Bank  : in out Memory_ROM_Bank_Type;
      Index : Bank_Index)
   is
      Unwrapped_Offset : constant Address :=
        Address (Index) * Bank_Size;
   begin
      Bank.Offset := Unwrapped_Offset mod Bank.Content'Length;
   end Set_Bank;

end Gade.Cart.Banked.ROM;
