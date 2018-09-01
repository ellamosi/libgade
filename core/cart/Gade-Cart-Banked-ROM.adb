package body Gade.Cart.Banked.ROM is

   procedure Initialize
     (Bank    : out Memory_ROM_Bank_Type;
      Content : ROM_Content_Access)
   is
   begin
      Bank.Content := Content;
   end Initialize;

   procedure Read
     (Bank    : Memory_ROM_Bank_Type;
      Address : ROM_Bank_Address;
      Value   : out Byte)
   is
   begin
      Value := Bank.Content (ROM_Address_Range (Address) + Bank.Offset);
   end Read;

   procedure Set_Bank
     (Bank  : in out Memory_ROM_Bank_Type;
      Index : ROM_Bank_Range)
   is
      Unwrapped_Offset : constant ROM_Address_Range :=
        ROM_Address_Range (Index) * 16 * 1024;
   begin
      Bank.Offset := Unwrapped_Offset mod Bank.Content'Length;
   end Set_Bank;

end Gade.Cart.Banked.ROM;
