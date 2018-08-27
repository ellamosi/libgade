package body Gade.Cart.Banks.RAM.Mem is

   procedure Initialize
     (Bank : out Memory_RAM_Bank_Type;
      Size : RAM_Size_Type;
      Path : String)
   is
      RAM_Bytes : constant RAM_Byte_Count_Type := RAM_Size (Size);
      subtype Content_Range is RAM_Address_Range range 0 .. RAM_Bytes - 1;
   begin
      Bank.Content := new RAM_Content_Type (Content_Range);
      Load (Path, Bank.Content.all);
      Bank.Size := Size;
      Bank.Path := new String'(Path);
   end Initialize;

   overriding procedure Read
     (Bank    : Memory_RAM_Bank_Type;
      Address : RAM_Bank_Address;
      Value   : out Byte)
   is
   begin
      Value := Bank.Content (RAM_Address (Bank, Address));
   end Read;

   overriding procedure Write
     (Bank    : in out Memory_RAM_Bank_Type;
      Address : RAM_Bank_Address;
      Value   : Byte)
   is
   begin
      Bank.Content (RAM_Address (Bank, Address)) := Value;
   end Write;

   procedure Set_Bank
     (Bank  : in out Memory_RAM_Bank_Type;
      Index : RAM_Bank_Range)
   is
   begin
      case Bank.Size is
         when RAM_16kbit =>
            Bank.Offset := 0;
            Bank.Mask   := 16#7FF#;
         when others =>
            Bank.Offset := Index * 16#2000#;
            Bank.Mask   := 16#1FFF#;
      end case;
   end Set_Bank;

   function RAM_Address
     (Bank    : Memory_RAM_Bank_Type;
      Address : RAM_Bank_Address)
      return RAM_Address_Range
   is
      Offseted_Address : constant RAM_Address_Range
        := RAM_Address_Range (Address) + Bank.Offset;
   begin
      return Offseted_Address and Bank.Mask;
   end RAM_Address;

   procedure Save
     (Bank : Memory_RAM_Bank_Type)
   is
   begin
      Save (Bank.Path.all, Bank.Content.all);
   end Save;

end Gade.Cart.Banks.RAM.Mem;
