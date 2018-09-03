package body Gade.Cart.Banked.RAM.Mem is

   procedure Initialize
     (Bank : out Memory_RAM_Bank_Type;
      Size : RAM_Size_Type;
      Path : String)
   is
      Byte_Size : constant Byte_Count_Type := RAM_Size (Size);
      subtype Content_Range is Address_Type range 0 .. Byte_Size - 1;
   begin
      Bank.Content := new Content_Type (Content_Range);
      Load (Path, Bank.Content.all);
      Bank.Size := Size;
      Bank.Path := new String'(Path);
      Bank.Bank_Count := Byte_Size / Bank_Size;
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
      Index : Bank_Index_Type)
   is
      Wrapped_Index : constant Bank_Index_Type := Index mod Bank.Bank_Count;
   begin
      case Bank.Size is
         when RAM_16kbit =>
            Bank.Offset := 0;
            Bank.Mask   := 16#7FF#;
         when others =>
            Bank.Offset := Wrapped_Index * Bank_Size;
            Bank.Mask   := Bank_Size - 1;
      end case;
   end Set_Bank;

   function RAM_Address
     (Bank    : Memory_RAM_Bank_Type;
      Address : RAM_Bank_Address)
      return Address_Type
   is
      Offseted_Address : constant Address_Type :=
        Address_Type (Address) + Bank.Offset;
   begin
      return Offseted_Address and Bank.Mask;
   end RAM_Address;

   procedure Save
     (Bank : Memory_RAM_Bank_Type)
   is
   begin
      Save (Bank.Path.all, Bank.Content.all);
   end Save;

end Gade.Cart.Banked.RAM.Mem;

