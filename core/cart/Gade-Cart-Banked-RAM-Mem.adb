package body Gade.Cart.Banked.RAM.Mem is

   procedure Initialize
     (Handler : out Handler_Type;
      Size    : RAM_Size_Type;
      Path    : String)
   is
      Byte_Size : constant Byte_Count := RAM_Size (Size);
      subtype Content_Range is Address range 0 .. Byte_Size - 1;
   begin
      Handler.RAM := new Content (Content_Range);
      Load (Path, Handler.RAM.all);
      Handler.Size := Size;
      Handler.Path := new String'(Path);
      Handler.N_Banks := Byte_Size / Bank_Size;
   end Initialize;

   overriding procedure Read
     (Handler : Handler_Type;
      Address : Bank_Address;
      Value   : out Byte)
   is
   begin
      Value := Handler.RAM (RAM_Address (Handler, Address));
   end Read;

   overriding procedure Write
     (Handler : in out Handler_Type;
      Address : Bank_Address;
      Value   : Byte)
   is
   begin
      Handler.RAM (RAM_Address (Handler, Address)) := Value;
   end Write;

   procedure Set_Bank
     (Handler : in out Handler_Type;
      Index   : Bank_Index)
   is
      Wrapped_Index : constant Bank_Index := Index mod Handler.N_Banks;
   begin
      case Handler.Size is
         when RAM_16kbit =>
            Handler.Offset := 0;
            Handler.Mask   := RAM_16kbit_Mask;
         when others =>
            Handler.Offset := Wrapped_Index * Bank_Size;
            Handler.Mask   := Address (Address_Mask);
      end case;
   end Set_Bank;

   function RAM_Address
     (Handler : Handler_Type;
      Addr    : Bank_Address)
      return Address
   is
      Offseted_Address : constant Address := Address (Addr) + Handler.Offset;
   begin
      return Offseted_Address and Handler.Mask;
   end RAM_Address;

   procedure Save (Handler : Handler_Type) is
   begin
      Save (Handler.Path.all, Handler.RAM.all);
   end Save;

end Gade.Cart.Banked.RAM.Mem;
