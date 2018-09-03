package body Gade.Cart.Banked.ROM is

   procedure Initialize
     (Handler : out Handler_Type;
      Content : Content_Access)
   is
   begin
      Handler.ROM := Content;
   end Initialize;

   procedure Read
     (Handler : Handler_Type;
      Addr    : Bank_Address;
      Value   : out Byte)
   is
   begin
      Value := Handler.ROM (Address (Addr) + Handler.Offset);
   end Read;

   procedure Set_Bank
     (Handler : in out Handler_Type;
      Index   : Bank_Index)
   is
      Unwrapped_Offset : constant Address := Address (Index) * Bank_Size;
   begin
      Handler.Offset := Unwrapped_Offset mod Handler.ROM'Length;
   end Set_Bank;

end Gade.Cart.Banked.ROM;
