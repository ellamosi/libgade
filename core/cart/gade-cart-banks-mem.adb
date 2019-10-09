package body Gade.Cart.Banks.Mem is

   overriding
   procedure Read
     (B : in out Memory_Bank; Address : Bank_Address; V : out Byte)
   is
   begin
      V := B.Content (Address + B.Offset);
   end Read;

   overriding
   procedure Write (B : in out Memory_Bank; Address : Bank_Address; V : Byte) is
   begin
      B.Content (Address + B.Offset) := V;
   end Write;

   overriding
   procedure Set_Bank (B : in out Memory_Bank; I : Bank_Index) is
      Unwrapped_Offset : constant Memory_Content_Address :=
        Memory_Content_Address (Size) * Memory_Content_Address (I);
   begin
      B.Offset := Unwrapped_Offset mod B.Content'Length;
   end Set_Bank;

   function "+" (Left : Bank_Address; Right : Memory_Content_Offset)
      return Memory_Content_Address
   is
   begin
      return Memory_Content_Address (Left) + Right;
   end "+";

end Gade.Cart.Banks.Mem;
