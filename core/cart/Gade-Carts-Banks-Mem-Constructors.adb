package body Gade.Carts.Banks.Mem.Constructors is

   procedure Initialize
     (B       : out Memory_Bank'Class;
      Content : Content_Access;
      Offset  : Memory_Content_Offset)
   is
   begin
      B.Content := Content;
      B.Offset := Offset;
   end Initialize;

end Gade.Carts.Banks.Mem.Constructors;
