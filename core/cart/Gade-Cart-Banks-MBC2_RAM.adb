package body Gade.Cart.Banks.MBC2_RAM is

   procedure Initialize (Content : out MBC2_RAM_Content_Type) is
   begin
      Content := (others => 0);
   end Initialize;

   function Load
     (File : MBC2_RAM_IO.File_Type) return MBC2_RAM_Content_Access
   is
      Content : constant MBC2_RAM_Content_Access := new MBC2_RAM_Content_Type;
   begin
      MBC2_RAM_IO.Read (File, Content.all);
      return Content;
   end Load;

   procedure Save
     (File    : MBC2_RAM_IO.File_Type;
      Content : MBC2_RAM_Content_Type)
   is
   begin
      MBC2_RAM_IO.Write (File, Content);
   end Save;

end Gade.Cart.Banks.MBC2_RAM;
