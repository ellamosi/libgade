package body Gade.Cart.RAM is

   procedure Load
     (Path    : String;
      Content : out RAM_Content_Type)
   is
      use RAM_Bank_IO;
      File       : File_Type;
      Bank_Index : RAM_Bank_Count_Type;
   begin
      Content := (others => null);
      Open (File, In_File, Path);
      Bank_Index := 0;
      while not End_Of_File (File) loop
         Content (Bank_Index) := Load (File);
         Bank_Index := Bank_Index + 1;
      end loop;
      Close (File);
   exception
      when Name_Error => Initialize (Content);
   end Load;

   procedure Initialize (Content : out RAM_Content_Type) is
   begin
      for Bank of Content loop
         Bank := new RAM_Bank_Content_Type;
         Initialize (Bank.all);
      end loop;
   end Initialize;

end Gade.Cart.RAM;
