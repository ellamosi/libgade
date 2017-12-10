with Ada.Text_IO;

package body Gade.Cart.ROM is

   function Load (Path : String) return ROM_Content_Access is
      use ROM_Bank_IO;
      File       : File_Type;
      Bank_Index : ROM_Bank_Count;
      Content    : ROM_Content_Type (ROM_Bank_Range);
   begin
      Content := (others => null);
      Open (File, In_File, Path);
      Bank_Index := 0;
      while not End_Of_File (File) loop
         Ada.Text_IO.Put_Line ("Loading bank " & Bank_Index'Img);
         Load (File, Content (Bank_Index));
         Bank_Index := Bank_Index + 1;
      end loop;
      Ada.Text_IO.Put_Line ("Loader: Successfully loaded" & Bank_Index'Img & " ROM banks.");
      Close (File);
      return Trim (Content, Bank_Index);
   end Load;

   function Trim
     (Content    : ROM_Content_Type;
      Bank_Count : ROM_Bank_Count) return ROM_Content_Access
   is
      subtype Trimmed_Range is ROM_Bank_Range range  0 .. Bank_Count - 1;
      Trimmed_Content : constant ROM_Content_Access :=
        new ROM_Content_Type (Trimmed_Range);
   begin
      Trimmed_Content.all := Content (Trimmed_Range);
      return Trimmed_Content;
   end Trim;

end Gade.Cart.ROM;
