with Ada.Text_IO;

package body Gade.External.ROM is

   procedure Load
     (ROM  : out ROM_Type;
      Path : String)
   is
      File       : Bank_Reader.File_Type;
      Bank_Index : ROM_Bank_Range;
   begin
      ROM := (others => Null_ROM_Bank'Access);
      Open (File, In_File, Path);
      Bank_Index := 0;
      while not End_Of_File (File) loop
         Ada.Text_IO.Put_Line ("Loading bank " & Bank_Index'Img);
         Load_Bank (File, ROM (Bank_Index));
         Bank_Index := Bank_Index + 1;
      end loop;
      Ada.Text_IO.Put_Line ("Loader: Successfully loaded" & Bank_Index'Img & " ROM banks.");
      Close (File);
   end Load;

   procedure Load_Bank
     (File : File_Type;
      Bank : out ROM_Bank_Access)
   is
      Target_Bank : constant access ROM_Bank_Type := new ROM_Bank_Type;
   begin
      Read (File, Target_Bank.all);
      Bank := ROM_Bank_Access (Target_Bank);
   end Load_Bank;

end Gade.External.ROM;
