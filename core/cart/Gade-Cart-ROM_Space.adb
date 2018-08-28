package body Gade.Cart.ROM_Space is

   procedure Initialize
     (Handler     : out ROM_Space_Type'Class;
      ROM_Content : ROM_Content_Access)
   is
      ROM_Bank   : ROM_Bank_Access;
      Bank_Count : constant ROM_Bank_Count := ROM_Content.all'Length;
      Bank_Index : ROM_Bank_Range;
   begin
      Handler.ROM_Content := ROM_Content;
      for Addressable_Bank_Index in Addressable_Bank_Range loop
         Bank_Index := Addressable_Bank_Index mod Bank_Count;
         ROM_Bank := ROM_Content (Bank_Index);
         Handler.Addressable_Banks (Addressable_Bank_Index) := ROM_Bank;
      end loop;
   end Initialize;

   overriding
   procedure Read
     (Handler : in out ROM_Space_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : out Byte)
   is
      pragma Unreferenced (GB);
      Bank_Address : constant ROM_Bank_Address := Address and Bank_Address_Mask;
   begin
      case External_ROM_IO_Address (Address) is
         when Bank0_Address =>
            Content := Handler.Addressable_Banks (0)(Bank_Address);
         when Bank1_Address =>
            Content := Handler.Addressable_Banks (1)(Bank_Address);
      end case;
   end Read;

end Gade.Cart.ROM_Space;
