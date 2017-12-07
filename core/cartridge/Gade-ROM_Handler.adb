package body Gade.ROM_Handler is

   procedure Create
     (Handler     : out ROM_Handler_Type;
      ROM         : ROM_Access;
      RAM_Handler : RAM_Handler_Access)
   is
      pragma Unreferenced (RAM_Handler);
   begin
      Handler.ROM := ROM;
      for Bank_Index in Addressable_Bank_Range loop
         Handler.Set_ROM_Bank (Bank_Index, Bank_Index);
      end loop;
   end Create;

   overriding
   procedure Read
     (Handler : in out ROM_Handler_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : out Byte)
   is
      pragma Unreferenced (GB);
      Bank_Address : constant ROM_Bank_Address := Address and Bank_Address_Mask;
   begin
      case External_ROM_IO_Address (Address) is
         when Bank0_Address => Content := Handler.Banks (0)(Bank_Address);
         when Bank1_Address => Content := Handler.Banks (1)(Bank_Address);
      end case;
   end Read;

   procedure Set_ROM_Bank
     (Handler          : in out ROM_Handler_Type;
      Addressable_Bank : Addressable_Bank_Range;
      ROM_Bank         : ROM_Bank_Range)
   is
   begin
      Handler.Banks (Addressable_Bank) := Handler.ROM (ROM_Bank);
   end Set_ROM_Bank;

end Gade.ROM_Handler;
