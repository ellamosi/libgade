with Gade.Cart.Banks.RAM.Blank;

package body Gade.Cart.RAM.Handlers.Banked is

   function Create (Size : RAM_Size_Type) return Banked_RAM_Handler_Access
   is
      Handler : constant Banked_RAM_Handler_Access :=
        new Banked_RAM_Handler_Type;
   begin
      Banked.Initialize (Handler.all, Size);
      return Handler;
   end Create;

   procedure Initialize
     (Handler : out Banked_RAM_Handler_Type'Class;
      Size    : RAM_Size_Type)
   is
      Bank_Count : constant RAM_Bank_Count_Type := RAM_Bank_Count (Size);
      subtype Content_Range is RAM_Bank_Range range  0 .. Bank_Count - 1;
   begin
      Handler.RAM_Content := new RAM_Content_Type (Content_Range);
      for Bank of Handler.RAM_Content.all loop
         Bank := new RAM_Bank_Content_Type'(others => 0);
      end loop;
      Handler.Memory_Bank := new Memory_RAM_Bank_Type;
      Handler.Switch_Banks (0);
      Handler.Disable;
   end Initialize;

   overriding
   procedure Read
     (Handler : in out Banked_RAM_Handler_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : out Byte)
   is
      pragma Unreferenced (GB);
      Bank_Address : constant RAM_Bank_Address := To_Bank_Address (Address);
   begin
      Handler.Current_Bank.Read (Bank_Address, Content);
   end Read;

   overriding
   procedure Write
     (Handler : in out Banked_RAM_Handler_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : Byte)
   is
      pragma Unreferenced (GB);
      Bank_Address : constant RAM_Bank_Address := To_Bank_Address (Address);
   begin
      Handler.Current_Bank.Write (Bank_Address, Content);
   end Write;

   overriding
   procedure Switch_Banks
     (Handler : in out Banked_RAM_Handler_Type;
      Bank    : RAM_Bank_Range)
   is
      Bank_Index : constant RAM_Bank_Range :=
        Bank mod Handler.RAM_Content.all'Length;
   begin
      Handler.Memory_Bank.Content := Handler.RAM_Content (Bank_Index);
   end Switch_Banks;

   overriding
   procedure Set_Enabled
     (Handler : in out Banked_RAM_Handler_Type;
      Enabled : Boolean)
   is
      State_Changed : constant Boolean := Handler.Enabled /= Enabled;
   begin
      if State_Changed and Enabled then
         Handler.Enable;
      elsif State_Changed and not Enabled then
         Handler.Disable;
      end if;
   end Set_Enabled;

   procedure Enable (Handler : in out Banked_RAM_Handler_Type) is
   begin
      Handler.Enabled := True;
      Handler.Current_Bank := RAM_Bank_Access (Handler.Memory_Bank);
   end Enable;

   procedure Disable (Handler : in out Banked_RAM_Handler_Type) is
   begin
      Handler.Enabled := False;
      Handler.Current_Bank :=
        RAM_Bank_Access (Gade.Cart.Banks.RAM.Blank.Singleton);
   end Disable;

   procedure Save (Handler : in out Banked_RAM_Handler_Type) is
      use RAM_Bank_IO;
      File : RAM_Bank_IO.File_Type;
   begin
      Create (File, Out_File, "gade.sav");
      for Bank of Handler.RAM_Content.all loop
         Save (File, Bank.all);
      end loop;
      Close (File);
   end Save;

end Gade.Cart.RAM.Handlers.Banked;
