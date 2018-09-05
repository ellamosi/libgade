with Gade.Cart.Banked.RAM.Blank;

package body Gade.Cart.Spaces.RAM.MBC2 is

   package Base_Bank renames Cart.Banked.RAM;
   package Blank_Bank renames Cart.Banked.RAM.Blank;
   package Memory_Bank renames Cart.Banked.RAM.MBC2;

   function Create (Path : String) return Handler_Access is
      Handler : constant Handler_Access := new Handler_Type;
   begin
      MBC2.Initialize (Handler.all, Path);
      return Handler;
   end Create;

   procedure Initialize
     (Handler : out Handler_Type'Class;
      Path    : String)
   is
   begin
      Handler.Memory := new Memory_Bank.Handler_Type;
      Handler.Memory.Initialize (Path);
      Handler.Reset;
   end Initialize;

   overriding
   procedure Reset (Handler : in out Handler_Type) is
   begin
      Handler.Disable;
   end Reset;

   overriding
   procedure Read
     (Handler : in out Handler_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : out Byte)
   is
      pragma Unreferenced (GB);
      Bank_Addr : constant Bank_Address := To_Bank_Address (Address);
   begin
      Handler.Current.Read (Bank_Addr, Content);
   end Read;

   overriding
   procedure Write
     (Handler : in out Handler_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : Byte)
   is
      pragma Unreferenced (GB);
      Bank_Addr : constant Bank_Address := To_Bank_Address (Address);
   begin
      Handler.Current.Write (Bank_Addr, Content);
   end Write;

   overriding
   procedure Set_Enabled
     (Handler : in out Handler_Type;
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

   procedure Enable (Handler : in out Handler_Type) is
   begin
      Handler.Enabled := True;
      Handler.Current := Base_Bank.Handler_Access (Handler.Memory);
   end Enable;

   procedure Disable (Handler : in out Handler_Type) is
   begin
      Handler.Enabled := False;
      Handler.Current := Base_Bank.Handler_Access (Blank_Bank.Singleton);
   end Disable;

   overriding
   procedure Save (Handler : Handler_Type) is
   begin
      Handler.Memory.Save;
   end Save;

end Gade.Cart.Spaces.RAM.MBC2;
