with Gade.Cart.Banks.RAM.Blank;

package body Gade.Cart.RAM_Space.Banked is

   function Create
     (Size : RAM_Size_Type;
      Path : String) return Banked_RAM_Space_Access
   is
      Space : constant Banked_RAM_Space_Access := new Banked_RAM_Space_Type;
   begin
      Banked.Initialize (Space.all, Size, Path);
      return Space;
   end Create;

   procedure Initialize
     (Space : out Banked_RAM_Space_Type'Class;
      Size  : RAM_Size_Type;
      Path  : String)
   is
   begin
      Space.Memory_Bank := new Memory_RAM_Bank_Type;
      Space.Memory_Bank.Initialize (Size, Path);
      Space.Reset;
   end Initialize;

   overriding
   procedure Reset (Space : in out Banked_RAM_Space_Type) is
   begin
      Space.Switch_Banks (0);
      Space.Disable;
   end Reset;

   overriding
   procedure Read
     (Space   : in out Banked_RAM_Space_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : out Byte)
   is
      pragma Unreferenced (GB);
      Bank_Address : constant RAM_Bank_Address := To_Bank_Address (Address);
   begin
      Space.Current_Bank.Read (Bank_Address, Content);
   end Read;

   overriding
   procedure Write
     (Space   : in out Banked_RAM_Space_Type;
      GB      : in out Gade.GB.GB_Type;
      Address : Word;
      Content : Byte)
   is
      pragma Unreferenced (GB);
      Bank_Address : constant RAM_Bank_Address := To_Bank_Address (Address);
   begin
      Space.Current_Bank.Write (Bank_Address, Content);
   end Write;

   overriding
   procedure Switch_Banks
     (Space : in out Banked_RAM_Space_Type;
      Bank  : RAM_Bank_Range)
   is
   begin
      Space.Memory_Bank.Set_Bank (Bank);
   end Switch_Banks;

   overriding
   procedure Set_Enabled
     (Space   : in out Banked_RAM_Space_Type;
      Enabled : Boolean)
   is
      State_Changed : constant Boolean := Space.Enabled /= Enabled;
   begin
      if State_Changed and Enabled then
         Space.Enable;
      elsif State_Changed and not Enabled then
         Space.Disable;
      end if;
   end Set_Enabled;

   procedure Enable (Space : in out Banked_RAM_Space_Type) is
   begin
      Space.Enabled := True;
      Space.Current_Bank := RAM_Bank_Access (Space.Memory_Bank);
   end Enable;

   procedure Disable (Space : in out Banked_RAM_Space_Type) is
   begin
      Space.Enabled := False;
      Space.Current_Bank :=
        RAM_Bank_Access (Gade.Cart.Banks.RAM.Blank.Singleton);
   end Disable;

   overriding
   procedure Save (Space : Banked_RAM_Space_Type) is
   begin
      Space.Memory_Bank.Save;
   end Save;

end Gade.Cart.RAM_Space.Banked;
