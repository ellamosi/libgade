with Gade.Cart.RTC; use Gade.Cart.RTC;

package body Gade.Cart.Spaces.RAM.Banked.MBC3 is

   function Create
     (Size : RAM_Size_Type;
      Path : String) return Handler_Access
   is
      Handler : constant Handler_Access := new Handler_Type;
   begin
      MBC3.Initialize (Handler.all, Size, Path);
      return Handler;
   end Create;

   procedure Initialize
     (Handler : out Handler_Type'Class;
      Size    : RAM_Size_Type;
      Path    : String)
   is
   begin
      Banked.Initialize (Handler, Size, Path);
      Handler.RTC_Bank := new Cart.Banked.RAM.RTC.Handler_Type;
      Cart.Banked.RAM.RTC.Initialize (Handler.RTC_Bank.all, Path);
   end Initialize;

   overriding
   procedure Switch_Banks
     (Handler : in out Handler_Type;
      Index   : Bank_Index)
   is
   begin
      case Index is
         when 16#00# .. 16#07# =>
            Handler.Memory_Bank.Set_Bank (Index);
            Handler.Current_Bank :=
              Cart.Banked.RAM.Handler_Access (Handler.Memory_Bank);
         when 16#08# .. 16#0C# =>
            Handler.RTC_Bank.Set_Register (Register'Val (Index - 16#08#));
            Handler.Current_Bank :=
              Cart.Banked.RAM.Handler_Access (Handler.RTC_Bank);
         when others => null;
      end case;
   end Switch_Banks;

   overriding
   procedure Save (Handler : Handler_Type) is
   begin
      Handler.Memory_Bank.Save;
   end Save;

end Gade.Cart.Spaces.RAM.Banked.MBC3;
