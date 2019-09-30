package body Gade.Cart.SH.Banked is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Handler : out Space_Handler'Class) is
   begin
      null;
   end Initialize;

   --------------
   -- Set_Bank --
   --------------

   procedure Set_Bank
     (Handler : in out Space_Handler'Class;
      Bank : Bank_Index_Type)
   is
   begin
      Handler.Bank := Bank;
   end Set_Bank;

end Gade.Cart.SH.Banked;
