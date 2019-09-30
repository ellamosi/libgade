generic
   type Bank_Index_Type is (<>);
package Gade.Cart.SH.Banked is

   type Space_Handler is abstract new SH.Space_Handler with record
      Bank : Bank_Index_Type;
   end record;

   procedure Initialize (Handler : out Space_Handler'Class);

   procedure Set_Bank
     (Handler : in out Space_Handler'Class;
      Bank    : Bank_Index_Type);

   procedure Anonymous_Operation (Handler : in out Space_Handler) is null;

end Gade.Cart.SH.Banked;
