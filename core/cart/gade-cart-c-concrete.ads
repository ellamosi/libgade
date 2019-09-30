private with Gade.Cart.SH.Banked;

package Gade.Cart.C.Concrete is

   type Concrete_Cart_Handler is new C.Cart_Handler with private;

   type Concrete_Cart_Handler_Access is not null access Concrete_Cart_Handler;

   function Create return Concrete_Cart_Handler_Access;

private

   --  Does not need to be a package necessarily
   package Concrete_Space_Handler is

      subtype Bank_Index_Type is Natural range 0 .. 255;

      package Banked_Space_Handler is new Gade.Cart.SH.Banked
        (Bank_Index_Type => Bank_Index_Type);

      type Space_Handler is new Banked_Space_Handler.Space_Handler with record
         Cart_Handler : Concrete_Cart_Handler_Access;
      end record;

      procedure Initialize
        (Handler      : out Space_Handler'Class;
         Cart_Handler : Concrete_Cart_Handler_Access);

      procedure Some_Operation (Handler : in out Space_Handler'Class);

      overriding
      procedure Anonymous_Operation (Handler : in out Space_Handler);

   end Concrete_Space_Handler;

   type Space_Handler_Access is access Concrete_Space_Handler.Space_Handler;
   subtype Space_Handler_NN_Access is not null Space_Handler_Access;

   type Concrete_Cart_Handler is new C.Cart_Handler with record
      SH1, SH2 : Space_Handler_NN_Access;
   end record;

   procedure Initialize (Handler : out Concrete_Cart_Handler'Class);

end Gade.Cart.C.Concrete;
