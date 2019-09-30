pragma Ada_2012;
package body Gade.Cart.C.Concrete is

   ----------------------------
   -- Concrete_Space_Handler --
   ----------------------------

   package body Concrete_Space_Handler is

      ----------------
      -- Initialize --
      ----------------

      procedure Initialize
        (Handler      : out Space_Handler'Class;
         Cart_Handler :     Concrete_Cart_Handler_Access)
      is
      begin
         Banked_Space_Handler.Initialize (Handler);
         Handler.Cart_Handler := Cart_Handler;
      end Initialize;

      procedure Some_Operation (Handler : in out Space_Handler'Class) is
      begin
         if Handler.Bank mod 2 = 0 then
            Handler.Cart_Handler.SH1.Set_Bank (1);
         else
            Handler.Cart_Handler.SH2.Set_Bank (0);
         end if;
      end Some_Operation;

      overriding
      procedure Anonymous_Operation (Handler : in out Space_Handler) is
      begin
         Some_Operation (Handler);
      end Anonymous_Operation;

   end Concrete_Space_Handler;

   ------------
   -- Create --
   ------------

   function Create return Concrete_Cart_Handler_Access is
      Result : constant Concrete_Cart_Handler_Access := new Concrete_Cart_Handler;
   begin
      Result.Initialize;
      return Result;
   end Create;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Handler : out Concrete_Cart_Handler'Class) is
   begin
      Handler.SH1 := new Concrete_Space_Handler.Space_Handler;
      Handler.SH2 := new Concrete_Space_Handler.Space_Handler;
   end Initialize;

end Gade.Cart.C.Concrete;
