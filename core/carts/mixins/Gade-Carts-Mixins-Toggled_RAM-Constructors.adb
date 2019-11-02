package body Gade.Carts.Mixins.Toggled_RAM.Constructors is

   procedure Initialize (C : in out Toggled_RAM_Cart'Class) is
   begin
      C.RAM_Enabled := False;
   end Initialize;

end Gade.Carts.Mixins.Toggled_RAM.Constructors;
