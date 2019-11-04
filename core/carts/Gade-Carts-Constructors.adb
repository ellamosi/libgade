package body Gade.Carts.Constructors is

   procedure Initialize
     (C          : out Cart;
      Save_Path  : String;
      Persistent : Boolean)
   is
   begin
      C.Save_Path := null;
      C.Persistent := Persistent;
      if Persistent then C.Save_Path := new String'(Save_Path); end if;
   end Initialize;

end Gade.Carts.Constructors;
