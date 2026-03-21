package body Gade.Carts.Constructors is
   use type Gade.Logging.Logger_Access;

   procedure Initialize
     (C          : out Cart;
      Save_Path  : String;
      Persistent : Boolean;
      Logger     : Gade.Logging.Logger_Access) is
   begin
      C.Save_Path := null;
      C.Persistent := Persistent;
      C.Logger := (if Logger = null then Gade.Logging.Default_Logger else Logger);
      if Persistent then
         C.Save_Path := new String'(Save_Path);
      end if;
   end Initialize;

end Gade.Carts.Constructors;
