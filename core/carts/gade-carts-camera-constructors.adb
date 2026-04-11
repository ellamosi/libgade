with Gade.Carts.Constructors;
with Gade.Camera;
with Gade.Carts.Mem.RAM; use Gade.Carts.Mem.RAM;
with Gade.Carts.Mixins.MBC.Constructors;

package body Gade.Carts.Camera.Constructors is
   package MBC_Constructors is new MBC_Mixin.Constructors;

   procedure Initialize
     (C           : out Camera_Cart'Class;
      ROM_Content : ROM_Content_Access;
      Header      : Cart_Header;
      RAM_Path    : String;
      Logger      : Gade.Logging.Logger_Access);

   function Create
     (ROM_Content : ROM_Content_Access;
      Header      : Cart_Header;
      RAM_Path    : String;
      Logger      : Gade.Logging.Logger_Access) return Camera_Cart_NN_Access
   is
      Result : constant Camera_Cart_NN_Access := new Camera_Cart;
   begin
      Initialize (Result.all, ROM_Content, Header, RAM_Path, Logger);
      return Result;
   end Create;

   procedure Initialize
     (C           : out Camera_Cart'Class;
      ROM_Content : ROM_Content_Access;
      Header      : Cart_Header;
      RAM_Path    : String;
      Logger      : Gade.Logging.Logger_Access)
   is
      Has_Battery : constant Boolean :=
        Cart_Type_Info_For_Cart (Header.Cart_Type).Battery;
      RAM_Content : constant RAM_Content_Access :=
        Create
          (Header.RAM_Size,
           ROM_RAM_Mixin.Banked_RAM_Mixin.Banked_RAM_Spaces.Max_Content_Size);
      Savable     : constant Boolean := Has_Battery and then RAM_Content /= null;
   begin
      Gade.Carts.Constructors.Initialize (Cart (C), RAM_Path, Savable, Logger);
      MBC_Constructors.Initialize (C, ROM_Content, RAM_Content);
      C.Provider := Gade.Camera.Default_Provider;
      Reset (C);
   end Initialize;

end Gade.Carts.Camera.Constructors;
