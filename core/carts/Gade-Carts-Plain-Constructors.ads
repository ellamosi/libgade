with Gade.Carts.Mem.ROM; use Gade.Carts.Mem.ROM;
private with Gade.Carts.Mixins.ROM_RAM.Constructors;

package Gade.Carts.Plain.Constructors is

   function Create
     (Content  : ROM_Content_Access;
      Header   : Cart_Header;
      RAM_Path : String)
      return Plain_Cart_NN_Access;

private

   package ROM_RAM_Constructors is new ROM_RAM_Mixin.Constructors;

   procedure Initialize
     (C           : out Plain_Cart'Class;
      ROM_Content : ROM_Content_Access;
      Header      : Cart_Header;
      RAM_Path    : String);

   subtype Plain_RAM_Size_Type is RAM_Size_Type range None .. RAM_64kbit;

   Address_Mask_For_RAM_Size : constant array (Plain_RAM_Size_Type)
     of Word :=
       (None        => 16#0000#, -- Unused
        RAM_16kbit  => 16#07FF#,
        RAM_64kbit  => 16#1FFF#);

end Gade.Carts.Plain.Constructors;
