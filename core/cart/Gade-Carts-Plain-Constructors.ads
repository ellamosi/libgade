with Gade.Carts.Memory_Contents; use Gade.Carts.Memory_Contents;

package Gade.Carts.Plain.Constructors is

   function Create
     (Content  : ROM_Content_Access;
      Header   : Cart_Header_Access; -- Could be made a non acces argument, type needs to be separated
      RAM_Path : String)
      return Plain_Cart_NN_Access;

private

   procedure Initialize
     (C        : out Plain_Cart'Class;
      Content  : ROM_Content_Access;
      Header   : Cart_Header_Access;
      RAM_Path : String);

   subtype Plain_RAM_Size_Type is RAM_Size_Type range None .. RAM_64kbit;

   Address_Mask_For_RAM_Size : constant array (Plain_RAM_Size_Type)
     of Word :=
       (None        => 16#0000#, -- Does not really matter
        RAM_16kbit  => 16#07FF#,
        RAM_64kbit  => 16#1FFF#);

end Gade.Carts.Plain.Constructors;
