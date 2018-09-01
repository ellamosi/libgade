package body Gade.Cart.Spaces.ROM.Plain is

   function Create (ROM_Content : ROM_Content_Access)
      return Plain_ROM_Space_Access
   is
      Space : constant Plain_ROM_Space_Access := new Plain_ROM_Space_Type;
   begin
      Plain.Initialize (Space.all, ROM_Content);
      return Space;
   end Create;

   procedure Initialize
     (Space       : out Plain_ROM_Space_Type'Class;
      ROM_Content : Gade.Cart.ROM.ROM_Content_Access)
   is
   begin
      ROM_Space_Type (Space).Initialize (ROM_Content);
   end Initialize;

end Gade.Cart.Spaces.ROM.Plain;
