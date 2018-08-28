package body Gade.Cart.ROM_Space.ROM_Only is

   function Create (ROM_Content : ROM_Content_Access)
      return ROM_Only_Space_Access
   is
      Space : constant ROM_Only_Space_Access := new ROM_Only_Space_Type;
   begin
      ROM_Only.Initialize (Space.all, ROM_Content);
      return Space;
   end Create;

   procedure Initialize
     (Space       : out ROM_Only_Space_Type'Class;
      ROM_Content : Gade.Cart.ROM.ROM_Content_Access)
   is
   begin
      ROM_Space_Type (Space).Initialize (ROM_Content);
   end Initialize;

end Gade.Cart.ROM_Space.ROM_Only;
