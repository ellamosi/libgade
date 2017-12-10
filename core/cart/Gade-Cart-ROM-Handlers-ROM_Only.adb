package body Gade.Cart.ROM.Handlers.ROM_Only is

   function Create (ROM_Content : ROM_Content_Access)
      return ROM_Only_Handler_Access
   is
      Handler : constant ROM_Only_Handler_Access := new ROM_Only_Handler_Type;
   begin
      ROM_Only.Initialize (Handler.all, ROM_Content);
      return Handler;
   end Create;

   procedure Initialize
     (Handler     : out ROM_Only_Handler_Type'Class;
      ROM_Content : Gade.Cart.ROM.ROM_Content_Access)
   is
   begin
      ROM_Handler_Type (Handler).Initialize (ROM_Content);
   end Initialize;

end Gade.Cart.ROM.Handlers.ROM_Only;
