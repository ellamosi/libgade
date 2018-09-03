package body Gade.Cart.Spaces.ROM.Plain is

   function Create (Content : Cart.ROM.Content_Access) return Handler_Access is
      Handler : constant Handler_Access := new Handler_Type;
   begin
      Plain.Initialize (Handler.all, Content);
      return Handler;
   end Create;

   procedure Initialize
     (Handler     : out Handler_Type'Class;
      ROM_Content : Cart.ROM.Content_Access)
   is
   begin
      ROM.Handler_Type (Handler).Initialize (ROM_Content);
   end Initialize;

end Gade.Cart.Spaces.ROM.Plain;

