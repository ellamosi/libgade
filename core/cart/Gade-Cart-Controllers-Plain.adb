package body Gade.Cart.Controllers.Plain is

   function Create
     (ROM_Content : Cart.ROM.Content_Access; RAM_Size : RAM_Size_Type;
      RAM_Path    : String) return Cart_Handler_Access
   is
      Handler : constant Cart_Handler_Access := new Cart_Handler;
   begin
      Handler.ROM_Handler := ROM_Space.Create (ROM_Content);
      Handler.RAM_Handler := RAM_Space.Create (RAM_Size, RAM_Path);
      return Handler;
   end Create;

   ---------------
   -- ROM_Space --
   ---------------

   package body ROM_Space is

      function Create (Content : Cart.ROM.Content_Access) return Handler_Access
      is
         Handler : constant Handler_Access := new Handler_Type;
      begin
         ROM_Space.Initialize (Handler.all, Content);
         return Handler;
      end Create;

      procedure Initialize
        (Handler     : out Handler_Type'Class;
         ROM_Content :     Cart.ROM.Content_Access)
      is
      begin
         Spaces.ROM.Initialize (Handler, ROM_Content);
         Handler.Reset;
      end Initialize;

   end ROM_Space;

   ---------------
   -- RAM_Space --
   ---------------

   package body RAM_Space is

      function Create
        (Size : RAM_Size_Type; Path : String) return Handler_Access
      is
         Handler : constant Handler_Access := new Handler_Type;
      begin
         RAM_Space.Initialize (Handler.all, Size, Path);
         return Handler;
      end Create;

      procedure Initialize
        (Handler : out Handler_Type'Class; Size : RAM_Size_Type; Path : String)
      is
      begin
         Spaces.RAM.Initialize (Handler, Size, Path);
      end Initialize;

   end RAM_Space;

end Gade.Cart.Controllers.Plain;
