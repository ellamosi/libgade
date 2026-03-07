package Gade.Input is

   type State is record
      A, B, SEL, START, RIGHT, LEFT, UP, DOWN : Boolean;
   end record;
   for State use record
      A      at 0 range 0 .. 0;
      B      at 0 range 1 .. 1;
      SEL    at 0 range 2 .. 2;
      START  at 0 range 3 .. 3;
      RIGHT  at 0 range 4 .. 4;
      LEFT   at 0 range 5 .. 5;
      UP     at 0 range 6 .. 6;
      DOWN   at 0 range 7 .. 7;
   end record;
   for State'Size use 8;
   pragma Convention (C, State);

   type Reader_Interface is interface;
   type Reader_Access is access all Reader_Interface'Class;

   function Read_Input
     (Reader : Reader_Interface) return State is abstract;

end Gade.Input;
