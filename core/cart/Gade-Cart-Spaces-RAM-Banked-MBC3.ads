with Gade.Cart.Banked.RAM.RTC;

package Gade.Cart.Spaces.RAM.Banked.MBC3 is

   type Handler_Type is new Banked.Handler_Type with private;
   type Handler_Access is access Handler_Type;

   function Create
     (Size : RAM_Size_Type;
      Path : String) return Handler_Access;

   overriding
   procedure Switch_Banks
     (Handler : in out Handler_Type;
      Index   : Bank_Index);

   overriding
   procedure Save (Handler : Handler_Type);

private

   type Handler_Type is new Banked.Handler_Type with record
      RTC_Bank : RTC.Handler_Access;
   end record;

   procedure Initialize
     (Handler : out Handler_Type'Class;
      Size    : RAM_Size_Type;
      Path    : String);

end Gade.Cart.Spaces.RAM.Banked.MBC3;
