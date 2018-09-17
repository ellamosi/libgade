with Gade.Cart.RTC; use Gade.Cart.RTC;

package Gade.Cart.Banked.RAM.RTC is

   type Handler_Type is new RAM.Handler_Type with private;
   type Handler_Access is access all Handler_Type;

   procedure Initialize
     (Handler : out Handler_Type;
      Path    : String);

   overriding
   procedure Read
     (Handler : in out Handler_Type;
      Address : Bank_Address;
      Value   : out Byte);

   overriding
   procedure Write
     (Handler : in out Handler_Type;
      Address : Bank_Address;
      Value   : Byte) is null;

   procedure Set_Register
     (Handler : in out Handler_Type;
      Reg     : Register);

   procedure Save
     (Handler : Handler_Type);

private

   type Register_Values is array (Register'Range) of Byte;

   type Handler_Type is new RAM.Handler_Type with record
      Clk     : Clock;
      Path    : Path_Access;
      Current : Register;
      Values  : Register_Values;
   end record;

end Gade.Cart.Banked.RAM.RTC;
