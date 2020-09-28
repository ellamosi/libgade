with Gade.Audio.Square.Plain; use Gade.Audio.Square.Plain;

package Gade.Audio.Square.Sweeping is

   Read_Masks : constant Register_Masks :=
     (NRx0 => 16#80#) & Plain.Read_Masks (NRx1 .. NRx4);

   type Sweeping_Square_Channel is new Plain_Square_Channel with private;

   overriding
   procedure Extended_Reset (Ch : in out Sweeping_Square_Channel);

   overriding
   function Get_Read_Masks (Ch : Sweeping_Square_Channel) return Register_Masks;

   overriding
   procedure Extended_Trigger (Ch : in out Sweeping_Square_Channel);

   procedure Frequency_Sweep_Step (Ch : in out Sweeping_Square_Channel);

private

   Sweep_IO_Mask : constant Byte := 16#80#; --  Most significant bit is unused

   type Sweeping_Square_Channel is new Plain_Square_Channel with record
      --  TODO: Revise names, sweep_ suffixing
      Sweep_Enabled    : Boolean;
      Shadow_Frequency : Frequency_Type;
      Sweep_Timer      : Timer;
      Sweep_Shift      : Sweep_Shift_Type;
      Sweep_Negate     : Boolean;
   end record;

   procedure Trigger_Frequency_Sweep
     (Ch : in out Sweeping_Square_Channel);
   procedure Frequency_Sweep_Shift (Ch : in out Sweeping_Square_Channel);

end Gade.Audio.Square.Sweeping;
