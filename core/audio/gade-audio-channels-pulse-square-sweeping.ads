package Gade.Audio.Channels.Pulse.Square.Sweeping is

   type Sweeping_Square_Channel is new Square_Channel with private;

   procedure Tick_Frequency_Sweep (Channel : in out Sweeping_Square_Channel);

   overriding
   function Id (Channel : Sweeping_Square_Channel) return Channel_Id;

private

   NRx0_Sweep_Mask : constant Byte := 16#80#;

   subtype Sweep_Period_Type is Effect_Period_IO;
   type Sweep_Shift_Type is mod 2 ** 3;

   type NRx0_Frequency_Sweep_IO is record
      Period : Sweep_Period_Type;
      Negate : Boolean;
      Shift  : Sweep_Shift_Type;
   end record;
   for NRx0_Frequency_Sweep_IO use record
      Period at 0 range 4 .. 6;
      Negate at 0 range 3 .. 3;
      Shift  at 0 range 0 .. 2;
   end record;
   for NRx0_Frequency_Sweep_IO'Size use Byte'Size;

   function To_NRx0_Frequency_Sweep_IO is new Ada.Unchecked_Conversion
     (Source => Byte,
      Target => NRx0_Frequency_Sweep_IO);

   type Frequency_Sweep_Details is record
      Enabled          : Boolean;
      Shadow_Frequency : Frequency_Type;
      Sweep_Timer      : Timer_Type; -- TODO: Rename type, component
      Shift            : Sweep_Shift_Type;
      Negate, Negated  : Boolean;
      Period           : Sweep_Period_Type;
   end record;

   subtype Parent is Square_Channel;
   type Sweeping_Square_Channel is new Parent with record
      NRx0  : Byte;
      Sweep : Frequency_Sweep_Details;
   end record;

   overriding
   procedure Disable
     (Channel : in out Sweeping_Square_Channel;
      Mode    : Disable_Mode);

   overriding
   procedure Trigger (Channel : in out Sweeping_Square_Channel);

   procedure Calculate_New_Frequency
     (Channel       : in out Sweeping_Square_Channel;
      New_Frequency : out Integer);

   procedure Step_Frequency_Sweep (Channel : in out Sweeping_Square_Channel);

   overriding
   function Read_NRx0 (Channel : Sweeping_Square_Channel) return Byte;

   overriding
   procedure Write_NRx0
     (Channel : in out Sweeping_Square_Channel;
      Value : Byte);

end Gade.Audio.Channels.Pulse.Square.Sweeping;
