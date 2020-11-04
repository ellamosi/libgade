package Gade.Audio.Channels.Pulse is

   type Pulse_Channel is abstract new Audio_Channel with private;

   procedure Tick_Volume_Envelope (Channel : in out Pulse_Channel);

private

   type Envelope_Volume is mod 2 ** 4;
   type Envelope_Direction is (Down, Up);
   for Envelope_Direction use (Down => 0, Up => 1);
   subtype Envelope_Period is Effect_Period_IO;

   Volume_Max_Level : constant := Envelope_Volume'Last;
   Volume_Min_Level : constant := Envelope_Volume'First;

   Final_Volumes : constant array (Envelope_Direction) of Natural :=
     (Down => Volume_Min_Level, Up => Volume_Max_Level);

   Steps : constant array (Envelope_Direction) of Integer := (-1, 1);

   type Pulse_State_Type is (Pulse_Low, Pulse_High);

   type Pulse_Levels_Type is array (Pulse_State_Type) of Sample;


   NRx2_Volume_Envelope_Mask : constant Byte := 16#00#;

   type NRx2_Volume_Envelope_IO is record
      Volume    : Envelope_Volume;
      Direction : Envelope_Direction;
      Period    : Envelope_Period;
   end record;
   for NRx2_Volume_Envelope_IO use record
      Volume    at 0 range 4 .. 7;
      Direction at 0 range 3 .. 3;
      Period    at 0 range 0 .. 2;
   end record;
   for NRx2_Volume_Envelope_IO'Size use Byte'Size;

   function To_NRx2_Volume_Envelope_IO is new Ada.Unchecked_Conversion
     (Source => Byte,
      Target => NRx2_Volume_Envelope_IO);


   type Volume_Envelope_Details is record
      Current_Volume : Natural; --  Keep track of volume for stepping
      Initial_Volume : Natural;
      Step           : Integer;
      Direction      : Envelope_Direction;
      Period         : Envelope_Period;
      Timer          : Timer_Type;
   end record;

   function Powers_DAC (NRx2_In : NRx2_Volume_Envelope_IO) return Boolean;

   procedure Reload_Timer (Envelope : in out Volume_Envelope_Details;
                           Stop     : Boolean := False);


   --  All the Pulse based channels have a maximum length of 64 (6 bit), only
   --  the wave channel differs with a maximum length of 256 (8 bit).
   Length_Bit_Size : constant := 6;

   package Pulse_Length_Trigger is new Length_Trigger (Length_Bit_Size);
   use Pulse_Length_Trigger;

   subtype Parent is Length_Trigger_Channel;
   type Pulse_Channel is abstract new Parent with record
      Pulse_Levels    : Pulse_Levels_Type;
      NRx2            : Byte;
      Volume_Envelope : Volume_Envelope_Details;
   end record;

   overriding
   procedure Disable
     (Channel : in out Pulse_Channel;
      Mode    : Disable_Mode);

   overriding
   function Read_NRx2 (Channel : Pulse_Channel) return Byte;

   overriding
   procedure Write_NRx2
     (Channel : in out Pulse_Channel;
      Value   : Byte);

   overriding
   procedure Trigger (Channel : in out Pulse_Channel);

   procedure Set_Volume
     (Channel : in out Pulse_Channel;
      Volume  : Natural);

   procedure Step_Volume_Envelope (Channel : in out Pulse_Channel);

end Gade.Audio.Channels.Pulse;
