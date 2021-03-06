with Gade.Dev;            use Gade.Dev;
with Gade.Dev.CPU;        use Gade.Dev.CPU;
with Gade.Dev.VRAM;       use Gade.Dev.VRAM;
with Gade.Dev.OAM;        use Gade.Dev.OAM;
with Gade.Dev.Joypad;     use Gade.Dev.Joypad;
with Gade.Dev.Timer;      use Gade.Dev.Timer;
with Gade.Dev.Display;    use Gade.Dev.Display;
with Gade.Dev.Interrupts; use Gade.Dev.Interrupts;
with Gade.Video_Buffer;   use Gade.Video_Buffer;
with Gade.Carts;          use Gade.Carts;

private package Gade.GB is

   --  0000: 16kB ROM bank #0 | 32kB Cartridge (First Half)
   --  4000: 16kB switchable ROM bank | 32kB Cartridge (Second Half)
   --  8000: 8kB Video RAM
   --  A000: 8kB switchable RAM bank
   --  C000: 8kB Internal RAM
   --  E000: Echo of 8kB Internal RAM
   --  FE00: Sprite Attrib Memory (OAM)
   --  FEA0: Empty but unusable for I/O
   --  FF00: I/O ports
   --  FF4C: Empty but unusable for I/O
   --  FF80: Internal RAM
   --  FFFF: Interrupt Enable Register

   type Memory_Bytes is array (Word'Range) of Byte;

   type GB_Public_Type is abstract tagged limited record
      CPU              : aliased CPU_Context;
      Cart             : Carts.Cart_Access := null;
      Video_RAM        : aliased VRAM_Type;
      Content          : Memory_Bytes;
      Video_OAM        : aliased OAM_Type;
      Interrupt_Flag   : aliased Interrupt_Flag_Type;
      Joypad           : aliased Joypad_Type;
      Timer            : aliased Timer_Type;
      Display          : aliased Display_Type;
      Interrupt_Enable : aliased Interrupt_Enable_Type;
   end record;

   type GB_Type is new GB_Public_Type with private;

   procedure Create (GB : out GB_Type);

   procedure Reset (GB : in out GB_Type);

   --  TODO: Revisit how sync components for performance/cleanliness
   procedure Report_Cycles
     (GB     : in out GB_Type;
      Video  : RGB32_Display_Buffer_Access;
      Cycles : Positive);

   procedure Report_Frame
     (GB     : in out GB_Type;
      Cycles : Positive);

private

   type Device_Type is (CPU, Dev1, Dev2);

   type Device_Access is access all Hardware_Device'Class;
   type Device_Array is array (Device_Type) of Device_Access;

   type GB_Type is new GB_Public_Type with record
      Devices : Device_Array;
   end record;

end Gade.GB;
