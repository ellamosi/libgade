private package Gade.Dev.Display.Handlers is

   type Handler_Common_Info_Type is abstract tagged record
      Dev          : Display_Access;
      Current_Line : Line_Count_Type;
   end record;

   type Display_Handler_Type is new Handler_Common_Info_Type with private;
   type Display_Handler_Access is access all Display_Handler_Type;

   function Create (Dev : Display_Access) return Display_Handler_Access;

   procedure Reset (Handler : in out Display_Handler_Type);

   procedure Report_Cycles
     (Handler : in out Display_Handler_Type;
      GB      : in out Gade.GB.GB_Type;
      Video   : RGB32_Display_Buffer_Access;
      Cycles  : Natural);

   procedure Notify_Display_Write
     (Handler : in out Display_Handler_Type; Address : Word; Value : Byte);

private

   type Pending_Display_Write is record
      Active  : Boolean := False;
      Phase   : Natural := 0;
      Address : Display_IO_Address := Display_IO_Address'First;
      Value   : Byte := 0;
   end record;

   subtype Pending_Display_Write_Index is Positive range 1 .. 8;
   type Pending_Display_Write_Array is
     array (Pending_Display_Write_Index) of Pending_Display_Write;

   type Mode_Handler_Type is abstract tagged;
   type Mode_Handler_Access is access all Mode_Handler_Type'Class;

   type Handler_Array is array (LCD_Controller_Mode_Type) of Mode_Handler_Access;

   --  TODO: This should go to its own file
   subtype Line_Buffer_Range is Natural range 0 .. 160 - 1;
   type Line_Buffer_Type is array (Line_Buffer_Range) of Natural;

   type Display_Handler_Type is new Handler_Common_Info_Type with record
      Mode                 : LCD_Controller_Mode_Type;
      Mode_Handlers        : Handler_Array;
      Current_Mode_Handler : Mode_Handler_Access;
      Sprite_Cache         : Sprite_Line_Cache;
      Timing_Cache         : Line_Buffer_Type;
      VRAM_Access_Cycles   : Natural;
      Latched_Map          : LCD_Map_Type;
      Pending_Writes       : Pending_Display_Write_Array;
      Pending_Line         : Line_Count_Type := 0;
      Pending_Line_Valid   : Boolean := False;
      Window_Line_Counter  : Natural;
      Window_Line_Active   : Boolean;
   end record;

   procedure Line_Changed
     (Handler  : in out Display_Handler_Type;
      GB       : in out Gade.GB.GB_Type;
      New_Line : Line_Count_Type);

   type Mode_Handler_Type is abstract tagged record
      Display_Handler  : Display_Handler_Access;
      Dev              : access Display_Type;
      Remaining_Cycles : Natural;
      Finished         : Boolean;
   end record;

   procedure Setup
     (Mode_Handler    : in out Mode_Handler_Type;
      Display_Handler : Display_Handler_Access;
      Dev             : access Display_Type);

   procedure Reset (Mode_Handler : in out Mode_Handler_Type);

   procedure Start
     (Mode_Handler : in out Mode_Handler_Type;
      GB           : in out Gade.GB.GB_Type;
      Video        : RGB32_Display_Buffer_Access);

   procedure Report_Cycles
     (Mode_Handler     : in out Mode_Handler_Type;
      GB               : in out Gade.GB.GB_Type;
      Video            : RGB32_Display_Buffer_Access;
      Cycles           : Natural;
      Remaining_Cycles : out Natural);

   procedure Mode_Finished
     (Mode_Handler : in out Mode_Handler_Type; GB : in out Gade.GB.GB_Type);

   function Is_Mode_Finished (Mode_Handler : Mode_Handler_Type) return Boolean;

   function Next_Mode (Mode_Handler : Mode_Handler_Type) return LCD_Controller_Mode_Type
   is abstract;

end Gade.Dev.Display.Handlers;
