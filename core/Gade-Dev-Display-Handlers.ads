private package Gade.Dev.Display.Handlers is

   type Handler_Common_Info_Type is abstract tagged record
      Dev          : Display_Access;
      Current_Line : Line_Count_Type;
   end record;

   type Display_Handler_Type is new Handler_Common_Info_Type with private;
   type Display_Handler_Access is access all Display_Handler_Type;

   function Create (Dev : Display_Access) return Display_Handler_Access;

   procedure Reset
     (Handler : in out Display_Handler_Type);

   procedure Report_Cycles
     (Handler : in out Display_Handler_Type;
      GB      : in out Gade.GB.GB_Type;
      Video   : RGB32_Display_Buffer_Access;
      Cycles  : Natural);

private

   type Mode_Handler_Type is abstract tagged;
   type Mode_Handler_Access is access all Mode_Handler_Type'Class;

   type Handler_Array is array (LCD_Controller_Mode_Type)
     of Mode_Handler_Access;

   type Display_Handler_Type is new Handler_Common_Info_Type with record
      Mode                 : LCD_Controller_Mode_Type;
      Mode_Handlers        : Handler_Array;
      Current_Mode_Handler : Mode_Handler_Access;
      Sprite_Cache         : Sprite_Line_Cache;
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

   procedure Reset
     (Mode_Handler : in out Mode_Handler_Type);

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
     (Mode_Handler : in out Mode_Handler_Type;
      GB           : in out Gade.GB.GB_Type);

   function Is_Mode_Finished
     (Mode_Handler : Mode_Handler_Type) return Boolean;

   function Next_Mode
     (Mode_Handler : Mode_Handler_Type) return LCD_Controller_Mode_Type
      is abstract;

end Gade.Dev.Display.Handlers;
