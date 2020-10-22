with Ada.Text_IO; use Ada.Text_IO;

package body Gade.Audio.Channels is

   procedure Create
     (Channel : out Audio_Channel;
      Audio   : Audio_Type)
   is
   begin
      Channel.Audio := Audio;
   end Create;

   procedure Reset (Channel : in out Audio_Channel) is
   begin
      --  TODO: Handle wave table initial value
      Audio_Channel'Class (Channel).Disable (APU_Power_Off);
   end Reset;

   procedure Turn_On (Channel : in out Audio_Channel) is
   begin
      Channel.Powered := True;
   end Turn_On;

   procedure Turn_Off (Channel : in out Audio_Channel) is
   begin
      Audio_Channel'Class (Channel).Disable (APU_Power_Off);
   end Turn_Off;

   procedure Disable
     (Channel : in out Audio_Channel;
      Mode    : Disable_Mode) is
   begin
      Channel.Powered := Mode /= APU_Power_Off;
   end Disable;

   function Read
     (Channel  : Audio_Channel'Class;
      Register : Channel_Register)
      return Byte
   is
   begin
      return
        (case Register is
            when NRx0 => Channel.Read_NRx0,
            when NRx1 => Channel.Read_NRx1,
            when NRx2 => Channel.Read_NRx2,
            when NRx3 => Channel.Read_NRx3,
            when NRx4 => Channel.Read_NRx4);
   end Read;

   procedure Write
     (Channel  : in out Audio_Channel'Class;
      Register : Channel_Register;
      Value    : Byte)
   is
   begin
      if Channel.Powered or Register = NRx1 then
         case Register is
         when NRx0 => Channel.Write_NRx0 (Value);
         when NRx1 => Channel.Write_NRx1 (Value);
         when NRx2 => Channel.Write_NRx2 (Value);
         when NRx3 => Channel.Write_NRx3 (Value);
         when NRx4 => Channel.Write_NRx4 (Value);
         end case;
      end if;
   end Write;

   function Read_Blank (Channel : Audio_Channel) return Byte is
      pragma Unreferenced (Channel);
   begin
      return Blank_Value;
   end Read_Blank;

   package body Length_Trigger is separate;
   package body Frequency_Mixin is separate;

end Gade.Audio.Channels;
