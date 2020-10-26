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
      Audio_Channel'Class (Channel).Turn_Off;
      Audio_Channel'Class (Channel).Turn_On;
   end Reset;

   procedure Turn_On (Channel : in out Audio_Channel) is
   begin
      Channel.Powered := True;
   end Turn_On;

   procedure Turn_Off (Channel : in out Audio_Channel) is
   begin
      Audio_Channel'Class (Channel).Disable (APU_Power_Off);
   end Turn_Off;

   procedure Trigger (Channel : in out Audio_Channel) is
   begin
      --  We don't know how long the next sample will be yet, fetch next
      --  sample in the following tick: (UGLY)
      Channel.Sample_Timer.Start (1);
   end Trigger;

   procedure Disable
     (Channel : in out Audio_Channel;
      Mode    : Disable_Mode) is
   begin
      Channel.Powered := Mode /= APU_Power_Off;
      Channel.Level := 0;
      Channel.Sample_Timer.Setup;
   end Disable;

   function Enabled (Channel : Audio_Channel) return Boolean is
   begin
      return Channel.Sample_Timer.Enabled;
   end Enabled;

   procedure Next_Sample
     (Channel : in out Audio_Channel;
      S       : out Sample)
   is
      procedure Tick_Notify_Sample_Step is new Tick_Notify
        (Observer_Type => Audio_Channel,
         Finished      => Step_Sample);
   begin
      S := Channel.Level;
      --  TODO: Can probably avoid this IF by just disabling the sample timer
      --  Put_Line ("NS:" & Channel.Enabled'Img);
      if Channel.Enabled then
         --  Put_Line ("Tick_Notify_Sample_Step");
         Tick_Notify_Sample_Step (Channel.Sample_Timer, Channel);
      end if;
   end Next_Sample;

   procedure Step_Sample (Channel : in out Audio_Channel) is
      New_Sample_Level : Sample;
      New_Level_Time   : Positive;
   begin
      --  Could maybe find a way to avoid a dynamic dispatch here for
      --  performance. Inlinable generic function?
      Audio_Channel'Class (Channel).Next_Sample_Level
        (New_Sample_Level, New_Level_Time);
      Channel.Level := New_Sample_Level;
      Channel.Sample_Timer.Start (New_Level_Time);
   end Step_Sample;

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

   function Name (Channel : Audio_Channel'Class) return String is
      Id : constant Channel_Id := Channel.Id;
   begin
      return
        (case Id is
            when NR1 => "Square 1",
            when NR2 => "Square 2",
            when NR3 => "Wave",
            when NR4 => "Noise");
   end Name;

   package body Length_Trigger is separate;
   package body Frequency_Mixin is separate;

end Gade.Audio.Channels;
