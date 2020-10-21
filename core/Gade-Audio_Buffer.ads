package Gade.Audio_Buffer is

   --  Gambatte:
   --  There are 35112 audio (stereo) samples in a video frame.
   --  An audio sample consists of two native endian 2s complement 16-bit PCM samples
   --  May run for up to 2064 audio samples too long.

   --  Own draft:
   --  35112 samples/frame at ~59.7 fps => 2096186.4 samples/sec (~2096kHz)

   Sample_Bit_Size : constant := 16;
   Sample_Minimum  : constant := -(2 ** (Sample_Bit_Size - 1));
   Sample_Maximum  : constant := 2 ** (Sample_Bit_Size - 1) - 1;

   type Sample is range Sample_Minimum .. Sample_Maximum;
   for Sample'Size use Sample_Bit_Size;

   type Stereo_Sample is record
      Left, Right : Sample;
   end record;

   type Audio_Buffer is array (Natural range <>) of Stereo_Sample;

   Samples_Frame   : constant := 35_112; -- / 2;
   --  Samples_Second  : constant := Samples_Frame * 60; -- 59.727500569606
   Samples_Second  : constant := 2_097_152 / 2;
   Extra_Samples   : constant := 2_064; -- May run for some samples too long
   Maximum_Samples : constant := Samples_Frame + Extra_Samples;

   type Audio_Buffer_Type is new Audio_Buffer (0 .. Maximum_Samples - 1)
     with Convention => C;

   type Audio_Buffer_Access is access all Audio_Buffer_Type
     with Convention => C;

end Gade.Audio_Buffer;
