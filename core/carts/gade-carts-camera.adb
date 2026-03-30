package body Gade.Carts.Camera is
   package Banked_RAM_Mixin renames ROM_RAM_Mixin.Banked_RAM_Mixin;

   Pattern_Base_Offset : constant := 16#0100#;
   Pattern_Height      : constant := 112;
   Pattern_Width       : constant := 128;

   function Capture_Cycles (C : Camera_Cart) return M_Cycle_Count;

   function Pattern_Color (X : Natural; Y : Natural) return Byte;

   function Register_Index (Address : External_RAM_IO_Address) return Natural;

   procedure Complete_Capture (C : in out Camera_Cart);

   procedure Resume_Capture (C : in out Camera_Cart);

   procedure Stop_Capture (C : in out Camera_Cart);

   procedure Write_Pattern (C : in out Camera_Cart);

   function Capture_Cycles (C : Camera_Cart) return M_Cycle_Count is
      Exposure_Steps : constant Natural :=
        Natural (Word (C.Registers (2)) * 2**8 + Word (C.Registers (3)));
      N_Bit_Cycles   : constant M_Cycle_Count :=
        (if (C.Registers (1) and 16#80#) = 0 then 512 else 0);
   begin
      return 32_446 + N_Bit_Cycles + M_Cycle_Count (16 * Exposure_Steps);
   end Capture_Cycles;

   overriding
   procedure Enable_RAM (C : in out Camera_Cart; Enable : Boolean) is
   begin
      C.RAM_Write_Enabled := Enable;
      Banked_RAM_Mixin.Enable_RAM (Banked_RAM_Mixin.Banked_RAM_Cart (C), True);
      if not C.Registers_Selected then
         Banked_RAM_Mixin.Select_RAM_Bank
           (Banked_RAM_Mixin.Banked_RAM_Cart (C), C.Selected_RAM_Bank);
      end if;
   end Enable_RAM;

   procedure Complete_Capture (C : in out Camera_Cart) is
   begin
      C.Capture_Active := False;
      C.Capture_Cycles_Left := 0;
      Write_Pattern (C);
   end Complete_Capture;

   function Pattern_Color (X : Natural; Y : Natural) return Byte is
      In_Border : constant Boolean :=
        X < 8
        or else X >= Pattern_Width - 8
        or else Y < 8
        or else Y >= Pattern_Height - 8;
   begin
      if In_Border then
         return 3;
      else
         return Byte ((X / 32) mod 4);
      end if;
   end Pattern_Color;

   function Register_Index (Address : External_RAM_IO_Address) return Natural is
   begin
      return Natural (Address - External_RAM_IO_Address'First) mod Register_Mirror_Size;
   end Register_Index;

   overriding
   procedure Read_RAM
     (C : in out Camera_Cart; Address : External_RAM_IO_Address; V : out Byte) is
   begin
      if C.Registers_Selected then
         Read_Register (C, Address, V);
      elsif C.Capture_Active then
         V := 16#00#;
      else
         Banked_RAM_Mixin.Read_RAM (Banked_RAM_Mixin.Banked_RAM_Cart (C), Address, V);
      end if;
   end Read_RAM;

   procedure Read_Register
     (C : in out Camera_Cart; Address : External_RAM_IO_Address; V : out Byte)
   is
      Idx : constant Natural := Register_Index (Address);
   begin
      if Idx = 0 then
         V :=
           (C.Registers (0) and Register_0_Read_Mask)
           or (if C.Capture_Active then 16#01# else 16#00#);
      else
         V := 16#00#;
      end if;
   end Read_Register;

   overriding
   procedure Report_Cycles (C : in out Camera_Cart; Cycles : M_Cycle_Count) is
   begin
      if not C.Capture_Active then
         return;
      end if;

      if Cycles >= C.Capture_Cycles_Left then
         Complete_Capture (C);
      else
         C.Capture_Cycles_Left := C.Capture_Cycles_Left - Cycles;
      end if;
   end Report_Cycles;

   overriding
   procedure Reset (C : in out Camera_Cart) is
   begin
      MBC_Cart (C).Reset;
      Banked_RAM_Mixin.Enable_RAM (Banked_RAM_Mixin.Banked_RAM_Cart (C), True);
      Banked_RAM_Mixin.Select_RAM_Bank (Banked_RAM_Mixin.Banked_RAM_Cart (C), 0);
      C.Capture_Active := False;
      C.Capture_Cycles_Left := 0;
      C.RAM_Write_Enabled := False;
      C.Registers := [others => 0];
      C.Registers_Selected := False;
      C.Selected_RAM_Bank := 0;
   end Reset;

   procedure Resume_Capture (C : in out Camera_Cart) is
   begin
      if C.Capture_Active then
         return;
      end if;

      if C.Capture_Cycles_Left = 0 then
         C.Capture_Cycles_Left := Capture_Cycles (C);
      end if;

      C.Capture_Active := True;
   end Resume_Capture;

   overriding
   procedure Select_Bank
     (C : in out Camera_Cart; Address : Bank_Select_Address; Value : Byte) is
   begin
      if Address in ROM_Bank_Select_Address then
         Select_ROM_Bank (C, Value);
      elsif Address in RAM_Bank_Select_Address then
         Select_RAM_Bank (C, Value);
      end if;
   end Select_Bank;

   procedure Select_RAM_Bank (C : in out Camera_Cart; Value : Byte) is
   begin
      if (Value and RAM_Select_Bit) /= 0 then
         C.Registers_Selected := True;
      else
         C.Registers_Selected := False;
         C.Selected_RAM_Bank :=
           ROM_RAM_Mixin.Banked_RAM_Mixin.Banked_RAM_Spaces.Bank_Index
             (Value and RAM_Index_Mask);
         Banked_RAM_Mixin.Select_RAM_Bank
           (Banked_RAM_Mixin.Banked_RAM_Cart (C), C.Selected_RAM_Bank);
      end if;
   end Select_RAM_Bank;

   procedure Select_ROM_Bank (C : in out Camera_Cart; Value : Byte) is
      use ROM_RAM_Mixin.Banked_ROM_Mixin.Banked_ROM_Spaces;
   begin
      C.Select_ROM_Bank (1, Bank_Index (Value and ROM_Index_Mask));
   end Select_ROM_Bank;

   procedure Stop_Capture (C : in out Camera_Cart) is
   begin
      C.Capture_Active := False;
   end Stop_Capture;

   overriding
   procedure Write_RAM
     (C : in out Camera_Cart; Address : External_RAM_IO_Address; V : Byte) is
   begin
      if C.Registers_Selected then
         Write_Register (C, Address, V);
      elsif C.Capture_Active or else not C.RAM_Write_Enabled then
         null;
      else
         Banked_RAM_Mixin.Write_RAM (Banked_RAM_Mixin.Banked_RAM_Cart (C), Address, V);
      end if;
   end Write_RAM;

   procedure Write_Pattern (C : in out Camera_Cart) is
      Address         : External_RAM_IO_Address;
      High            : Byte;
      Low             : Byte;
      Original_Bank   : constant Banked_RAM_Mixin.Banked_RAM_Spaces.Bank_Index :=
        C.Selected_RAM_Bank;
      Original_Select : constant Boolean := C.Registers_Selected;
      Shade           : Byte;
      X               : Natural;
      Y               : Natural;
   begin
      Banked_RAM_Mixin.Select_RAM_Bank (Banked_RAM_Mixin.Banked_RAM_Cart (C), 0);

      for Tile_Y in 0 .. (Pattern_Height / 8) - 1 loop
         for Tile_X in 0 .. (Pattern_Width / 8) - 1 loop
            for Row in 0 .. 7 loop
               High := 16#00#;
               Low := 16#00#;

               for Col in 0 .. 7 loop
                  X := Tile_X * 8 + Col;
                  Y := Tile_Y * 8 + Row;
                  Shade := Pattern_Color (X, Y);
                  if (Shade and 16#01#) /= 0 then
                     Low := Low or 2**(7 - Col);
                  end if;
                  if (Shade and 16#02#) /= 0 then
                     High := High or 2**(7 - Col);
                  end if;
               end loop;

               Address :=
                 External_RAM_IO_Address'First
                 + Word (Pattern_Base_Offset + ((Tile_Y * 16 + Tile_X) * 16) + Row * 2);
               Banked_RAM_Mixin.Write_RAM
                 (Banked_RAM_Mixin.Banked_RAM_Cart (C), Address, Low);
               Banked_RAM_Mixin.Write_RAM
                 (Banked_RAM_Mixin.Banked_RAM_Cart (C), Address + 1, High);
            end loop;
         end loop;
      end loop;

      Banked_RAM_Mixin.Select_RAM_Bank
        (Banked_RAM_Mixin.Banked_RAM_Cart (C), Original_Bank);
      C.Registers_Selected := Original_Select;
   end Write_Pattern;

   procedure Write_Register
     (C : in out Camera_Cart; Address : External_RAM_IO_Address; V : Byte)
   is
      Idx : constant Natural := Register_Index (Address);
   begin
      if Idx = 0 then
         C.Registers (0) := V and Register_0_Write_Mask;
         if (V and 16#01#) /= 0 then
            Resume_Capture (C);
         else
            Stop_Capture (C);
         end if;
      elsif Idx in Camera_Register_Index then
         C.Registers (Camera_Register_Index (Idx)) := V;
      end if;
   end Write_Register;

end Gade.Carts.Camera;
