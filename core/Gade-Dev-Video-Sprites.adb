with Gade.Dev.VRAM; use Gade.Dev.VRAM;

package body Gade.Dev.Video.Sprites is

   procedure Insert_By_Processing_Priority
     (Buffer   : in out Sprite_Priority_Buffer;
      Index    : Sprite_Index_Type)
   is
   begin
      --  Sprites are already sorted by index, so just append them
      Buffer.N_Sprites := Buffer.N_Sprites + 1;
      Buffer.Indexes (Buffer.N_Sprites) := Index;
   end Insert_By_Processing_Priority;

   procedure Insert_By_Draw_Priority
     (Buffer  : in out Sprite_Priority_Buffer;
      Index   : Sprite_Index_Type;
      Sprites : Sprite_Array_Type)
   is
      function Compare_Priorities (L, R : Sprite_Index_Type) return Boolean;
      function Compare_Priorities (L, R : Sprite_Index_Type) return Boolean is
      begin
         return
           Sprites (L).X < Sprites (R).X or
           (Sprites (L).X = Sprites (R).X and L < R);
      end Compare_Priorities;

      I : Natural := Buffer.N_Sprites;
   begin
      while I >= 1 and then Compare_Priorities (Index, Buffer.Indexes (I)) loop
         I := I - 1;
      end loop;
      Buffer.Indexes (I + 2 .. Buffer.N_Sprites + 1) :=
         Buffer.Indexes (I + 1 .. Buffer.N_Sprites);
      Buffer.N_Sprites := Buffer.N_Sprites + 1;
      Buffer.Indexes (I + 1) := Index;
   end Insert_By_Draw_Priority;

   procedure Prioritize_Sprites
     (Buffer  : out Sprite_Priority_Buffer;
      Sprites : Sprite_Array_Type;
      Row     : Display_Vertical_Range;
      Size    : Sprite_Size_Type)
   is
      function Is_Enabled (Sprite : Sprite_Type) return Boolean;
      function Is_Enabled (Sprite : Sprite_Type) return Boolean is
         Sprite_Row : constant Integer :=
           Row - (Natural (Sprite.Y) - Double_Sprite_Height);
      begin
         --  If a sprite is in the line it gets processed regardless of whether
         --  it's visible or not on its X coordinate
         case Size is
            when Single => return Sprite_Row in 0 .. Single_Sprite_Height - 1;
            when Double => return Sprite_Row in 0 .. Double_Sprite_Height - 1;
         end case;
      end Is_Enabled;

      function Is_Visible (Sprite : Sprite_Type) return Boolean;
      function Is_Visible (Sprite : Sprite_Type) return Boolean is
      begin
         --  All the processed sprites are in the line, so no need to check
         --  for Y coordinates again
         return Sprite.X in 1 .. Display_Width + Sprite_Width - 1;
      end Is_Visible;

      Processed : Sprite_Priority_Buffer;
   begin
      Processed.N_Sprites := 0;
      Buffer.N_Sprites := 0;

      for Sprite_Index in Sprites'Range loop
         if Is_Enabled (Sprites (Sprite_Index)) then
            Insert_By_Processing_Priority (Processed, Sprite_Index);
         end if;
         if Processed.N_Sprites = Max_Line_Sprites then exit; end if;
      end loop;

      for Sprite_Index of Processed.Indexes (1 .. Processed.N_Sprites) loop
         if Is_Visible (Sprites (Sprite_Index)) then
            Insert_By_Draw_Priority (Buffer, Sprite_Index, Sprites);
         end if;
      end loop;
   end Prioritize_Sprites;

   procedure Populate_Line_Cache
     (VRAM  : Gade.Dev.VRAM.VRAM_Type;
      OAM   : Gade.Dev.OAM.OAM_Type;
      Cache : out Sprite_Line_Cache;
      Row   : Display_Vertical_Range;
      Size  : Sprite_Size_Type)
   is
      Priority_Buffer : Sprite_Priority_Buffer;
   begin
      --  Reset cache
      for Element of Cache loop
         Element.Value := Sprite_Transparent_Color;
      end loop;

      --  Prepare priority buffer
      Prioritize_Sprites (Priority_Buffer, OAM.Map.Sprites, Row, Size);

      --  Loop in reverse, draw the most prioritized sprite the last so it's
      --  drawn the latest, on top
      for Sprite_Index of reverse
        Priority_Buffer.Indexes (1 .. Priority_Buffer.N_Sprites)
      loop
         Populate_Sprite_Line
           (VRAM, OAM.Map.Sprites (Sprite_Index), Cache, Row, Size);
      end loop;
   end Populate_Line_Cache;

   procedure Populate_Sprite_Line
     (VRAM   : Gade.Dev.VRAM.VRAM_Type;
      Sprite : Sprite_Type;
      Cache  : in out Sprite_Line_Cache;
      Row    : Display_Vertical_Range;
      Size   : Sprite_Size_Type)
   is
      --  Bottom right corner of the sprite (+1)
      Sprite_X : constant Natural := Natural (Sprite.X);
      Sprite_Y : constant Natural := Natural (Sprite.Y);
      --  Mirroring axes
      X_Flip   : constant Boolean := Sprite.X_Flip;
      Y_Flip   : constant Boolean := Sprite.Y_Flip;
      --  Row within the sprite (0 .. 15)
      Sprite_Row : constant Integer := Row - (Sprite_Y - Double_Sprite_Height);
      --  Column within the sprite (0 .. 7)
      Sprite_Col : Natural;
      --  Row/Column within the sprite's tile
      Tile_Row : constant Natural := Y_Flip_Lookup (Size, Y_Flip, Sprite_Row);
      Tile_Col : Natural;

      Tile_Index : constant Tile_Index_Type :=
        Tile_Index_Type (Sprite.Pattern) +
        Sprite_Index_Add_Lookup (Size, Y_Flip, Sprite_Row);
      --  Extremes to be drawn within the line for the sprite
      Left  : constant Display_Horizontal_Range :=
        Sprite_Horizontal_Range'Max
          (Display_Horizontal_Range'First,
           Sprite_X - Sprite_Width);
      Right : constant Display_Horizontal_Range :=
        Sprite_Horizontal_Range'Min
          (Display_Horizontal_Range'Last,
           Sprite_X - 1);
      --  Actual width drawn of the sprite
      Effective_Width : constant Positive := Right - Left + 1;

      Color : Color_Value;
   begin
      Sprite_Col := Sprite_X - Left - Effective_Width;
      for Col in Left .. Right loop
         Tile_Col := X_Flip_Lookup (X_Flip, Sprite_Col);
         Color := Read_Raster_Tile
           (VRAM.Tile_Buffer, Tile_Index, Tile_Row, Tile_Col);

         if Color /= Sprite_Transparent_Color then
            Cache (Col) := (Color, Sprite.Priority, Sprite.Palette);
         end if;
         Sprite_Col := Sprite_Col + 1;
      end loop;
   end Populate_Sprite_Line;

end Gade.Dev.Video.Sprites;
