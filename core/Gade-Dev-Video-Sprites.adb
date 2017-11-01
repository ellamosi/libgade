with Gade.Dev.VRAM; use Gade.Dev.VRAM;
with Ada.Containers.Generic_Constrained_Array_Sort;

package body Gade.Dev.Video.Sprites is

   function Read
     (VRAM      : Gade.Dev.VRAM.VRAM_Type;
      OAM       : Gade.Dev.OAM.OAM_Type;
      Row, Col  : Integer;
      Size      : Sprite_Size_Type) return Sprite_Result_Type
   is
      PX_Row, PX_Col : Integer;
      X_Flip, Y_Flip : Boolean;
      Y_Limit    : Natural;
      Tile_Index : Tile_Index_Type;
      Result     : Sprite_Result_Type;
      Min_Vis_X  : Natural; -- Overlapping leftmost sprite has priority
      Sprite_X, Sprite_Y : Natural;
      PX_Val     : Color_Value;
      Index_Add : Tile_Index_Type;
   begin
      Min_Vis_X := Natural'Last;
      Y_Limit := Sprite_Height (Size);
      Result.Value := Sprite_Transparent_Color;

      --  Iterate in reverse because the lowest sprite has priority if
      --  overlapping with another sprite in the same X position
      for Current_Sprite in reverse OAM.Map.Sprites'Range loop
         Sprite_X := Natural (OAM.Map.Sprites (Current_Sprite).X);
         Sprite_Y := Natural (OAM.Map.Sprites (Current_Sprite).Y);
         PX_Col := Col - Sprite_X + 8;
         PX_Row := Row - Sprite_Y + 16;

         --  TODO: Revise this, as some sprites need to be taken into account for
         --  the line sprite limits
         if PX_Row >= 0 and PX_Row < Y_Limit and PX_Col >= 0 and PX_Col < 8 then
            --  Sprite is in pixel

            X_Flip := OAM.Map.Sprites (Current_Sprite).X_Flip;
            Y_Flip := OAM.Map.Sprites (Current_Sprite).Y_Flip;

            --  Size = Double and ((PX_Row >= 8 and not Y_Flip) or (PX_Row <= 7 and Y_Flip)
            Index_Add := Sprite_Index_Add_Lookup (Size, Y_Flip, PX_Row);

            PX_Col := X_Flip_Lookup (X_Flip, PX_Col);
            PX_Row := Y_Flip_Lookup (Size, Y_Flip, PX_Row);

            Tile_Index := Tile_Index_Type (OAM.Map.Sprites (Current_Sprite).Pattern) + Index_Add;
            PX_Val := Read_Raster_Tile (VRAM.Tile_Buffer, Tile_Index, PX_Row, PX_Col);

            if PX_Val /= 0 and Min_Vis_X >= Sprite_X then
               Result.Value := PX_Val;
               --  TODO: Needs a better name or enum!
               Result.Priority := OAM.Map.Sprites (Current_Sprite).Priority;
               Result.Palette := OAM.Map.Sprites (Current_Sprite).Palette;
            end if;
         end if;
      end loop;

      return Result;
   end Read;

   function Read
     (Cache : Sprite_Line_Cache;
      Col   : Display_Horizontal_Range) return Sprite_Result_Type
   is
      Result     : Sprite_Result_Type;
   begin
      Result.Value    := Cache (Col).Color;
      Result.Priority := Cache (Col).Priority;
      Result.Palette  := Cache (Col).Palette;
      return Result;
   end Read;

   type Sprite_Priority is record
      X_Pos        : Natural;
      Sprite_Index : Sprite_Index_Type;
   end record;

   function "<" (L, R : Sprite_Priority) return Boolean;
   function "<" (L, R : Sprite_Priority) return Boolean is
   begin
      --  DMG Mode, TODO: For GBC it only should rely on the sprite index
      return L.X_Pos < R.X_Pos or (L.X_Pos = R.X_Pos and L.Sprite_Index < R.Sprite_Index);
   end "<";

   type Sprite_Priorities is array (Sprite_Array_Type'Range) of Sprite_Priority;

   procedure Sort is new Ada.Containers.Generic_Constrained_Array_Sort
     (Index_Type   => Sprite_Index_Type,
      Element_Type => Sprite_Priority,
      Array_Type   => Sprite_Priorities);



   procedure Populate_Line_Cache
     (VRAM  : Gade.Dev.VRAM.VRAM_Type;
      OAM   : Gade.Dev.OAM.OAM_Type;
      Cache : out Sprite_Line_Cache;
      Row   : Display_Vertical_Range;
      Size  : Sprite_Size_Type)
   is
      Max_Line_Sprites : constant := 10;
      Sprite_X, Sprite_Y : Natural; -- Bottom right corner of the sprite (+1)
      Sprite_Row : Integer;
      X_Flip, Y_Flip : Boolean;
      Col : Integer;
      Tile_Index : Tile_Index_Type;
      Tile_Row, Tile_Col : Natural;
      Color : Color_Value;

      Current_Sprite : Sprite_Index_Type;
      Y_Limit : constant Positive := Sprite_Height (Size);
      Index_Add : Tile_Index_Type;
      Prios : Sprite_Priorities;
      Processed_Sprites : Natural;

      Prio_Index : Integer;
   begin
      --  Reset cache
      for Element of Cache loop
         Element.Color := Sprite_Transparent_Color;
      end loop;

      for Sprite_Index in OAM.Map.Sprites'Range loop
         Prios (Sprite_Index).X_Pos := Natural (OAM.Map.Sprites (Sprite_Index).X);
         Prios (Sprite_Index).Sprite_Index := Sprite_Index;
      end loop;
      Sort (Prios);

      Processed_Sprites := 0;
      Prio_Index := Integer (Prios'Last);
      while Processed_Sprites < Max_Line_Sprites and Prio_Index >= Integer (Sprite_Index_Type'First) loop
         Current_Sprite := Prios (Sprite_Index_Type (Prio_Index)).Sprite_Index;

         Sprite_X := Natural (OAM.Map.Sprites (Current_Sprite).X);
         Sprite_Y := Natural (OAM.Map.Sprites (Current_Sprite).Y);
         Sprite_Row := Row - (Sprite_Y - 16);

         if Sprite_Row in 0 .. Y_Limit - 1 then
            --  Sprite is processed

            if Sprite_X in 1 .. 160 + 8 - 1 then
               --  Sprite is visible

               X_Flip := OAM.Map.Sprites (Current_Sprite).X_Flip;
               Y_Flip := OAM.Map.Sprites (Current_Sprite).Y_Flip;
               Index_Add := Sprite_Index_Add_Lookup (Size, Y_Flip, Sprite_Row);
               Tile_Row := Y_Flip_Lookup (Size, Y_Flip, Sprite_Row);
               Tile_Index := Tile_Index_Type (OAM.Map.Sprites (Current_Sprite).Pattern) + Index_Add;

               for Sprite_Col in 0 .. 7 loop
                  Col := Sprite_X - 8 + Sprite_Col;

                  if Col in Display_Horizontal_Range then
                     Tile_Col := X_Flip_Lookup (X_Flip, Col - Sprite_X + 8);
                     Color := Read_Raster_Tile (VRAM.Tile_Buffer, Tile_Index, Tile_Row, Tile_Col);

                     if Color /= Sprite_Transparent_Color then
                        Cache (Col).Color := Color;
                        --  TODO: Needs a better name or enum!
                        Cache (Col).Priority := OAM.Map.Sprites (Current_Sprite).Priority;
                        Cache (Col).Palette := OAM.Map.Sprites (Current_Sprite).Palette;
                     end if;
                  end if;
               end loop;
            end if;
            Processed_Sprites := Processed_Sprites + 1;
         end if;
         Prio_Index := Prio_Index - 1;
      end loop;
   end Populate_Line_Cache;

end Gade.Dev.Video.Sprites;
