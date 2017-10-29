with Gade.Dev.VRAM; use Gade.Dev.VRAM;

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

end Gade.Dev.Video.Sprites;
