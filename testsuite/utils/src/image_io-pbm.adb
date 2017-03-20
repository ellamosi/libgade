with Ada.Streams.Stream_IO;
with Ada.Characters.Handling;
with IO_Exceptions;

package body Image_IO.PBM is

  type Unsigned_Byte is mod 2**8;
  for Unsigned_Byte'Size use 8;

  -- El fitxer PBM conté una imatge amb valors codificats en ASCII
  ASCII_PPM_Magic_String    : constant String := "P3";
  -- El fitxer PBM conté una imatge amb valors codificats en binari
  Raw_Bits_PPM_Magic_String : constant String := "P6";

  Comment_Character : constant Character := '#';

  function Is_Whitespace (Char : in Character ) return Boolean is
  begin
    return Char = ASCII.HT or Char = ASCII.LF or Char = ASCII.CR or Char = ' ';
  end Is_Whitespace;

  -- Salta espais i comentaris del fitxer d'entrada
  procedure Skip_Whitespace_And_Comments
    (File : in out Ada.Streams.Stream_IO.File_Type)
  is
    Char : Character;

    type Processing_State_Type is (Looking_For_Non_Whitespace,
                                   Looking_For_End_Of_Line);

    Processing_State : Processing_State_Type := Looking_For_Non_Whitespace;
    Current_Index : Ada.Streams.Stream_IO.Positive_Count;

    use type Ada.Streams.Stream_IO.Count;
  begin
    -- Sortim al primer caràcter que no sigui espai ni formi part d'un comentari
    loop
      Character'Read(Ada.Streams.Stream_IO.Stream(File), Char);
      case Processing_State is
        when Looking_For_Non_Whitespace =>
          if Char = Comment_Character then
            Processing_State := Looking_For_End_Of_Line;
          elsif not Is_Whitespace(Char) then
            exit;
          end if;
        when Looking_For_End_Of_Line =>
          if Char = ASCII.LF or Char = ASCII.CR then
            Processing_State := Looking_For_Non_Whitespace;
          end if;
      end case;
    end loop;

    -- Reculem
    Current_Index := Ada.Streams.Stream_IO.Index(File);
    Current_Index := Current_Index - 1;
    Ada.Streams.Stream_IO.Set_Index(
      File => File,
      To   => Current_Index);
  end Skip_Whitespace_And_Comments;

  -- Llegeix enters codificats en ASCII
  generic
    type Integer_Type is  (<>);
  procedure Read_ASCII_Integer_From_Stream
    (File : in out Ada.Streams.Stream_IO.File_Type;
     Item :    out Integer_Type);

  procedure Read_ASCII_Integer_From_Stream
    (File : in out Ada.Streams.Stream_IO.File_Type;
     Item :    out Integer_Type)
  is
    Current_Index          : Ada.Streams.Stream_IO.Positive_Count;
    First_Index_Of_Integer : Ada.Streams.Stream_IO.Positive_Count;
    Last_Index_Of_Integer  : Ada.Streams.Stream_IO.Positive_Count;
    Char                   : Character;

    use type Ada.Streams.Stream_IO.Count;
  begin
    Current_Index := Ada.Streams.Stream_IO.Index(File);

    loop
      Character'Read(Ada.Streams.Stream_IO.Stream(File), Char);
      exit when not Is_Whitespace(Char);
    end loop;

    if not Ada.Characters.Handling.Is_Digit(Char)
      and Char /= '-' and Char /= '+' then
      -- No és dígit

      Ada.Streams.Stream_IO.Set_Index
        (File => File,
         To   => Current_Index);

      raise IO_Exceptions.Data_Error;
    else
      First_Index_Of_Integer := Ada.Streams.Stream_IO.Index(File) - 1;

      loop
        Character'Read(Ada.Streams.Stream_IO.Stream(File), Char);
        exit when not Ada.Characters.Handling.Is_Digit(Char) or
          Ada.Streams.Stream_IO.End_Of_FIle(File);
      end loop;

      Last_Index_Of_Integer := Ada.Streams.Stream_IO.Index(File) - 1;

      declare
        Integer_As_String : String (1 .. Integer (Last_Index_Of_Integer -
          First_Index_Of_Integer));
      begin
        Ada.Streams.Stream_IO.Set_Index(
          File => File,
          To   => First_Index_Of_Integer);
        String'Read(Ada.Streams.Stream_IO.Stream(File), Integer_As_String);
        Item := Integer_Type'Value(Integer_As_String);
      end;
    end if;
  end Read_ASCII_Integer_From_Stream;

  procedure Read_ASCII_Dimension_From_Stream is
  new Read_ASCII_Integer_From_Stream(Dimension);

  procedure Read_ASCII_Pixel_Value_From_Stream is
  new Read_ASCII_Integer_From_Stream(Pixel_Value_Type);

  -- Consumeix un caràcter
  procedure Read_Single_Whitespace
    (File : in out Ada.Streams.Stream_IO.File_Type )
  is
    Char : Character;
  begin
    Character'Read(Ada.Streams.Stream_IO.Stream(File), Char);
  end Read_Single_Whitespace;

  -- Obre un fitxer PBM i en llegeix la capçalera
  procedure Open_File_And_Read_Header
    (Name            : in     String;
     Columns         :    out Dimension;
     Rows            :    out Dimension;
     Max_Color_Value :    out Pixel_Value_Type;
     Pixel_Data_Is   :    out Pixel_Value_Storage_Type;
     File            : in out Ada.Streams.Stream_IO.File_Type)
  is
    Magic_String : String (1 .. 2);
  begin
    Ada.Streams.Stream_IO.Open
      (File => File,
       Mode => Ada.Streams.Stream_IO.In_File,
       Name => Name);

    String'Read(Ada.Streams.Stream_IO.Stream(File), Magic_String);

    if Magic_String = ASCII_PPM_Magic_String then
      Pixel_Data_Is := ASCII_Data;
    elsif Magic_String = Raw_Bits_PPM_Magic_String then
      Pixel_Data_Is := Raw_Bits;
    else
      Ada.Streams.Stream_IO.Close(File);
      raise Not_A_PBM_File_Error;
    end if;

    Skip_Whitespace_And_Comments(File);

    Read_ASCII_Dimension_From_Stream
      (File => File,
       Item => Columns);

    Skip_Whitespace_And_Comments(File);

    Read_ASCII_Dimension_From_Stream
      (File => File,
       Item => Rows);

    Skip_Whitespace_And_Comments(File);

    Read_ASCII_Pixel_Value_From_Stream
      (File => File,
       Item => Max_Color_Value);

    Read_Single_Whitespace(File => File);
  end Open_File_And_Read_Header;

  -- Obre un fitxer PBM i en llegeix els continguts
  procedure Read_PBM
    (Name  : in     String;
     Image : in out Image_Type)
  is
    File            : Ada.Streams.Stream_IO.File_Type;
    Columns         : Dimension;
    Rows            : Dimension;
    Pixel_Data_Is   : Pixel_Value_Storage_Type;
    Pixel_Value     : Color_Channels_Type;
    Max_Color_Value : Pixel_Value_Type;
    Temp_Channel    : Unsigned_Byte;
  begin
    Open_File_And_Read_Header
      (Name            => Name,
       Columns         => Columns,
       Rows            => Rows,
       Max_Color_Value => Max_Color_Value,
       Pixel_Data_Is   => Pixel_Data_Is,
       File            => File);

    Set_Size(
      Image     => Image,
      Columns   => Columns,
      Rows      => Rows,
      Max_Color => Max_Color_Value);

    if Pixel_Data_Is = ASCII_Data then

      -- Per cada fila
      for Row in 1..Coordinate(Rows) loop
        -- Per cada columna
        for Column in 1..Coordinate(Columns) loop
          Read_ASCII_Pixel_Value_From_Stream
            (File => File,
             Item => Pixel_Value.Red);
          Read_ASCII_Pixel_Value_From_Stream
            (File => File,
             Item => Pixel_Value.Green);
          Read_ASCII_Pixel_Value_From_Stream
            (File => File,
             Item => Pixel_Value.Blue);
          Set_Pixel
            (Image  => Image,
             Column => Column,
             Row    => Row,
             Value  => Pixel_Value);
        end loop;
      end loop;

    else

      -- Per cada fila
      for Row in 1..Coordinate(Rows) loop
        -- Per cada columna
        for Column in 1..Coordinate(Columns) loop
          Unsigned_Byte'Read
            (Ada.Streams.Stream_IO.Stream(File),
             Temp_Channel);
          Pixel_Value.Red := Pixel_Value_Type(Temp_Channel);
          Unsigned_Byte'Read
            (Ada.Streams.Stream_IO.Stream(File),
             Temp_Channel);
          Pixel_Value.Green := Pixel_Value_Type(Temp_Channel);
          Unsigned_Byte'Read
            (Ada.Streams.Stream_IO.Stream(File),
             Temp_Channel);
          Pixel_Value.Blue := Pixel_Value_Type(Temp_Channel);
          Set_Pixel
            (Image  => Image,
             Column => Column,
             Row    => Row,
             Value  => Pixel_Value);
        end loop;
      end loop;

    end if;
    Ada.Streams.Stream_IO.Close(File);
  exception
    when others =>
      if Ada.Streams.Stream_IO.Is_Open(File) then
        Ada.Streams.Stream_IO.Close(File);
      end if;

      raise Not_A_PBM_File_Error;
  end Read_PBM;

end Image_IO.PBM;
