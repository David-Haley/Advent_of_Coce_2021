with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

procedure December_08 is

   subtype Segments is Character range 'a' .. 'g';

   type Displays is array (Segments) of Boolean;

   subtype Data_Indices is Natural range 0 .. 9;
   type Data_Arrays is array (Data_Indices) of Displays;
   subtype Display_Indices is Positive range 1 .. 4;
   type Display_Arrays is array (Display_Indices) of Displays;

   type Frames is record
      Data_Array : Data_Arrays;
      Display_Array : Display_Arrays;
   end record; -- Frames

   subtype Display_Values is Natural range 0 .. 9999;

   package Frame_Stores is new Ada.Containers.Vectors (Positive, Frames);
   use Frame_Stores;

   procedure Get_Input (Frame_Store : out Frame_Stores.Vector) is

      -- december_08 [Input_File_Name]

      function To_Display (S : in String) return Displays is

         Display : Displays := (others => False);

      begin -- To_Display
         for C in Positive range S'First .. S'Last loop
            Display (S (C)) := True;
         end loop; -- C in Positive range S'First .. S'Last
         return Display;
      end To_Display;

      Segment_Set : constant Character_Set := To_Set ( "abcdefg");
      Input_File : File_Type;
      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;
      Frame : Frames;

   begin -- Get_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_08.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Start_At := 1;
         for I in Data_Indices loop
            Find_Token (Text, Segment_Set, Start_At, Inside, First, Last);
            Frame.Data_Array (I) := To_Display (Slice (Text, First, Last));
            Start_At := Last + 1;
         end loop; -- I in Data_Indices
         for J in Display_Indices loop
            Find_Token (Text, Segment_Set, Start_At, Inside, First, Last);
            Frame.Display_Array (J) := To_Display (Slice (Text, First, Last));
            Start_At := Last + 1;
         end loop; -- J in Display_Indices
         Append (Frame_Store, Frame);
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Get_Input;

   function Count_1478 (Frame_Store : in Frame_Stores.Vector) return Natural is

      Count : Natural := 0;
      Segment_Count : Natural;

   begin -- Count_1478
      for F in Iterate (Frame_Store) loop
         for D in Display_Indices loop
            Segment_Count := 0;
            for S in Segments loop
               if Frame_Store (F).Display_Array (D) (S) then
                  Segment_Count := Segment_Count + 1;
               end if; -- Frame_Store (F).Display_Array (D) (S)
            end loop; -- S in Segments
            if Segment_Count = 2 or Segment_Count = 4 or Segment_Count = 3 or
              Segment_Count = 7 then
               Count := Count + 1;
            end if; -- Segment_Count = 2 or Segment_Count = 4 or ...
         end loop; -- D in Display_Indices
      end loop; -- F in Iterate (Frame_Store)
      return Count;
   end Count_1478;

   function Unscramble (Frame : in Frames) return Display_Values is

      function "and" (Left, Right : in Displays) return Displays is

         Result : Displays;

      begin -- "and"
         for S in Segments loop
            Result (S) := Left (S) and Right (S);
         end loop; -- S in Segments
         return Result;
      end "and";

      function "or" (Left, Right : in Displays) return Displays is

         Result : Displays;

      begin -- "or"
         for S in Segments loop
            Result (S) := Left (S) or Right (S);
         end loop; -- S in Segments
         return Result;
      end "or";

      function "not" (Right : in Displays) return Displays is

         Result : Displays;

      begin -- "not"
         for S in Segments loop
            Result (S) := not Right (S);
         end loop; -- S in Segments
         return Result;
      end "not";

      Segment_Count : Natural;
      Pattern : Data_Arrays;
      Data_Segment_Count:
        array (Segments) of Data_Indices := (others => 0);
      a, b, c, d, e, f, g, ag : Displays;
      Result : Display_Values := 0;

   begin -- Unscramble
      for D in Data_Indices loop
         Segment_Count := 0;
         for S in Segments loop
            if Frame.Data_Array (D) (S) then
               Segment_Count := Segment_Count + 1;
               Data_Segment_Count (S) := Data_Segment_Count (S) + 1;
            end if; -- Frame_Store (F).Display_Array (D) (S)
         end loop; -- S in Segments
         if Segment_Count = 2 then
            Pattern (1) := Frame.Data_Array (D);
         elsif Segment_Count = 4 then
            Pattern (4) := Frame.Data_Array (D);
         elsif Segment_Count = 3 then
            Pattern (7) := Frame.Data_Array (D);
         elsif Segment_Count = 7 then
            Pattern (8) := Frame.Data_Array (D);
         end if; -- Segment_Count = 2
      end loop; -- D in Display_Indices
      ag := Pattern (8);
      for D in Data_Indices loop
         for S in Segments loop
            if Data_Segment_Count (S) = 9 and not Frame.Data_Array (D) (S) then
               Pattern (2) := Frame.Data_Array (D);
            end if; -- Data_Segment_Count (S) = 9 and not Frame.Data_Array ...
         end loop; -- S in Segments
         if Frame.Data_Array (D) /= Pattern (1) and
           Frame.Data_Array (D) /= Pattern (4) and
           Frame.Data_Array (D) /= Pattern (7) and
           Frame.Data_Array (D) /= Pattern (8) then
            ag := ag and Frame.Data_Array (D);
         end if; -- Frame_Store (F).Display_Array (D) ..
      end loop; -- D in Display_Indices
      a := Pattern (7) and not Pattern (1);
      g := ag and not a;
      e := Pattern (8) and not (Pattern (4) or Pattern (7) or g);
      f := not Pattern (2) and Pattern (1);
      c := Pattern (1) and not f;
      Pattern (6) := (Pattern (8) and not Pattern (7)) or a or f;
      Pattern (5) := Pattern (6) and not e;
      Pattern (9) := Pattern (5) or c;
      b := Pattern (4) and not Pattern (1) and not Pattern (2);
      d := Pattern (4) and not Pattern (1) and not b;
      Pattern (0) := Pattern (8) and not d;
      -- Solution would work if Pattern (0) was not resolved because x + 0 = x
      Pattern (3) := a or c or d or f or g;
      for D in Display_Indices loop
         Result := Result * 10; -- Note 0 * 10 = 0
         for I in Data_Indices loop
            if Frame.Display_Array (D) = Pattern (I) then
               Result := Result + I;
            end if; -- Frame.Display_Array (D) = Pattern (I)
         end loop; -- I in Single_Digits
      end loop; -- D in Display_Indices
      return Result;
   end Unscramble;

   Frame_Store : Frame_Stores.Vector;
   Total : Natural := 0;

begin -- December_08
   Get_Input (Frame_Store);
   Put_Line ("Part One Answer:" & Count_1478 (Frame_Store)'Img);
   for F in Iterate (Frame_Store) loop
      Total := Total + Unscramble (Frame_Store (F));
   end loop; -- F in Iterate (Frame_Store)
   Put_Line ("Part Two Answer:" & Total'img);
end December_08;
