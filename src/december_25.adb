with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with NT_Console;
with DJH.Execution_Time;

procedure December_25 is

   Empty : constant Character := '.';
   East : constant Character := '>';
   South : constant Character := 'v';

   subtype Coordinates is Natural;
   subtype Bottom_Elements is Character with
     Static_Predicate => Bottom_Elements in Empty | East | South;

   type Points is record
      X, Y : Coordinates;
   end record; -- Points

   function "<" (Left, Right : Points) return Boolean is

   begin -- "<"
      return Left.Y < Right.Y or (Left.Y = Right.Y and Left.X < Right.X);
   end "<";

   function "=" (Left, Right : Points) return Boolean is

   begin -- "="
      return Left.X = Right.X and Left.Y = Right.Y;
   end "=";

   package Bottom_Stores is new
     Ada.Containers.Ordered_Maps (Points, Bottom_Elements);
   use Bottom_Stores;

   procedure Get_Input (Bottom_Store : out Bottom_Stores.Map;
                        X_Limit, Y_Limit : out Coordinates) is

      -- december_25 [Input_File_Name]

      Input_File : File_Type;
      Text : Unbounded_String;
      Point : Points;

   begin -- Get_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_25.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Point.Y := 0;
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         for I in Positive range 1 .. Length (Text) loop
            Point.X := I - 1;
            Include (Bottom_Store, Point, Element (Text,I));
         end loop; -- I in Positive range 1 .. Length (Text)
         if Point.Y = 0 then
            X_Limit := Point.X; -- all lines assumed to be same length as first
         end if; -- Point.X > X_Limit)
         Y_limit := Point.Y;
         Point.Y := Point.Y + 1;
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Get_Input;

   procedure Update (Bottom : in Bottom_Stores.Map;
                     X_Limit, Y_Limit : in Coordinates;
                     Next_Bottom : out Bottom_Stores.Map) is

      East_Bottom : Bottom_Stores.Map := Bottom_Stores.Empty_Map;

   begin -- Update
      -- Update East Herd;
      for Y in Coordinates range 0 .. Y_Limit loop
         for X in Coordinates range 0 .. X_Limit loop
            if Bottom ((X, Y)) = Empty or Bottom ((X, Y)) = South then
               Include (East_Bottom, (X, Y), Bottom ((X, Y)));
            end if; -- Bottom ((X, Y)) = Empty or Bottom ((X, Y)) = South
         end loop; -- X in Coordinates range 0 .. X_Limit
      end loop; -- Y in Coordinates range 0 .. Y_Limit
      for Y in Coordinates range 0 .. Y_Limit loop
         for X in Coordinates range 0 .. X_Limit loop
            if Bottom ((X, Y)) = East then
               if X + 1 <= X_Limit then
                  if Bottom ((X + 1, Y)) = Empty then
                     Include (East_Bottom, (X, Y), Empty);
                     Include (East_Bottom, (X + 1, Y), East);
                  else
                     include (East_Bottom, (X, Y), East);
                  end if; -- Bottom ((X + 1, Y)) = Empty
               else
                  -- wrap around
                  if Bottom ((0, Y)) = Empty then
                     Include (East_Bottom, (X, Y), Empty);
                     Include (East_Bottom, (0, Y), East);
                  else
                     include (East_Bottom, (X, Y), East);
                  end if; -- Bottom ((0, Y)) = Empty
               end if; -- X + 1 <= X_Limit
            end if; --if Bottom ((X, Y)) = East
         end loop; -- X in Coordinates range 0 .. X_Limit
      end loop; -- Y in Coordinates range 0 .. Y_Limit
      -- update South Herd
      for Y in Coordinates range 0 .. Y_Limit loop
         for X in Coordinates range 0 .. X_Limit loop
            if East_Bottom ((X, Y)) = Empty or East_Bottom ((X, Y)) = East then
               Include (Next_Bottom, (X, Y), East_Bottom ((X, Y)));
            end if; -- East_Bottom ((X, Y)) = Empty or East_Bottom ((X, Y)) ...
         end loop; -- X in Coordinates range 0 .. X_Limit
      end loop; -- Y in Coordinates range 0 .. Y_Limit
      for Y in Coordinates range 0 .. Y_Limit loop
         for X in Coordinates range 0 .. X_Limit loop
            if East_Bottom ((X, Y)) = South then
               if Y + 1 <= Y_Limit then
                  if East_Bottom ((X, Y + 1)) = Empty then
                     Include (Next_Bottom, (X, Y), Empty);
                     Include (Next_Bottom, (X, Y + 1), South);
                  else
                     include (Next_Bottom, (X, Y), South);
                  end if; -- if East_Bottom ((X, Y + 1)) = Empty
               else
                  -- wrap around
                  if East_Bottom ((X, 0)) = Empty then
                     Include (Next_Bottom, (X, Y), Empty);
                     Include (Next_Bottom, (X, 0), South);
                  else
                     include (Next_Bottom, (X, Y), South);
                  end if; -- East_Bottom ((X, 0)) = Empty
               end if; -- Y + 1 <= Y_Limit
            end if; -- East_Bottom ((X, Y)) = South
         end loop; -- X in Coordinates range 0 .. X_Limit
      end loop; -- Y in Coordinates range 0 .. Y_Limit
   end Update;

   procedure Run_Animate (Bottom_Store : in out Bottom_Stores.Map;
                          X_Limit, Y_Limit : in Coordinates;
                          Next_Store : out Bottom_Stores.Map) is

      package Screen is new NT_Console (X_Limit, Y_Limit + 2);
      use Screen;

      Steps : Positive := 1;

   begin -- Run_Animate
      Clear_Screen;
      Set_Cursor (False);
      loop -- update until no more moves
         Update (Bottom_Store, X_Limit, Y_Limit, Next_Store);
         exit when Next_Store = Bottom_Store;
         for Y in Coordinates range 0 .. Y_Limit loop
            Goto_XY (X_Pos'First, Y);
            for X in Coordinates range 0 .. X_Limit loop
               Put (Next_Store ((X, Y)));
            end loop; -- X in Coordinates range 0 .. X_Limit
         end loop; -- Y in Coordinates range 0 .. Y_Limit
         delay 0.25;
         Steps := Steps + 1;
         Bottom_Store := Next_Store;
      end loop; -- update until no more moves
      Goto_XY (X_Pos'First, Y_Limit + 1);
      Put ("Part One Answer:" & Steps'Img);
      Goto_XY (X_Pos'First, Y_Limit + 2);
      DJH.Execution_Time.Put_CPU_Time;
      Set_Cursor (True);
   end Run_Animate;

   Bottom_Store, Next_Store : Bottom_Stores.Map;
   X_Limit, Y_Limit : Coordinates;
   Steps : Positive := 1;
   Ch : character;
   Animate : Boolean;

begin -- December_25
   Get_Input (Bottom_Store, X_Limit, Y_Limit);
   Put ("Run Animation y | n : ");
   Get (Ch);
   Animate := Ch = 'Y' or Ch = 'y';
   if Animate then
      Run_Animate (Bottom_Store, X_Limit, Y_Limit, Next_Store);
   else
      loop -- update until no more moves
         Update (Bottom_Store, X_Limit, Y_Limit, Next_Store);
         exit when Next_Store = Bottom_Store;
         Steps := Steps + 1;
         Bottom_Store := Next_Store;
      end loop; -- update until no more moves
      Put_Line ("Answer:" & Steps'Img);
      DJH.Execution_Time.Put_CPU_Time;
   end if; -- Animate
end December_25;
