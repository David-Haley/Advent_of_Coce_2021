with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with NT_Console;

procedure December_09 is

   subtype Coordinates is Natural;
   subtype Extended_Coordinated is Integer range -1 .. Coordinates'Last;
   subtype Heights is Natural range 0 .. 9;

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

   package Point_Stores is new
     Ada.Containers.Ordered_Maps (Points, Heights);
   use Point_Stores;

   package Basin_Sets is new Ada.Containers.Ordered_Sets (Points);
   use Basin_Sets;

   subtype Basin_Areas is Natural;

   package Basin_Lists is new Ada.Containers.Vectors (Positive, Basin_Areas);
   use Basin_Lists;

   package Screen is new NT_Console (101, 101);
   use Screen;

   procedure Get_Input (Point_Store : out Point_Stores.Map;
                       Basin_Set : out Basin_Sets.Set) is

      -- december_09 [Input_File_Name]

      Input_File : File_Type;
      Text : Unbounded_String;
      Point : Points;

   begin -- Get_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_09.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Point.Y := 0;
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         for I in Positive range 1 .. Length (Text) loop
            Point.X := I - 1;
            Include (Point_Store, Point, Heights'Value (Slice (Text, I, I)));
            if Heights'Value (Slice (Text, I, I)) < Heights'Last then
               Include (Basin_Set, Point);
            end if; -- Heights'Value (Slice (Text, I, I)) < Heights'Last
         end loop;
         Point.Y := Point.Y + 1;
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Get_Input;

   function Total_Risk (Point_Store : in Point_Stores.Map) return Natural is

      Current_Point, Test_Point : Points;
      Is_Lower : Boolean;
      Current_Height : Heights;
      Result : Natural := 0;

   begin -- Total_Risk
      for I in Iterate (Point_Store) loop
         Current_Point := Key (I);
         Current_Height := Point_Store (Current_Point);
         Is_Lower := True;
         -- (X, Y - 1)
         if Current_Point.Y > 0 then
            Test_Point.X := Current_Point.X;
            Test_Point.Y := Current_Point.Y - 1;
            Is_Lower := Is_Lower and Current_Height < Point_Store (Test_Point);
         end if; -- Current_Point.Y > 0
         -- (X, Y + 1)
         Test_Point.X := Current_Point.X;
         Test_Point.Y := Current_Point.Y + 1;
         if Contains (Point_Store, Test_Point) then
            Is_Lower := Is_Lower and Current_Height < Point_Store (Test_Point);
         end if; -- Contains (Point_Store, Test_Point)
         -- (X + 1, Y)
         Test_Point.X := Current_Point.X + 1;
         Test_Point.Y := Current_Point.Y;
         if Contains (Point_Store, Test_Point) then
            Is_Lower := Is_Lower and Current_Height < Point_Store (Test_Point);
         end if; -- Contains (Point_Store, Test_Point)
         -- (X -1, Y)
         if Current_Point.X > 0 then
            Test_Point.X := Current_Point.X - 1;
            Test_Point.Y := Current_Point.Y;
            Is_Lower := Is_Lower and Current_Height < Point_Store (Test_Point);
         end if; -- Current_Point.X > 0
         if Is_Lower then
            Result := Result + Point_Store (Current_Point) + 1;
         end if; -- Is_Lower
      end loop; -- I in Iterate (Point_Store)
      return Result;
   end Total_Risk;

   procedure Find_Basins (Basin_Set : in out Basin_Sets.Set;
                          Basin_List : out Basin_Lists.Vector;
                          Point_Store : in Point_Stores.Map :=
                            Point_Stores.Empty_Map;
                          Animate : in Boolean := False) is

      package Point_Queue_Interfaces is new
        Ada.Containers.Synchronized_Queue_Interfaces (Points);

      package Point_Queues is new
        Ada.Containers.Unbounded_Synchronized_Queues (Point_Queue_Interfaces);

      package Basin_List_Sorting is new Basin_Lists.Generic_Sorting;

      subtype Differences is Extended_Coordinated range -1 .. 1;

      function Valid_Point (X, Y : in Extended_Coordinated;
                            Basin_Set : in Basin_Sets.Set) return Boolean is

      begin -- Valid_Point
         return (X >= Coordinates'First and Y >= Coordinates'First) and then
           Contains (Basin_Set, (X, Y));
      end Valid_Point;

      Procedure Animation (Point : in Points;
                           Point_Store : in Point_Stores.Map) is

      begin -- Animation
         Goto_XY (Point.X + 1, Point.Y + 1);
         Put (Trim (Heights'Image (Point_Store (Point)), Left));
         delay 0.02;
      end Animation;

      Point_Queue : Point_Queues.Queue;
      Point, Next_Point : Points;
      Area : Basin_Areas;
      Dx, Dy : Extended_Coordinated;

   begin -- Find_Basins
      while Length (Basin_Set) > 0 loop
         Area := 0;
         Next_Point := First_Element (Basin_Set);
         Point_Queue.Enqueue (Next_Point);
         while Point_Queue.Current_Use > 0 loop
            Point_Queue.Dequeue (Point);
            if Contains (Basin_Set, Point) then
               -- The contains test is required because a point may be enqueued
               -- more than once, it can only be deleted once and should be
               -- counted in the area once
               if Animate then
                  Animation (Point, Point_Store);
               end if; -- Animate
               Area := Area + 1;
               Delete (Basin_Set, Point);
               Dy := 0;
               Dx := -1;
               if Valid_Point (Point.X + Dx, Point.Y + Dy, Basin_Set) then
                  Next_Point := (Point.X + Dx, Point.Y + Dy);
                  Point_Queue.Enqueue (Next_Point);
               end if; -- Valid_Point (Point.X + Dx, Point.Y + Dy, Basin_Set)
               Dx := 1;
               if Valid_Point (Point.X + Dx, Point.Y + Dy, Basin_Set) then
                  Next_Point := (Point.X + Dx, Point.Y + Dy);
                  Point_Queue.Enqueue (Next_Point);
               end if; -- Valid_Point (Point.X + Dx, Point.Y + Dy, Basin_Set)
               Dx := 0;
               Dy := -1;
               if Valid_Point (Point.X + Dx, Point.Y + Dy, Basin_Set) then
                  Next_Point := (Point.X + Dx, Point.Y + Dy);
                  Point_Queue.Enqueue (Next_Point);
               end if; -- Valid_Point (Point.X + Dx, Point.Y + Dy, Basin_Set)
               Dy := 1;
               if Valid_Point (Point.X + Dx, Point.Y + Dy, Basin_Set) then
                  Next_Point := (Point.X + Dx, Point.Y + Dy);
                  Point_Queue.Enqueue (Next_Point);
               end if; -- Valid_Point (Point.X + Dx, Point.Y + Dy, Basin_Set)
            end if; -- Contains (Basin_Set, Point)
         end loop; -- Point_Queue.Current_Use > 0
         Append (Basin_List, Area);
      end loop; -- Length (Basin_Set) > 0
      Basin_List_Sorting.Sort (Basin_List);
   end Find_Basins;

   Point_Store : Point_Stores.Map;
   Basin_Set : Basin_Sets.Set;
   Basin_List : Basin_Lists.Vector;
   Last : Positive;
   Ch : character;
   Animate : Boolean;

begin -- December_09
   Get_Input (Point_Store, Basin_Set);
   Put_Line ("Part One Answer:" & Total_Risk (Point_Store)'Img);
   Put ("Run Part Two Animation y | n : ");
   Get (Ch);
   Animate := Ch = 'Y' or Ch = 'y';
   if Animate then
      Clear_Screen;
      Set_Cursor (False);
   end if; -- Animate
   Find_Basins (Basin_Set, Basin_List, Point_Store, Animate);
   Last := Last_Index (Basin_List);
   if Animate then
      Set_Cursor (True);
      Goto_XY (0, Y_Pos'Last);
   end if; -- Animate
   Put_Line ("Part Two Answer:" & Basin_Areas'Image (Basin_List (Last) *
               Basin_List (Last - 1) * Basin_List (Last - 2)));
   -- will raise an exception if there are not at least three basins
end December_09;
