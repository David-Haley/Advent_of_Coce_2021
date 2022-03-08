with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Interfaces; use Interfaces;
with DJH.Execution_Time; use DJH.Execution_Time;

procedure December_22 is

   subtype Coordinates is Integer;

   type Rectangular_Prisms is record
      X1, X2, Y1, Y2, Z1, Z2 : Coordinates;
   end record; -- Rectangular_Prisms

   type Boot_List_Elements is record
      Prism : Rectangular_Prisms;
      Command : Boolean; -- On = True
   end record; -- Boot_List_Elements

   package Boot_Lists is new
     Ada.Containers.Vectors (Positive, Boot_List_Elements);
   use Boot_Lists;

   subtype Cube_Coordinates is Coordinates range -50 .. 50;
   type Cube_Arrays is array
     (Cube_Coordinates, Cube_Coordinates, Cube_Coordinates) of Boolean;

   procedure Get_Input (Boot_List : out Boot_Lists.Vector) is

      Integer_Set : constant Character_Set := To_Set ("-0123456789");
      On_Off_Set : constant Character_Set := To_Set ("ofn");
      X_String : constant String := "x=";
      Y_String : constant String := ",y=";
      Z_String : constant String := ",z=";
      Range_String : constant String := "..";

      Input_File : File_Type;
      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;
      Boot_List_Element : Boot_List_Elements;

   begin -- Get_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_22.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Start_At := 1;
         Find_Token (Text, On_Off_Set, Start_At, Inside, First, Last);
         if Slice (Text, First, Last) = "on" then
            Boot_List_Element.Command := True;
         elsif Slice (Text, First, Last) = "off" then
            Boot_List_Element.Command := False;
         else
            Assert (False, Ada.Text_IO.Count'Image (Line (Input_File) - 1) &
                      ": Expected ""on"" or ""off"" found """ &
                      Slice (Text, First, Last) & '"');
         end if; -- Slice (Text, First, Last) = "on"
         Start_At := Last + 1;
         Find_Token (Text, Integer_Set, Start_At, Inside, First, Last);
         Assert (X_String = Slice (Text, First - X_String'Length , First - 1),
                 Ada.Text_IO.Count'Image (Line (Input_File) - 1) &
                   ": Expected """ & X_String & """ found """ &
                   Slice (Text, First - X_String'Length , First - 1) & '"');
         Boot_List_Element.Prism.X1 :=
           Coordinates'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text, Integer_Set, Start_At, Inside, First, Last);
         Assert (Range_String = Slice (Text, First - Range_String'Length ,
                 First - 1), Ada.Text_IO.Count'Image (Line (Input_File) - 1) &
                   ": Expected """ & Range_String & """ found """ &
                   Slice (Text, First - Range_String'Length , First - 1) & '"');
         Boot_List_Element.Prism.X2 :=
           Coordinates'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text, Integer_Set, Start_At, Inside, First, Last);
         Assert (Y_String = Slice (Text, First - Y_String'Length , First - 1),
                 Ada.Text_IO.Count'Image (Line (Input_File) - 1) &
                   ": Expected """ & Y_String & """ found """ &
                   Slice (Text, First - Y_String'Length , First - 1) & '"');
         Boot_List_Element.Prism.Y1 :=
           Coordinates'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text, Integer_Set, Start_At, Inside, First, Last);
         Assert (Range_String = Slice (Text, First - Range_String'Length ,
                 First - 1), Ada.Text_IO.Count'Image (Line (Input_File) - 1) &
                   ": Expected """ & Range_String & """ and found """ &
                   Slice (Text, First - Range_String'Length , First - 1) & '"');
         Boot_List_Element.Prism.Y2 :=
           Coordinates'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text, Integer_Set, Start_At, Inside, First, Last);
         Assert (Z_String = Slice (Text, First - Z_String'Length , First - 1),
                 Ada.Text_IO.Count'Image (Line (Input_File) - 1) &
                   ": Expected """ & Z_String & """ found """ &
                   Slice (Text, First - Z_String'Length , First - 1) & '"');
         Boot_List_Element.Prism.Z1 :=
           Coordinates'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text, Integer_Set, Start_At, Inside, First, Last);
         Assert (Range_String = Slice (Text, First - Range_String'Length ,
                 First - 1), Ada.Text_IO.Count'Image (Line (Input_File) - 1) &
                   ": Expected """ & Range_String & """ found """ &
                   Slice (Text, First - Range_String'Length , First - 1) & '"');
         Boot_List_Element.Prism.Z2 :=
           Coordinates'Value (Slice (Text, First, Last));
         Append (Boot_List, Boot_List_Element);
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Get_Input;

   function Has_Intersection (Left, Right : Rectangular_Prisms)
                              return Boolean is

   begin -- Has_Intersection
      return (((Left.X1 <= Right.X1 and Right.X1 <= Left.X2) or
                (Left.X1 <= Right.X2 and Right.X2 <= Left.X2)) and
                ((Left.Y1 <= Right.Y1 and Right.Y1 <= Left.Y2) or
                     (Left.Y1 <= Right.Y2 and Right.Y2 <= Left.Y2)) and
                  ((Left.Z1 <= Right.Z1 and Right.Z1 <= Left.Z2) or
                     (Left.Z1 <= Right.Z2 and Right.Z2 <= Left.Z2))) or
          (((Right.X1 <= Left.X1 and Left.X1 <= Right.X2) or
              (Right.X1 <= Left.X2 and Left.X2 <= Right.X2)) and
             ((Right.Y1 <= Left.Y1 and Left.Y1 <= Right.Y2) or
                  (Right.Y1 <= Left.Y2 and Left.Y2 <= Right.Y2)) and
               ((Right.Z1 <= Left.Z1 and Left.Z1 <= Right.Z2) or
                  (Right.Z1 <= Left.Z2 and Left.Z2 <= Right.Z2)));
   end Has_Intersection;

   procedure Update (Cube_Array : in out Cube_Arrays;
                     Boot_List_Element : in Boot_List_Elements) is

      Cube : Rectangular_Prisms :=
        (Cube_Coordinates'First, Cube_Coordinates'Last,
         Cube_Coordinates'First, Cube_Coordinates'Last,
         Cube_Coordinates'First, Cube_Coordinates'Last);

      Xi1, Xi2, Yi1, Yi2, Zi1, Zi2 : Cube_Coordinates;

   begin -- Update
      if Has_Intersection (Cube, Boot_List_Element.Prism) then
         if Boot_List_Element.Prism.X1 < Cube_Coordinates'First then
            Xi1 := Cube_Coordinates'First;
         else
            Xi1 := Boot_List_Element.Prism.X1;
         end if; -- Boot_List_Element.Prism.X1 < Cube_Coordinates'First
         if Boot_List_Element.Prism.X2 > Cube_Coordinates'Last then
            Xi2 := Cube_Coordinates'Last;
         else
            Xi2 := Boot_List_Element.Prism.X2;
         end if; -- Boot_List_Element.Prism.X2 < Cube_Coordinates'Last
         if Boot_List_Element.Prism.Y1 < Cube_Coordinates'First then
            Yi1 := Cube_Coordinates'First;
         else
            Yi1 := Boot_List_Element.Prism.Y1;
         end if; -- Boot_List_Element.Prism.Y1 < Cube_Coordinates'First
         if Boot_List_Element.Prism.Y2 > Cube_Coordinates'Last then
            Yi2 := Cube_Coordinates'Last;
         else
            Yi2 := Boot_List_Element.Prism.Y2;
         end if; -- Boot_List_Element.Prism.Y2 < Cube_Coordinates'Last
         if Boot_List_Element.Prism.Z1 < Cube_Coordinates'First then
            Zi1 := Cube_Coordinates'First;
         else
            Zi1 := Boot_List_Element.Prism.Z1;
         end if; -- Boot_List_Element.Prism.Z1 < Cube_Coordinates'First
         if Boot_List_Element.Prism.Z2 > Cube_Coordinates'Last then
            Zi2 := Cube_Coordinates'Last;
         else
            Zi2 := Boot_List_Element.Prism.Z2;
         end if; -- Boot_List_Element.Prism.Z2 < Cube_Coordinates'Last
         for X in Cube_Coordinates range Xi1 .. Xi2 loop
            for Y in Cube_Coordinates range Yi1 .. Yi2 loop
               for Z in Cube_Coordinates range Zi1 .. Zi2 loop
                  Cube_Array (X, Y, Z) := Boot_List_Element.Command;
               end loop; -- Z in Cube_Coordinates range Zi1 .. Zi2
            end loop; -- Y in Cube_Coordinates range Yi1 .. Yi2
         end loop; -- X in Cube_Coordinates range Xi1 .. Xi2
      end if; --Has_Intersection
   end Update;

   function Cube_Count (Cube_Array : in Cube_Arrays) return Natural is

      Result : Natural := 0;

   begin -- Cube_Count
      for X in Cube_Coordinates loop
         for Y in Cube_Coordinates loop
            for Z in Cube_Coordinates loop
               if Cube_Array (X, Y, Z) then
                  Result := Result + 1;
               end if; -- Cube_Array (X, Y, Z)
            end loop; -- Z in Cube_Coordinates
         end loop; -- Y in Cube_Coordinates
      end loop; -- X in Cube_Coordinates
      return Result;
   end Cube_Count;

   function Part_Two_Count (Boot_List : in Boot_Lists.Vector)
                            return Unsigned_64 is

      -- Problem is reduced to a one dimentional task which reduces to summing
      -- non overlaping line segments parallel to the X axis. By iterating over
      -- the Z and Y ranges all cubes are counted along a single line. For
      -- efficiency only Z and Y values near boundaries are evaluated as those
      -- between the boundaries are the same and can be summed without
      -- recalculation.

      type Rectangles is Record
         Command : Boolean;
         X1, Y1, X2, Y2 : Coordinates;
      end record; -- Rectangles

      package Rectangle_Lists is new
        Ada.Containers.Vectors (Positive, Rectangles);
      use Rectangle_Lists;

      package Coordinate_Lists is new
        Ada.Containers.Ordered_Maps (Coordinates, Coordinates);
      use Coordinate_Lists;

      package Coordinate_Sets is new Ada.Containers.Ordered_Sets (Coordinates);
      use Coordinate_Sets;

      procedure Build_Z_Set (Boot_List : in Boot_Lists.Vector;
                             Z_Set : out Coordinate_Sets.Set) is

         -- For any X,Y plane the cube count should be the the same as the
         -- previous or next unless there is a new rectangular prism in play.
         -- Thus a change in the cube count will only occur when the Z value
         -- being evaluated is equal to the Z1 or Z2 value of some prism.
         -- A check is provided by calculating for the Z value before the
         -- change for the start of a prism and the Z value after the change for
         -- the end of a prism. From examination of the input data this should
         -- reduce the number of Z iterations from the order of 200,000 to
         -- less than 2000, a factor of 100 increase in speed! Sets are used so
         -- that if two or more boundaries coincide the calculations are not
         -- repeated.

      begin -- Build_Z_Set
         Z_Set := Coordinate_Sets.Empty_Set;
         for B in Iterate (Boot_List) loop
            Include (Z_Set, Boot_List (B).Prism.Z1 - 1);
            Include (Z_Set, Boot_List (B).Prism.Z1);
            Include (Z_Set, Boot_List (B).Prism.Z2);
            Include (Z_Set, Boot_List (B).Prism.Z2 + 1);
         end loop; -- B in Iterate (Boot_List)
      end Build_Z_Set;

      procedure Build_Y_Set (Rectangle_List : in Rectangle_Lists.Vector;
                             Y_Set : out Coordinate_Sets.Set) is

         -- A similar argument can be applied to lines with varing Y. See
         -- comments in procedure Build_Z_Set above.

      begin -- Build_Y_Set
         Y_Set := Coordinate_Sets.Empty_Set;
         for R in Iterate (Rectangle_List) loop
            Include (Y_Set, Rectangle_List (R).Y1 - 1);
            Include (Y_Set, Rectangle_List (R).Y1);
            Include (Y_Set, Rectangle_List (R).Y2);
            Include (Y_Set, Rectangle_List (R).Y2 + 1);
         end loop; -- B in Iterate (Rectangle_List)
      end Build_Y_Set;

      Count :  Unsigned_64 := 0; -- Total for whole space
      Count_Y, Previous_Count_Y : Unsigned_64 := 0; -- Total for one line
      Count_Z, Previous_Count_Z : Unsigned_64 := 0; -- Total for one X,Y plane
      Rectangle : Rectangles;
      Rectangle_List : Rectangle_Lists.Vector := Rectangle_Lists.Empty_Vector;
      On_list : Coordinate_Lists.Map := Coordinate_Lists.Empty_Map;
      Y_Set, Z_Set : Coordinate_Sets.Set;
      Xc : Coordinate_Lists.Cursor;
      X, X_Delete, Y, Previous_Y, Z, Previous_Z : Coordinates;
      To_Add : Boolean;

   begin -- Part_Two_Count
      Build_Z_Set (Boot_List, Z_Set);
      Previous_Z := First_Element (Z_Set);
      Previous_Count_Z := 0;
      for Zc in Iterate (Z_Set) loop
         Z := Z_Set (Zc);
         Clear (Rectangle_List);
         Count_Z := 0;
         for B in Iterate (Boot_List) loop
            if Boot_List (B).Prism.Z1 <= Z and Z <= Boot_List (B).Prism.Z2 then
               Rectangle := (Command => Boot_List (B).Command,
                             X1 => Boot_List (B).Prism.X1,
                             X2 => Boot_List (B).Prism.X2,
                             Y1 => Boot_List (B).Prism.Y1,
                             Y2 => Boot_List (B).Prism.Y2);
               Append (Rectangle_List, Rectangle);
            end if; -- Boot_List (B).Prism.Z1 <= Z and ...
         end loop;
         Build_Y_Set (Rectangle_List, Y_Set);
         if not is_Empty (Y_Set) then
            -- this test is necessary to avoid a runtime error if Y_Set is empty
            Previous_Y := First_Element (Y_Set);
         end if; -- not Empty_Set (Y_Set)
         Previous_Count_Y := 0;
         for Yc in Iterate (Y_Set) loop
            Y := Y_Set (Yc);
            Count_Y := 0;
            Clear (On_list);
            for R in Iterate (Rectangle_List) loop
               if Rectangle_List (R).Y1 <= Y and Y <= Rectangle_List (R).Y2 then
                  Rectangle := Rectangle_List (R);
                  if Rectangle.Command then
                     -- Turn on
                     To_Add := True;
                     if Is_Empty (On_list) then
                        Insert (On_list, Rectangle.X1, Rectangle.X2);
                     else
                        Xc := First (On_List);
                        while Xc /= Coordinate_Lists.No_Element and
                          To_Add loop
                           if Rectangle.X1 <= Key (Xc) and
                             On_list (Xc) <= Rectangle.X2 then
                              -- An existing line of turned on cubes is totally
                              -- contained within the line to be turned on.
                              X_Delete := Key (Xc);
                              Delete (On_list, X_Delete);
                              Xc := Ceiling (On_list, X_Delete);
                           elsif Key (Xc) <= Rectangle.X1 and
                             Rectangle.X2 <= On_list (Xc) then
                              -- Rectangle is totally contained in existing
                              -- On_List element and thus can be discarded.
                              To_Add := False;
                           elsif Key (Xc) <= Rectangle.X2 and
                             Rectangle.X2 <= On_list (Xc) then
                              -- Extend Rectangle at X2 end
                              Rectangle.X2 := On_list (Xc);
                              X_Delete := Key (Xc);
                              Delete (On_list, X_Delete);
                              Xc := Ceiling (On_list, X_Delete);
                           elsif Key (Xc) <= Rectangle.X1 and
                             Rectangle.X1 <= On_list (Xc) then
                              -- Extend Rectangle at X1 end
                              Rectangle.X1 := Key (Xc);
                              X_Delete := Key (Xc);
                              Delete (On_list, X_Delete);
                              Xc := Ceiling (On_list, X_Delete);
                           else
                              -- No Overlap
                              Next (Xc);
                           end if; -- Rectangle.X1 <= Key (Xc) and ...
                        end loop; -- Xc /= Coordinate_Lists.No_Element
                        if To_Add then
                           -- After the above tests the line being added should
                           -- be non overlaping with any line in On_List.
                           Insert (On_list, Rectangle.X1, Rectangle.X2);
                        end if; -- To_Add
                     end if; -- Is_Empty (On_list)
                  else
                     -- Turn off
                     Xc := First (On_List);
                     while Xc /= Coordinate_Lists.No_Element loop
                        if Rectangle.X1 <= Key (Xc) and
                          On_list (Xc) <= Rectangle.X2 then
                           -- On_List element totally included in deletion,
                           --remove from On_list
                           X_Delete := Key (Xc);
                           X := On_list (Xc);
                           Delete (On_list, X_Delete);
                           Xc := Ceiling (On_list, X);
                        elsif Key (Xc) < Rectangle.X1 and
                          Rectangle.X2 < On_list (Xc) then
                           -- Deletion range lies completely within On_List
                           -- element, split in two. Note < and > operators
                           -- at least one cube must remain for a valid
                           -- On_List element!
                           X := On_list (Xc);
                           X_Delete := Key (Xc);
                           Delete (On_List, Xc);
                           -- Required to prevent run time error tampering
                           -- with cursor  Xc
                           Insert (On_list, X_Delete, Rectangle.X1 - 1);
                           Insert (On_list, Rectangle.X2 + 1, X);
                           Xc := Ceiling (On_List, Rectangle.X2);
                        elsif Key (Xc) <= Rectangle.X2 and
                          Rectangle.X2 < On_list (Xc) then
                           -- Deletion at X1, since this is the key a new
                           -- element must be included and the old one
                           -- deleted
                           X_Delete := Key (Xc);
                           X := On_list (Xc);
                           Delete (On_list, Xc);
                           insert (On_list, Rectangle.X2 + 1, X);
                           Xc := Ceiling (On_list, X);
                        elsif Key (Xc) < Rectangle.X1 and
                          Rectangle.X1 <= On_list (Xc) then
                           -- Deletion at X2 end, set X2 only.
                           On_List (Xc) := Rectangle.X1 - 1;
                           Next (Xc);
                        else
                           -- no overlap
                           Next (Xc);
                        end if; -- Rectangle.X1 <= Key (Xc) and
                     end loop; -- Xc /= Coordinate_Lists.No_Element
                  end if; -- Rectangle.Command
               end if; -- Rectangle_List (R).Y1 <= Y and ...
            end loop; -- R in Iterate (Rectangle_List)
            for O in Iterate (On_list) loop
               Assert (Key (O) <= On_list (O), "Invalid line");
               Count_Y := Count_Y + Unsigned_64 (On_List (O) - Key (O) + 1);
            end loop; -- O in Iterate (On_list)
            if Y - Previous_Y > 1 then
               Assert (Count_Y = Previous_Count_Y,
                       "Unexpected change in Count_Y");
               Count_Z := Count_Z + Count_Y * Unsigned_64 (Y - Previous_Y);
            else
               Count_Z := Count_Z + Count_Y;
            end if; -- Y - Previous_Y > 1
            Previous_Y := Y;
            Previous_Count_Y := Count_Y;
         end loop; -- Yc in Iterate (Y_Set)
         if Z - Previous_Z > 1 then
            Assert (Count_Z = Previous_Count_Z, "Unexpected change in Count_Z");
            Count := Count + Count_Z * Unsigned_64 (Z - Previous_Z);
         else
            Count := Count + Count_Z;
         end if; -- Z - Previous_Z > 1
         Previous_Z := Z;
         Previous_Count_Z := Count_Z;
      end loop; -- Zc in Iterate (Z_Sets)
      return Count;
   end Part_Two_Count;

   Boot_List : Boot_Lists.Vector;
   Cube_Array : Cube_Arrays := (others => (others => (others => False)));

begin -- December_22
   Get_Input (Boot_List);
   for B in Iterate (Boot_List) loop
      Update (Cube_Array, Boot_List (B));
   end loop; -- B in Iterate Boot_List)
   Put_Line ("Part One Answer:" & Cube_Count (Cube_Array)'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Put_Line ("Part Two Answer:" & Part_Two_Count (Boot_List)'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_22;
