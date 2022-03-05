with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
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
                   Slice (Text, First - X_String'Length , First - 1) & '"');
         Boot_List_Element.Prism.X2 :=
           Coordinates'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text, Integer_Set, Start_At, Inside, First, Last);
         Assert (Y_String = Slice (Text, First - Y_String'Length , First - 1),
                 Ada.Text_IO.Count'Image (Line (Input_File) - 1) &
                   ": Expected """ & X_String & """ found """ &
           Slice (Text, First - Y_String'Length , First - 1) & '"');
         Boot_List_Element.Prism.Y1 :=
           Coordinates'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text, Integer_Set, Start_At, Inside, First, Last);
         Assert (Range_String = Slice (Text, First - Range_String'Length ,
                 First - 1), Ada.Text_IO.Count'Image (Line (Input_File) - 1) &
                   ": Expected """ & Range_String & """ and found """ &
                   Slice (Text, First - X_String'Length , First - 1) & '"');
         Boot_List_Element.Prism.Y2 :=
           Coordinates'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text, Integer_Set, Start_At, Inside, First, Last);
         Assert (Z_String = Slice (Text, First - Z_String'Length , First - 1),
                 Ada.Text_IO.Count'Image (Line (Input_File) - 1) &
                   ": Expected """ & Z_String & """ found """ &
                   Slice (Text, First - X_String'Length , First - 1) & '"');
         Boot_List_Element.Prism.Z1 :=
           Coordinates'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text, Integer_Set, Start_At, Inside, First, Last);
         Assert (Range_String = Slice (Text, First - Range_String'Length ,
                 First - 1), Ada.Text_IO.Count'Image (Line (Input_File) - 1) &
                   ": Expected """ & Range_String & """ found """ &
                   Slice (Text, First - X_String'Length , First - 1) & '"');
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

   procedure Find_Z_Limits (Boot_List : in Boot_Lists.Vector;
                            Z_Min, Z_Max : out Coordinates) is

   begin -- Find_Z_Limits
      Z_Min := Coordinates'Last;
      Z_Max := Coordinates'First;
      for B in Iterate (Boot_List) loop
         if Boot_List (B).Prism.Z1 < Z_Min then
            Z_Min := Boot_List (B).Prism.Z1;
         end if; -- Boot_List (B).Prism.Z1 < Z_Min
         if Boot_List (B).Prism.Z2 > Z_Max then
            Z_Max := Boot_List (B).Prism.Z2;
         end if; -- Boot_List (B).Z2 > Z_Max
      end loop; -- B in Iterate (Boot_List)
   end Find_Z_Limits;

   function Part_Two_Count (Boot_List : in Boot_Lists.Vector;
                            Z_Min, Z_Max : in Coordinates)
                            return Unsigned_64 is

      subtype Z_Coordinates is Coordinates range Z_Min .. Z_Max;

      type Rectangles is Record
         X1, Y1, X2, Y2 : Coordinates;
      end record; -- Rectangles

      subtype Vertex_Counts is Natural with
         Static_Predicate => Vertex_Counts in 0 .. 2 | 4;

      package On_Lists is new Ada.Containers.Doubly_Linked_Lists (Rectangles);
      use On_Lists;

      package On_Queue_Inteface is new
        Ada.Containers.Synchronized_Queue_Interfaces (Rectangles);
      package On_Queues is new
        Ada.Containers.Unbounded_Synchronized_Queues (On_Queue_Inteface);
      use On_Queues;

      function Overlap (Left, Right : in Rectangles)
                        return Boolean is

         -- Left has at least one vertex in Right

      begin -- Overlap
         return (((Right.X1 <= Left.X1 and Left.X1 <= Right.X2)
                 or
                   (Right.X1 <= Left.X2 and Left.X2 <= Right.X2))
                 and
                   ((Right.Y1 <= Left.Y1 and Left.Y1 <= Right.Y2)
                    or
                      (Right.Y1 <= Left.Y2 and Left.Y2 <= Right.Y2)));
      end Overlap;

      function Contains (Left, Right : in Rectangles)
                         return Boolean is

         -- Left fits entirely within Right

      begin -- Contains
         return Right.X1 <= Left.X1 and Left.X2 <= Right.X2 and
           Right.Y1 <= Left.Y1 and Left.Y2 <= Right.Y2;
      end Contains;

      function Vertex_Q1 (Left, Right : in Rectangles)
                             return Boolean is

      begin -- Vertex_Q1
         return Right.X1 <= Left.X2 and Left.X2 <= Right.X2 and
           Right.Y1 <= Left.Y2 and Left.Y2 <= Right.Y2;
      end Vertex_Q1;

      function Vertex_Q2 (Left, Right : in Rectangles)
                             return Boolean is

      begin -- Vertex_Q2
         return Right.X1 <= Left.X2 and Left.X2 <= Right.X2 and
           Right.Y1 <= Left.Y1 and Left.Y1 <= Right.Y2;
      end Vertex_Q2;

      function Vertex_Q3 (Left, Right : in Rectangles)
                             return Boolean is

      begin -- Vertex_Q3
         return Right.X1 <= Left.X1 and Left.X1 <= Right.X2 and
           Right.Y1 <= Left.Y1 and Left.Y1 <= Right.Y2;
      end Vertex_Q3;

      function Vertex_Q4 (Left, Right : in Rectangles)
                             return Boolean is

      begin -- Vertex_Q4
         return Right.X1 <= Left.X1 and Left.X1 <= Right.X2 and
           Right.Y1 <= Left.Y2 and Left.Y2 <= Right.Y2;
      end Vertex_Q4;

      function Count_Vetices (Left, Right : in Rectangles)
                                 return Vertex_Counts is

         -- Returns the count of Left vertices within Right

         Result : Natural := 0;

      begin -- Count_Vetices
         if Vertex_Q1 (Left, Right) then
            Result := Result + 1;
         end if; -- Vertex_Q1 (Left, Right)
         if Vertex_Q2 (Left, Right) then
            Result := Result + 1;
         end if; -- Vertex_Q2 (Left, Right)
         if Vertex_Q3 (Left, Right) then
            Result := Result + 1;
         end if; -- Vertex_Q3 (Left, Right)
         if Vertex_Q4 (Left, Right) then
            Result := Result + 1;
         end if; -- Vertex_Q4 (Left, Right)
         return Result;
      end Count_Vetices;

      procedure Split (Unchanged, To_Split : in Rectangles;
                       On_Queue : in out On_Queues.Queue;
                      Normal_Cut : Boolean := True) is

         function Valid (Rectangle : in Rectangles) return Boolean is

         begin -- Valid
            return Rectangle.X1 <= Rectangle.X2 and
              Rectangle.Y1 <= Rectangle.Y2;
         end Valid;

         type Rectangle_Indices is (C, D, E, F);
         Rectangle_Array : array (Rectangle_Indices) of Rectangles;

         Limit : Rectangle_Indices;

      begin -- Split
         --  Put_Line ("Vc" & Count_Vetices (Unchanged, To_Split)'Img);
         case Count_Vetices (Unchanged, To_Split) is
            when 4 =>
               Limit := F;
               Rectangle_Array (C) :=  (To_Split.X1, Unchanged.Y2 + 1,
                                        Unchanged.X2, To_Split.Y2);
               Rectangle_Array (D) := (Unchanged.X2 + 1, Unchanged.Y1,
                                       To_Split.X2, To_Split.Y2);
               Rectangle_Array (E) := (Unchanged.X1, To_Split.Y1,
                                       To_Split.X2, Unchanged.Y1 - 1);
               Rectangle_Array (F) := (To_Split.X1, To_Split.Y1,
                                       Unchanged.X1 - 1, Unchanged.Y2);
            when 2 =>
               if Normal_Cut then
                  Limit := E;
                  if Vertex_Q2 (Unchanged, To_Split) and
                    Vertex_Q3 (Unchanged, To_Split) then
                     Rectangle_Array (C) := (To_Split.X1, To_Split.Y1,
                                             To_Split.X2, Unchanged.Y1 - 1);
                     Rectangle_Array (D) := (Unchanged.X2 + 1, Unchanged.Y1,
                                             To_Split.X2, To_Split.Y2);
                     Rectangle_Array (E) := (To_Split.X1, Unchanged.Y1,
                                             Unchanged.X1 - 1, To_Split.Y2);
                  elsif Vertex_Q3 (Unchanged, To_Split) and
                    Vertex_Q4 (Unchanged, To_Split) then
                     Rectangle_Array (C) := (To_Split.X1, To_Split.Y1,
                                             Unchanged.X1 - 1, To_Split.Y2);
                     Rectangle_Array (D) := (Unchanged.X1, To_Split.Y1,
                                             To_Split.X2, Unchanged.Y1 - 1);
                     Rectangle_Array (E) := (Unchanged.X1, Unchanged.Y2 + 1,
                                             To_Split.X2, To_Split.Y2);
                  elsif Vertex_Q1 (Unchanged, To_Split) and
                    Vertex_Q4 (Unchanged, To_Split) then
                     Rectangle_Array (C) := (To_Split.X1, Unchanged.Y2 + 1,
                                             To_Split.X2, To_Split.Y2);
                     Rectangle_Array (D) := (To_Split.X1, To_Split.Y1,
                                             Unchanged.X1 - 1, Unchanged.Y2);
                     Rectangle_Array (E) := (Unchanged.X2 + 1, To_Split.Y1,
                                             To_Split.X2, Unchanged.Y2);
                  elsif Vertex_Q1 (Unchanged, To_Split) and
                    Vertex_Q2 (Unchanged, To_Split) then
                     Rectangle_Array (C) := (Unchanged.X2 + 1, To_Split.Y1,
                                             To_Split.X2, To_Split.Y2);
                     Rectangle_Array (D) := (To_Split.X1, Unchanged.Y2 + 1,
                                             Unchanged.X2, To_Split.Y2);
                     Rectangle_Array (E) := (To_Split.X1, To_Split.Y1,
                                             Unchanged.X2, Unchanged.Y1 - 1);
                  end if; -- Vertex_Q2 (Unchanged, To_Split)
               else
                  Limit := C;
                  if Vertex_Q2 (Unchanged, To_Split) and
                    Vertex_Q3 (Unchanged, To_Split) then
                     Rectangle_Array (C) := (Unchanged.X1, To_Split.Y2 + 1,
                                             Unchanged.X2, Unchanged.Y2);
                  elsif Vertex_Q3 (Unchanged, To_Split) and
                    Vertex_Q4 (Unchanged, To_Split) then
                     Rectangle_Array (C) := (To_Split.X2 + 1, Unchanged.Y1,
                                             Unchanged.X2, Unchanged.y2);
                  elsif Vertex_Q1 (Unchanged, To_Split) and
                    Vertex_Q4 (Unchanged, To_Split) then
                     Rectangle_Array (C) := (Unchanged.X1, Unchanged.Y1,
                                             Unchanged.X2, To_Split.Y1 - 1);
                  elsif Vertex_Q1 (Unchanged, To_Split) and
                    Vertex_Q2 (Unchanged, To_Split) then
                     Rectangle_Array (C) := (Unchanged.X1, Unchanged.Y1,
                                             To_Split.X1 - 1, Unchanged.Y2);
                  end if; -- Vertex_Q2 (Unchanged, To_Split)
               end if; -- Normal_Cut
            when 1 =>
               Limit := D;
               if Vertex_Q1 (Unchanged, To_Split) then
                  Rectangle_Array (C) := (Unchanged.X2 + 1, To_Split.Y1,
                                         To_Split.X2, To_Split.Y2);
                  Rectangle_Array (D) := (To_Split.X1, Unchanged.Y2 + 1,
                                         Unchanged.X2, To_Split.Y2);
               elsif Vertex_Q2 (Unchanged, To_Split) then
                  Rectangle_Array (C) := (Unchanged.X2 + 1, To_Split.Y1,
                                         To_Split.X2, To_Split.Y2);
                  Rectangle_Array (D) := (To_Split.X1, To_Split.Y1,
                                         Unchanged.X2, Unchanged.Y1 - 1);
               elsif Vertex_Q3 (Unchanged, To_Split) then
                  Rectangle_Array (C) := (To_Split.X1, To_Split.Y1,
                                         Unchanged.X1 - 1, To_Split.Y2);
                  Rectangle_Array (D) := (Unchanged.X1, To_Split.Y1,
                                         To_Split.X2, Unchanged.Y1 -1);
               elsif Vertex_Q4 (Unchanged, To_Split) then
                  Rectangle_Array (C) := (To_Split.X1, To_Split.Y1,
                                          Unchanged.X1 - 1, To_Split.Y2);
                  Rectangle_Array (D) := (Unchanged.X1, Unchanged.Y2 + 1,
                                          To_Split.X2, To_Split.Y2);
               end if; -- Vertex_Q1 (Unchanged, To_Split)
            when 0 =>
               null; -- nothing to do
               Rectangle_Array (C) := (0, 0, -1, -1); -- Invalid rectangle
               Limit := C;
         end case; -- Count_Vetices (Unchanged, To_Split)
         for R in Rectangle_Indices range C .. Limit loop
            --  Put_Line (R'Img & Rectangle_Array (R).X1'Img & Rectangle_Array (R).Y1'Img & Rectangle_Array (R).X2'Img & Rectangle_Array (R).Y2'Img);
            if Valid (Rectangle_Array (R)) then
               On_Queue.Enqueue (Rectangle_Array (R));
            end if; -- Valid (Rectangle_Array (R))
         end loop; -- R in Rectangle_Indices range C .. Limit
      end Split;

      function Valid_On_List (Rectangle : in Rectangles;
                              On_List : in On_Lists.List) return Boolean is

         Result : Boolean := False;

      begin -- Valid_On_List
         for o in Iterate (On_List) loop
            Result := Result or Overlap (Rectangle, On_List (O)) or
              Overlap (On_List (O), Rectangle);
         end loop;
         return not Result;
      end Valid_On_List;

      Count, Count_Z : Unsigned_64 := 0;
      Oc, To_Delete : On_Lists.Cursor;

      Rectangle : Rectangles;
      On_List : On_Lists.List;
      On_Queue : On_Queues.Queue;
      Processed : Boolean;

   begin -- Part_Two_Count
      for Z in Z_Coordinates loop
         Clear (On_List);
         Count_Z := 0;
         for B in Iterate (Boot_List) loop
            if Boot_List (B).Prism.Z1 <= Z and
              Z <= Boot_List (B).Prism.Z2 then
               Rectangle := (X1 => Boot_List (B).Prism.X1,
                             Y1 => Boot_List (B).Prism.Y1,
                             X2 => Boot_List (B).Prism.X2,
                             Y2 => Boot_List (B).Prism.Y2);
               if Boot_List (B).Command then
                  -- Turn on Rectangle
                  --  Put_Line ("On " & Rectangle.X1'Img & Rectangle.Y1'Img & Rectangle.X2'Img & Rectangle.Y2'Img);
                  On_Queue.Enqueue (Rectangle);
                  while On_Queue.Current_Use > 0 loop
                     On_Queue.Dequeue (Rectangle);
                     Processed := False;
                     Oc := First (On_List);
                     while Oc /= On_Lists.No_Element and not Processed loop
                        if Contains (On_List (Oc), Rectangle) then
                           Assert (Count_Vetices (On_List (Oc), Rectangle) = 4,
                                   "On contains /= 4");
                           --  Put_Line ("On contains");
                           To_Delete := Oc;
                           Next (Oc);
                           Delete (On_List, To_Delete);
                           -- remove from list because it is totally contained
                           -- in the rectangle to be added.
                        elsif Contains (Rectangle, On_List (Oc)) then
                           --  Put_Line ("On already on");
                           Assert (Count_Vetices (Rectangle, On_List (Oc)) = 4,
                                  "On already on /= 4");
                           -- Already on so do nothng
                           Processed := True;
                        elsif Overlap (On_List (Oc), Rectangle) then
                           --  Put ("On overlap L R");
                           if Count_Vetices (On_List (Oc), Rectangle) = 2 then
                              --  Put_Line (" 2");
                              Split (On_List (Oc), Rectangle, On_Queue, False);
                              To_Delete := Oc;
                              Next (Oc);
                              Delete (On_List, To_Delete);
                           else
                              Assert (Count_Vetices (On_List (Oc), Rectangle)
                                      = 1, "On overlap L R /= 1");
                              --  Put_Line (" 1");
                              Split (On_List (Oc), Rectangle, On_Queue);
                              Processed := True;
                           end if; -- Count_Vetices (On_List (Oc), ...
                        elsif Overlap (Rectangle, On_List (Oc)) then
                           Assert (Count_Vetices (Rectangle, On_List (Oc)) = 2,
                                   "On overlap R L /= 2");
                           --  Put_Line ("On overlap R L");
                           Split (Rectangle, On_List (Oc), On_Queue, False);
                           Processed := True;
                        else
                           --  Put_Line ("On no overlap");
                           Next (Oc);
                        end if; -- Contains (On_List (Oc), Rectangle)
                     end loop; -- Oc /= On_Lists.No_Element and not Processed
                     if not Processed then
                        -- did not overlap anything now in list
                        Append (On_List, Rectangle);
                     end if; -- not Processed
                  end loop; -- On_Queue.Current_Use > 0
               else
                  -- Turn off Rectangle
                  --  Put_Line ("Off " & Rectangle.X1'Img & Rectangle.Y1'Img & Rectangle.X2'Img & Rectangle.Y2'Img);
                  Assert (On_Queue.Current_Use = 0, "Non empty queue");
                  Oc := First (On_List);
                  while Oc /= On_Lists.No_Element loop
                     if Contains (On_List (Oc), Rectangle) then
                        Assert (Count_Vetices (On_List (Oc), Rectangle)
                                = 4, "Off contains /= 4");
                        --  Put_Line ("Off contains");
                        To_Delete := Oc;
                        Next (Oc);
                        Delete (On_List, To_Delete);
                     elsif  Overlap (Rectangle, On_List (Oc)) then
                        --  Put_Line ("Off overlap R L");
                        Split (Rectangle, On_List (Oc), On_Queue);
                        To_Delete := Oc;
                        Next (Oc);
                        Delete (On_List, To_Delete);
                     elsif  Overlap (On_List (Oc), Rectangle) then
                        Assert (Count_Vetices (On_List (Oc), Rectangle)
                                = 2, "Off overlap L R /= 2");
                        --  Put_Line ("Off overlap L R");
                        Split (On_List (Oc), Rectangle, On_Queue, False);
                        To_Delete := Oc;
                        Next (Oc);
                        Delete (On_List, To_Delete);
                     else
                        --  Put_Line ("Off no overlap");
                        Next (Oc);
                     end if; -- Contains (On_List (Oc), Rectangle)
                  end loop; -- Oc /= On_Lists.No_Element
                  while On_Queue.Current_Use > 0 loop
                     On_Queue.Dequeue (Rectangle);
                     Append (On_List, Rectangle);
                  end loop; -- On_Queue.Current_Use > 0
               end if; -- Boot_List (B).Command
            end if; -- Boot_List (B).Prism.Z1 <= Z
         end loop; -- B in Iterate (Boot_List)
         for O in Iterate (On_List) loop
            --  Put_Line (On_List (O).X1'Img & On_List (O).Y1'Img & On_List (O).X2'Img & On_List (O).Y2'Img & Unsigned_64'Image (Unsigned_64 (On_List (O).X2 - On_List (O).X1 + 1) * Unsigned_64(On_List (O).Y2 - On_List (O).Y1 + 1)));
            Count_Z := Count_Z + Unsigned_64 (On_List (O).X2 - On_List (O).X1 + 1) *
              Unsigned_64 (On_List (O).Y2 - On_List (O).Y1 + 1);
         end loop; -- O in Iterate (On_List)
         Count := Count + Count_Z;
         --  Put_Line ("Z:" & Z'Img & Count_Z'Img & Count'Img);
      end loop; -- Z in Z_Coordinates
      return Count;
   end Part_Two_Count;

   Boot_List : Boot_Lists.Vector;
      Cube_Array : Cube_Arrays := (others => (others => (others => False)));
      Z_Min, Z_Max : Coordinates;

begin -- December_22
   Get_Input (Boot_List);
   for I in Iterate (Boot_List) loop
      Update (Cube_Array, Boot_List (I));
   end loop; -- I in Iterate Boot_List)
   Put_Line ("Part One Answer:" & Cube_Count (Cube_Array)'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Find_Z_Limits (Boot_List, Z_Min, Z_Max);
   Put_Line ("Part Two Answer:" & Part_Two_Count (Boot_List, Z_Min, Z_Max)'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Put_Line (Unsigned_64'Image (Unsigned_64'Last));
end December_22;
