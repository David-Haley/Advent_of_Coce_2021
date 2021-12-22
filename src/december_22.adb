with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
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
            Put_Line ("Bad Command: " & Slice (Text, First, Last));
         end if; -- Slice (Text, First, Last) = "on"
         Start_At := Last + 1;
         Find_Token (Text, Integer_Set, Start_At, Inside, First, Last);
         Boot_List_Element.Prism.X1 :=
           Coordinates'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text, Integer_Set, Start_At, Inside, First, Last);
         Boot_List_Element.Prism.X2 :=
           Coordinates'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text, Integer_Set, Start_At, Inside, First, Last);
         Boot_List_Element.Prism.Y1 :=
           Coordinates'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text, Integer_Set, Start_At, Inside, First, Last);
         Boot_List_Element.Prism.Y2 :=
           Coordinates'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text, Integer_Set, Start_At, Inside, First, Last);
         Boot_List_Element.Prism.Z1 :=
           Coordinates'Value (Slice (Text, First, Last));
         Start_At := Last + 1;
         Find_Token (Text, Integer_Set, Start_At, Inside, First, Last);
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

   Boot_List : Boot_Lists.Vector;
   Cube_Array : Cube_Arrays := (others => (others => (others => False)));

begin -- December_22
   Get_Input (Boot_List);
   for I in Iterate (Boot_List) loop
      Update (Cube_Array, Boot_List (I));
   end loop; -- I in Iterate Boot_List)
   Put_Line ("Part One Answer:" & Cube_Count (Cube_Array)'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_22;
