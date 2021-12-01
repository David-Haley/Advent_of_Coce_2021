with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

procedure December_01 is

   Window : constant Positive := 3;

   package Depth_Tables is new Ada.Containers.Vectors (Positive, Positive);
   use Depth_Tables;

      function Sum (Depth_Table : in Depth_Tables.Vector;
                    Start : in Positive) return Positive is

         Result : Natural := 0;

      begin -- Sum
         for I in Positive range Start .. Start + Window - 1 loop
            Result := Result + Depth_Table (I);
         end loop; -- I in Positive range Start .. Start + Window - 1
         return Result;
      end Sum;

   Input_File : File_Type;
   Text : Unbounded_String;
   Depth, Previous_Depth : Positive;
   Increase : Natural := 0;
   Depth_Table : Depth_Tables.Vector;
   Depth_Index : Positive;

begin -- December_01
   Open (Input_File, In_File, "december_01.txt");
   Get_Line (Input_File, Text);
   Previous_Depth := Positive'Value (To_String (Text));
   Append (Depth_Table, Previous_Depth);
   while not End_Of_File (Input_File) loop
      Get_Line (Input_File, Text);
      Depth := Positive'Value (To_String (Text));
      Append (Depth_Table, Depth, 1);
      if Depth > Previous_Depth then
         Increase := Increase + 1;
      end if; -- if Depth > Previous_Depth then
      Previous_Depth := Depth;
   end loop; -- not End_Of_File (Input_File)
   Close (Input_File);
   Put_Line ("Number of Increases:" & Increase'Img);
   Increase := 0;
   Depth_Index := 1;
   while Count_Type (Depth_Index + Window) <= Length (Depth_Table) loop
      if Sum (Depth_Table, Depth_Index) < Sum (Depth_Table, Depth_Index + 1)
      then
         Increase := Increase + 1;
      end if; -- Sum (Depth_Table, Depth_Index) < Sum (Depth_Table, ...
      Depth_Index := Depth_Index + 1;
   end loop; -- Count_Type (Depth_Index + Window) <= Length (Depth_Table)
   Put_Line ("Part Two Number of Increases:" & Increase'Img);
end December_01;
