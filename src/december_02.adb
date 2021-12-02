with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure December_02 is

   type Directions is (Down, Up, Forward);
   package Direction_IO is new Ada.Text_IO.Enumeration_IO (Directions);
   use Direction_IO;

   Input_File : File_Type;
   Text : Unbounded_String;
   Start_At, Step : Positive;
   Last : Natural;
   X, Y : Natural := 0;
   Direction : Directions;
   Aim : Integer := 0;

begin -- December_02
   Open (Input_File, In_File, "december_02.txt");
   while not End_Of_Line (Input_File) loop
      Get_Line (Input_File, Text);
      Get (To_String (Text), Direction, Last);
      Start_At := Last + 1;
      Step := Positive'Value(Slice (Text, Start_At, Length (Text)));
      case Direction is
         when Down =>
            Y := Y + Step;
         when Forward =>
            X := X + Step;
         when Up =>
            Y := Y - Step;
      end case;
   end loop; -- not End_Of_Line (Input_File)
   Put_Line ("Part One Answer:" & Positive'Image (X * Y));
   Reset (Input_File);
   X := 0;
   Y := 0;
   while not End_Of_Line (Input_File) loop
      Get_Line (Input_File, Text);
      Get (To_String (Text), Direction, Last);
      Start_At := Last + 1;
      Step := Positive'Value(Slice (Text, Start_At, Length (Text)));
      case Direction is
         when Down =>
            Aim := Aim + Step;
         when Forward =>
            X := X + Step;
            Y := Y + Step * Aim;
         when Up =>
            Aim := Aim - Step;
      end case;
   end loop; -- not End_Of_Line (Input_File)
   Put_Line ("Part Two Answer:" & Positive'Image (X * Y));
   Close (Input_File);
end December_02;
