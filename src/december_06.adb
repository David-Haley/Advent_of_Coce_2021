with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Interfaces; use Interfaces;
procedure December_06 is

   subtype First_Cycles is Positive range 1 .. 2;
   type Adult_Cycles is mod 7;

   type Juvanile_Populations is array (First_Cycles) of Unsigned_64;
   type Adult_Populations is array (Adult_Cycles) of Unsigned_64;

   type Schools is record
      Juvanile_Population : Juvanile_Populations := (others => 0);
      Adult_Population : Adult_Populations := (others => 0);
   end record; -- Schools

   procedure Update (School : in out Schools) is

      Hatchling_Count : constant Unsigned_64 :=
        School.Adult_Population (Adult_Cycles'First);
      New_School : Schools;

   begin -- Update
      for A in Adult_Cycles loop
         New_School.Adult_Population (A - 1) :=
           School.Adult_Population (A);
      end loop; -- A in Adult_Cycles
      New_School.Adult_Population (Adult_Cycles'Last) :=
        New_School.Adult_Population (Adult_Cycles'Last) +
        School.Juvanile_Population (1);
      New_School.Juvanile_Population (1) := School.Juvanile_Population (2);
      New_School.Juvanile_Population (2) := Hatchling_Count;
      School := New_School;
   end Update;

   function Population_Count (School : in Schools) return Unsigned_64 is

      Total : Unsigned_64 := 0;

   begin -- Population_Count
      for A in Adult_Cycles loop
         Total := Total + School.Adult_Population (A);
      end loop; -- A in Adult_Cycles
      for J in First_Cycles loop
         Total := Total + School.Juvanile_Population (J);
      end loop; -- J in First_Cycles
      return Total;
   end Population_Count;

   Input_File : File_Type;
   Text : Unbounded_String;
   Start_At, First : Positive;
   Last : Natural;
   School : Schools;
   Adult_Cycle : Adult_Cycles;
   Day_Limit : Positive := 80;
   Day : Positive := 1;

begin -- December_06
   if Argument_Count = 0 then
      Open (Input_File, In_File, "december_06.txt");
   else
      Open (Input_File, In_File, Argument(1));
      if Argument_Count > 1 then
         Day_Limit := Positive'Value (Argument (2));
      end if; -- Argument_Count > 1
   end if; -- Argument_Count = 0
   Get_Line (Input_File, Text);
   Start_At := 1;
   while Start_At < Length (Text) loop
      Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
      Adult_Cycle := Adult_Cycles'Value (Slice (Text, First, Last));
      School.Adult_Population (Adult_Cycle) :=
        School.Adult_Population (Adult_Cycle) + 1;
      Start_At := Last + 1;
   end loop; -- Start_At < Length (Text)
   Close (Input_File);
   While Day <= Day_Limit loop
      Update (School);
      Day := Day + 1;
   end loop; -- D in Positive range 1 .. Day_Limit
   Put_Line ("Part One Answer:" & Population_Count (School)'Img);
   while Day <= 256 loop
      Update (School);
      Day := Day + 1;
   end loop; -- D in Positive range 1 .. Day_Limit
   Put_Line ("Part Two Answer:" & Population_Count (School)'Img);
end December_06;
