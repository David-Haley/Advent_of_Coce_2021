with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

procedure December_07 is

   subtype Positions is Natural;

   package Position_Stores is new Ada.Containers.Vectors (Positive, Positions);
   use Position_Stores;

   procedure Get_Input (Position_Store : out Position_Stores.Vector;
                       Upper, Lower : out Positions) is

      -- december_07 [Input_File_Name]

      Input_File : File_Type;
      Text : Unbounded_String;
      Start_At, First : Positive;
      Last : Natural;
      Position : Positions;

   begin -- Get_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_07.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Get_Line (Input_File, Text);
      Upper := Positions'First;
      Lower := Positions'Last;
      Start_At := 1;
      while Start_At < Length (Text) loop
         Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
         Position := Positions'Value (Slice (Text, First, Last));
         Append (Position_Store, Position);
         if Position > Upper then
            Upper := Position;
         end if; --  Position > Upper
         if Position < Lower then
            Lower := Position;
         end if; -- Position < Lower
         Start_At := Last + 1;
      end loop; -- Start_At < Length (Text)
      Close (Input_File);
   end Get_Input;

   function Minimum_Fuel (Position_Store : in Position_Stores.Vector;
                          Lower, Upper : in Positions) return Natural is

      -- This is a brute force solution which executes in time which is the
      -- product of the number of entries and their range.
      -- Analitically the optimum position to move to is the median, that is,
      -- the position where half the population is either side of the value.
      -- The median calculation can be done in linear time

      Result : Natural := Natural'Last;
      Fuel : Natural;

   begin -- Minimum_Fuel
      for P in Positions range Lower .. Upper loop
         Fuel := 0;
         for S in Iterate (Position_Store) loop
            Fuel := Fuel + abs (Position_Store (S) - P);
         end loop; -- S in Iterate (Position_Store)
         if Fuel < Result then
            Result := Fuel;
         end if; -- Fuel < Minimum_Fuel
      end loop; -- P in Positions range Lower .. Upper
      return Result;
   end Minimum_Fuel;

   function Minimum_Fuel_2 (Position_Store : in Position_Stores.Vector;
                            Lower, Upper : in Positions) return Natural is

      -- This is a brute force solution which executes in time which is the
      -- product of the number of entries and their range.
      -- Analitically the optimum position to move to is the mean. The mean
      -- calculation can be done in linear time.

      Result : Natural := Natural'Last;
      Fuel : Natural;

   begin -- Minimum_Fuel_
      for P in Positions range Lower .. Upper loop
         Fuel := 0;
         for S in Iterate (Position_Store) loop
            Fuel := Fuel + ((abs (Position_Store (S) - P)) ** 2 +
                            abs (Position_Store (S) - P)) / 2;
            -- sum of the arithenetic series 1, 2, 3, 4 ...
         end loop; -- S in Iterate (Position_Store)
         if Fuel < Result then
            Result := Fuel;
         end if; -- Fuel < Minimum_Fuel
      end loop; -- P in Positions range Lower .. Upper
      return Result;
   end Minimum_Fuel_2;

      Position_Store : Position_Stores.Vector;
      Lower, Upper : Positions;

begin -- December_07
   Get_Input (Position_Store, Upper, Lower);
   Put_Line ("Part One Answer:" &
               Minimum_Fuel (Position_Store, Lower, Upper)'Img);
   Put_Line ("Part Two Answer:" &
               Minimum_Fuel_2 (Position_Store, Lower, Upper)'Img);
end December_07;
