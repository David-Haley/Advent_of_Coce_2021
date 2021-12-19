with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Assertions; use Ada.Assertions;
with Interfaces; use Interfaces;
with DJH.Execution_Time;

procedure December_18 is

   subtype SF_Numbers is Unbounded_String;

   package SF_Number_Stores is new Ada.Containers.Vectors (Positive, SF_Numbers);
   use SF_Number_Stores;

   procedure Get_Input (SF_Number_Store : out SF_Number_Stores.Vector) is

      -- december_18 [Input_File_Name]

      Input_File : File_Type;
      Text : Unbounded_String;

   begin -- Get_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_18.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Append (SF_Number_Store, Text);
      end loop; -- End_Of_File (Input_File)
      Close (Input_File);
   end Get_Input;

   function "+" (Left, Right : in SF_Numbers) return SF_Numbers is

      function Reduce (Sum : in SF_Numbers) return SF_Numbers is

         Procedure Add_Left (Source : in SF_Numbers;
                             Destination : in out SF_Numbers;
                             Left_First, Left_Last : in Positive) is

            Start_At, First, Left_Number_First : Positive;
            Last, Temp : Natural;
            Left_Number_Last : Natural := 0;

         begin -- Add_Left
            Start_At := 1;
            loop -- find left number
               Find_Token (Destination, Decimal_Digit_Set, Start_At, Inside,
                           First, Last);
               exit when Last = 0;
               Start_At := Last + 1;
               Left_Number_First := First;
               Left_Number_Last := Last;
            end loop;
            -- When this is called the current pair has not been copied
            if Left_Number_Last /= 0 then
               Temp := Natural'Value (Slice (Source, Left_First, Left_Last)) +
                 Natural'Value (Slice (Destination, Left_Number_First,
                                Left_Number_Last));
               Replace_Slice (Destination, Left_Number_First, Left_Number_Last,
                              Trim (Temp'Img, Both));
            end if; -- Left_Number_Last /= 0
         end Add_Left;

         Procedure Add_Right (Destination : in out SF_Numbers;
                              Right_First, Right_Last : in Positive) is

            Right_Number_First : Positive;
            Right_Number_Last, Temp : Natural;

         begin -- Add_Right
            Find_Token (Destination, Decimal_Digit_Set, Right_Last + 1, Inside,
                        Right_Number_First, Right_Number_Last);
            if Right_Number_Last /= 0 then
               Temp := Natural'Value (Slice (Destination, Right_First,
                                      Right_Last )) +
                 Natural'Value (Slice (Destination, Right_Number_First,
                                Right_Number_Last));
               Replace_Slice (Destination, Right_Number_First,
                              Right_Number_Last, Trim (Temp'Img, Both));
            end if; -- Right_Number_Last /= 0
         end Add_Right;

         procedure SF_Split (Destination : in out SF_Numbers;
                             First, Last : in Positive) is

            Temp : Positive;
            New_Pair : SF_Numbers := Null_Unbounded_String;

         begin -- SF_Split
            Temp := Natural'Value (Slice (Destination, First, Last));
            New_Pair := To_Unbounded_String
              ('[' & Trim (Natural'Image (Temp / 2), Both) & ',' &
                 Trim (Natural'Image ((Temp + 1) / 2), Both) & ']');
            Replace_Slice (Destination, First, Last, To_String (New_Pair));
         end SF_Split;

         Max_Depth : constant Natural := 4;
         Temp : SF_Numbers := Sum;
         I : Positive;
         Result : SF_Numbers;
         No_Explode, No_Split, Finished : Boolean;
         Depth : Natural;
         Left_First, Left_Last, Right_First, Right_Last : Positive;
         Start_At, First : Positive;
         Last : Natural;

      begin -- Reduce
         loop -- until no reduction
            No_Explode := True;
            No_Split := True;
            Result := Null_Unbounded_String;
            Depth := 0;
            I := 1;
            while I <= Length (Temp) loop
               if Element (Temp, I) = '[' then
                  Depth := Depth + 1;
               elsif Element (Temp, I) = ']' then
                  Depth := Depth - 1;
               end if; -- Element (Temp, I) = '['
               if Depth <= 4 then
                  Result := Result & Element (Temp, I);
               else
                  No_Explode := False;
                  Start_At := I;
                  -- Allow for exploding numbers which are yet to be split
                  Find_Token (Temp, Decimal_Digit_Set, Start_At, Inside,
                              Left_First, Left_Last);
                  Start_At := Left_Last + 1;
                  Assert (Left_First = I + 1, "Text after '['" &
                            Slice (Temp, I + 1, Left_First - 1));
                  Assert (Element (Temp, Start_At) = ',',
                          "Exoected ',' and found " &
                            Element (Temp, Start_At));
                  Find_Token (Temp, Decimal_Digit_Set, Start_At, Inside,
                              Right_First, Right_Last);
                  Assert (Right_First = Left_Last + 2, "Text after ','" &
                            Slice (Temp, Left_Last + 2, Right_First - 1));
                  Assert (Element (Temp, Right_Last + 1) = ']',
                          "Exoected ']' and found " &
                            Element (Temp, Right_Last + 1));
                  Add_Left (Temp, Result, Left_First, Left_Last);
                  Add_Right (Temp, Right_First, Right_Last);
                  -- Peplace the pair with 0 and copy remainder of Temp to
                  -- Result
                  Result := Result & '0' &
                    Slice (Temp, Right_Last + 2, Length (Temp));
                  I := Length (Temp);
                  -- I points to end with an I := I + 1 to follow which will
                  -- terminate the while loop.
               end if; -- Depth <= 4
               I := I + 1;
            end loop; -- I in Positive range 1 .. Length (Temp)
            -- Do most lefthand split, one only
            Finished := False;
            Start_At := 1;
            while No_Explode and No_Split and not Finished loop
               Find_Token (Result, Decimal_Digit_Set, Start_At, Inside, First,
                           Last);
               if Last >= First + 1 then
                  -- Two or more digit number found
                  No_Split := False;
                  SF_Split (Result, First, Last);
               end if; -- Last >= First + 1
               Start_At := Last + 1;
               Finished := Last = 0;
            end loop; -- No_Explode and No_Split and not Finished
            exit when No_Explode and No_Split;
            Temp := Result;
            Result := Null_Unbounded_String;
         end loop; -- until no reduction
         return Result;
      end Reduce;

      Sum : SF_Numbers;

   begin -- "+"
      if Left = Null_Unbounded_String then
         return Right;
      elsif Right = Null_Unbounded_String then
         return Left;
      elsif Left = Null_Unbounded_String and Right = Null_Unbounded_String then
         return Null_Unbounded_String;
      else
         Sum := ('[' & Left & ',' & Right & ']');
         return Reduce (Sum);
      end if; -- Left = Null_Unbounded_String
   end "+";

   function Magnitude (SF_Number_In : in SF_Numbers) return Natural is

      Function Evaluate (SF_Number : in out SF_Numbers) return Natural is

         Left, Right, Last : Natural;
         Start_At, First : Positive;

      begin -- Evaluate
         if Element (SF_Number, 1) = '[' then
            Delete (SF_Number, 1, 1);
            Left := Evaluate (SF_Number);
            Assert (Element (SF_Number, 1) = ',',
                    "Expected ',' found " & Element (SF_Number, 1));
            Delete (SF_Number, 1, 1);
            Right := Evaluate (SF_Number);
            Assert (Element (SF_Number, 1) = ']',
                    "Expected ']' found " & Element (SF_Number, 1));
            Delete (SF_Number, 1, 1);
            return 3 * Left + 2 * Right;
         else
            -- Should be a numeric literal
            Start_At := 1;
            Find_Token (SF_Number, Decimal_Digit_Set, Start_At, Inside, First,
                        Last);
            Assert (Last /= 0 and then First = 1,
                    "Expected numeric literal");
            Left := Natural'Value (Slice (SF_Number, First, Last));
            -- This could be either left or right, don't know and it doesen't
            -- matter just return the value, the higher level will determine the
            -- weighting, 3 or 2;
            Delete (SF_Number, First, Last);
            return Left;
         end if; -- Element (SF_Number, 1) = '['
      end Evaluate;

      SF_Number : SF_Numbers := SF_Number_In;
      Result : Natural;

   begin -- Magnitude
      Result := Evaluate (SF_Number);
      Assert (Length (SF_Number) = 0, "Unparsed text remains");
      -- If the parsing is complete even the last ']' should be deleted
      return Result;
   end Magnitude;

   SF_Number_Store : SF_Number_Stores.Vector;
   Total : SF_Numbers := Null_Unbounded_String;
   Sum, Part_Two : Natural;

begin -- December_18
   Get_Input (SF_Number_Store);
   for S in Iterate (SF_Number_Store) loop
      Total := Total + SF_Number_Store (S);
   end loop; -- S in Iterate (SF_Number_Store)
   Put_Line (Total);
   Put_Line ("Part One Answer:" & Natural'Image (Magnitude (Total)));
   Part_Two := 0;
   for F in Iterate (SF_Number_Store) loop
      for S in Iterate (SF_Number_Store) loop
         if F /= S then
            Sum := Magnitude (SF_Number_Store (S) + SF_Number_Store (F));
            if Sum > Part_Two then
               Part_Two := Sum;
            end if; -- Sum > Part_Two
         end if; -- F /= S
      end loop; -- S in Iterate (SF_Number_Store)
   end loop; -- F in Iterate (SF_Number_Store)
   Put_Line ("Part Two Answer:" & Part_Two'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_18;
