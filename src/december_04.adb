with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

procedure December_04 is

   type Bingo_Numbers is new Natural;

   package Number_Stores is new
     Ada.Containers.Vectors (Positive, Bingo_Numbers);
   use Number_Stores;

   type Card_Elements is record
      Bingo_Number : Bingo_Numbers;
      Called : Boolean := False;
   end record; -- Card_Elements
   type Rows is new Positive range 1 .. 5;
   type Columns is new Positive range 1 .. 5;
   type Cards is array (Rows, Columns) of Card_Elements;

   package Card_Stores is new Ada.Containers.Vectors (Positive, Cards);
   use Card_Stores;

   procedure Mark_Card (Card : in out Cards;
                        Drawn : in Bingo_Numbers) is

   begin -- Mark_Card
      for R in Rows loop
         for C in Columns loop
            Card (R, C).Called := Card (R, C).Called or
              Card (R, C).Bingo_Number = Drawn;
         end loop; -- C in Columns
      end loop; -- R in Rows)
   end Mark_Card;

   function Is_Winner (Card : in Cards) return Boolean is

      Row_Check : array (Rows) of Boolean := (others => True);
      Column_Check : array (Columns) of Boolean := (others => True);
      Result : Boolean := False;

   begin -- Is_Winner
      for R in Rows loop
         for C in Columns loop
            Row_Check (R) := Row_Check (R) and Card (R, C).Called;
            Column_Check (C) := Column_Check (C) and Card (R, C).Called;
         end loop; -- C in Columns
         Result := Result or Row_Check (R);
      end loop; -- R in Rows
      for C in Columns loop
         Result := Result or Column_Check (C);
      end loop; -- C in Columns
      return Result;
   end Is_Winner;

   function All_Winners (Card_Store : in Card_Stores.Vector) return Boolean is

      Result : Boolean := True;

   begin -- All_Winners
      for C in Iterate (Card_Store) loop
         Result := Result and Is_Winner (Card_Store (C));
      end loop; -- C in Iterate (Card_Store)
      return Result;
   end All_Winners;

   function Score (Card : in Cards;
                   Drawn : in Bingo_Numbers) return Bingo_Numbers is

      Undrawn_Sum : Bingo_Numbers := 0;

   begin -- Score
      for R in Rows loop
         for C in Columns loop
            if not Card (R, C).Called then
               Undrawn_Sum := Undrawn_Sum + Card (R, C).Bingo_Number;
            end if; -- not Card (R, C).Drawn
         end loop; -- C in Columns
      end loop; -- R in Rows
      return Undrawn_Sum * Drawn;
   end Score;

   Input_File : File_Type;
   Text : Unbounded_String;
   Start_At, First : Positive;
   Last : Natural;
   Number_Store : Number_Stores.Vector;
   Card, First_Winning_Card, Last_Winning_Card : Cards;
   Card_Store : Card_Stores.Vector;
   First_Winning_Number, Last_Winning_Number : Bingo_Numbers;
   First_Found, Last_Found : Boolean := False;
   Undrawn_Sum : Bingo_Numbers := 0;

begin -- December_04
   if Argument_Count = 0 then
      Open (Input_File, In_File, "december_04.txt");
   else
      Open (Input_File, In_File, Argument(1));
   end if; -- Argument_Count = 0
   -- read drawn numbers
   Get_Line (Input_File, Text);
   Start_At := 1;
   while Start_At < Length (Text) loop
      Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
      Append (Number_Store, Bingo_Numbers'Value(Slice (Text, First, Last)));
      Start_At := Last + 1;
   end loop; -- Start_At < Length (Text)
   while not End_Of_File (Input_File) loop
      Skip_Line (Input_File);
      for R in Rows loop
         Get_Line (Input_File, Text);
         Start_At := 1;
         for C in Columns loop
            Find_Token (Text, Decimal_Digit_Set, Start_At, Inside, First, Last);
            Card (R, C).Bingo_Number :=
              Bingo_Numbers'Value (Slice (Text, First, Last));
            Start_At := Last + 1;
         end loop; -- C in Columns
      end loop; -- R in Rows
      Append (Card_Store, Card);
   end loop; -- not End_Of_File (Input_File)
   Close (Input_File);
   for N in iterate (Number_Store) loop
      for C in Iterate (Card_Store) loop
         Mark_Card (Card_Store (C), Number_Store (N));
         if Is_Winner (Card_Store (C)) then
            if not First_Found then
               First_Winning_Number := Number_Store (N);
               First_Winning_Card := Card_Store (C);
               First_Found := True;
            end if; -- not First_Found
            if not Last_Found and then All_Winners (Card_Store) then
               Last_Winning_Number := Number_Store (N);
               Last_Winning_Card := Card_Store (C);
               Last_Found := True;
            end if; -- not Last_Found and then All_Winners (Card_Store)
         end if; -- Is_Winner (Card)
      end loop; -- C in Iterate (Card_Store)
   end loop; -- N in iterate (Number_Store)
   Put_Line ("Part One Answer:"  &
               Bingo_Numbers'Image (Score (First_Winning_Card,
               First_Winning_Number)));
   Put_Line ("Part Two Answer:"  &
               Bingo_Numbers'Image (Score (Last_Winning_Card,
               Last_Winning_Number)));
end December_04;
