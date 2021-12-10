with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
With Interfaces; use Interfaces;
with DJH.Execution_Time;

procedure December_10 is

   subtype Scores is Unsigned_64;
   package Score_Lists is new Ada.Containers.Vectors (Positive, Scores);
   use Score_Lists;
   package Score_List_Sorting is new Score_Lists.Generic_Sorting;

   procedure Parse (Text : in out Unbounded_String;
                    Error_Score : out Natural;
                    Error_Found : out Boolean;
                    Auto_Complete_Score : out Scores) is

      subtype Opening_Tokens is Character with
        Static_Predicate => Opening_Tokens in '(' | '[' | '{' | '<';
      subtype Closing_Tokens is Character  with
        Static_Predicate => Closing_Tokens in ')' | ']' | '}' | '>';

      function Value_Error (Found : in Closing_Tokens) return Positive is

      begin -- Value_Error
         case Found is
            when ')' => return 3;
            when ']' => return 57;
            when '}' => return 1197;
            when '>' => return 25137;
         end case; -- Value_Error
      end Value_Error;

      function Value_Auto_Complete (Left : in Opening_Tokens) return Scores is

      begin -- Value_Auto_Complete
         case Left is
            when '(' => return 1; -- Auto complete with ')'
            when '[' => return 2; -- Auto complete with ']'
            when '{' => return 3; -- Auto complete with '}'
            when '<' => return 4; -- Auto complete with '>'
         end case; -- Left
      end Value_Auto_Complete;

      Left_Token : Opening_Tokens;

   begin -- Parse
      Error_Found := False;
      Error_Score := 0;
      Auto_Complete_Score := 0;
      while (not Error_Found and Length (Text) > 0) and then
        Element (Text, 1) in Opening_Tokens loop
         Left_Token := Element (Text, 1);
         Delete (Text, 1, 1);
         Parse (Text, Error_Score, Error_Found, Auto_Complete_Score);
         Error_Found := Error_Found or Length (Text) = 0;
         if not Error_Found then
            case Left_Token is
               when '(' => Error_Found := Element (Text, 1) /= ')';
               when '[' => Error_Found := Element (Text, 1) /= ']';
               when '{' => Error_Found := Element (Text, 1) /= '}';
               when '<' => Error_Found := Element (Text, 1) /= '>';
            end case; -- Left_Token
            if Error_Found then
               Error_Score := Value_Error (Element (Text, 1));
            end if; -- Error_Found
            Delete (Text, 1, 1);
         end if; -- not Error_Found
      end loop; -- (not Error_Found and Length (Text) > 0) and then ...
      if Error_Found and Error_Score = 0 then
         -- incomplete line
         Auto_Complete_Score := Auto_Complete_Score * 5 +
           Value_Auto_Complete (Left_Token);
      end if; -- Error_Found and Error_Score = 0
   end Parse;

   Input_File : File_Type;
   Text : Unbounded_String;
   Error_Found : Boolean;
   Error_Score, Total_Error_Score : Natural;
   Auto_Complete_Score : Scores;
   Score_List : Score_Lists.Vector;
   Middle : Positive;

   -- december_10 [Input_File_Name]

begin -- December_10
   if Argument_Count = 0 then
      Open (Input_File, In_File, "december_10.txt");
   else
      Open (Input_File, In_File, Argument(1));
   end if; -- Argument_Count = 0
   Total_Error_Score := 0;
   while not End_Of_File (Input_File) loop
      Get_Line (Input_File, Text);
      Parse (Text, Error_Score, Error_Found, Auto_Complete_Score);
      Total_Error_Score := Total_Error_Score + Error_Score;
      if Error_Found and Error_Score = 0 then
         Append (Score_List, Auto_Complete_Score);
      end if; -- Error_Found and Error_Score
   end loop; -- not End_Of_File (Input_File)
   Close (Input_File);
   Put_Line ("Part One Answer:" & Total_Error_Score'Img);
   Middle := Positive (Length (Score_List) / 2 + 1);
   Score_List_Sorting.Sort (Score_List);
   Put_Line ("Part Two Answer:" & Scores'Image (Score_List (Middle)));
   -- Above will raise an exception if no incomplete lines found
   DJH.Execution_Time.Put_CPU_Time;
end December_10;
