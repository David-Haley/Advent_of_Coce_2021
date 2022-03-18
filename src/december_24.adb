with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use  Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps; use Ada.Strings.Maps;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with DJH.Execution_Time;

procedure December_24 is

   -- Solution owes much to Day 24 megatrends. Push and Pop concept came from
   -- there; however the code is original. The virtual machine has been
   -- implemented and is to check that the answers actually return a z value
   -- of 0. The virtual machine can produce a full execution trace.

   Bad_Input : exception;

   subtype Words is Integer;
   subtype Model_Digits is Words range 1 .. 9;
   package Word_IO is new Ada.Text_IO.Integer_IO (Words);

   type OP_Codes is (inp, add, mul, div, mod_op, eql);
   -- Note mod_op has to be used instead of mod because "mod" is a reserved
   -- word in Ada. This also precludes directly reading the op code as an
   -- enumerated type;
   OP_Code_Stringe : constant array (OP_Codes) of String (1..3) :=
        ("inp", "add", "mul", "div", "mod", "eql");

   type Registers is (w, x, y, z);

   package Register_IO is new Ada.Text_IO.Enumeration_IO (Registers);

   type Register_Arrays is array (Registers) of Words;

   type Instructions is record
      OP_Code : OP_Codes;
      Destination : Registers;
      Immediate : Boolean := False;
      Value : Words := 0; -- valid for Immediate = True
      Source : Registers := x; -- valid for Immediate = False and Op_Code /= inp
   end record; -- Instructions

   subtype Input_Indices is Positive range 1 .. 14;
   type Input_Arrays is array (Input_Indices) of Words;

   subtype Source_Lines is Positive;

   package Program_Stores is new
     Ada.Containers.Vectors (Source_Lines, Instructions);
   use Program_Stores;

   type Code_Block_Element is record
      Start, Finish, Div_Instruction : Source_Lines;
   end record; -- Code_Block_Element

   type Code_Blocks is array (Input_Indices) of Code_Block_Element;

   type Parts is not null access
     procedure(Program_Store : in Program_Stores.Vector;
               Code_Block : in Code_Blocks;
               D1, D2 : in Input_Indices;
               Model : in out Input_Arrays);

   procedure Get_Input (Program_Store : out Program_Stores.Vector) is

      -- december_24 [Input_File_Name]

      Space : constant Character_Set := To_Set (" ");
      Register_Set : constant Character_Set := To_Set ("wxyz");
      Integer_Set : constant Character_Set := To_Set ("-0123456789");

      Input_File : File_Type;
      Text : Unbounded_String;
      Start_At, First : Positive;
      Last, IO_Last : Natural;
      Instruction : Instructions;

   begin -- Get_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_24.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         Start_At := 1;
         Find_Token (Text, Space, Start_At, Outside, First, Last);
         For O in OP_Codes loop
            if Slice (Text, First, Last) = OP_Code_Stringe (O) then
               Instruction.OP_Code := O;
            end if; -- Slice (Text, First, Last) = OP_Code_Stringe (O)
         end loop; -- O in OP_Codes
         if Slice (Text, First, Last) /=
           OP_Code_Stringe (Instruction.OP_Code) then
            raise Bad_Input with "Invalid OP Code ";
         end if; -- Slice (Text, First, Last) /= ...
         Start_At := Last + 1;
         Find_Token (Text, Register_Set, Start_At, Inside, First, Last);
         if Last /= 0 then
            Register_IO.Get (Slice (Text, First, Last), Instruction.Destination,
                             IO_Last);
         else
            raise Bad_Input with "No or bad destination register ";
         end if; -- Last /= 0
         if Instruction.OP_Code = inp then
            Instruction.Source := Instruction.Destination;
            Instruction.Immediate := False;
            Instruction.Value := 0;
         else
            Start_At := Last + 1;
            Find_Token (Text, Register_Set, Start_At, Inside, First, Last);
            if Last = 0 then
               Instruction.Immediate := True;
               Instruction.Source := x;
               Find_Token (Text, Integer_Set, Start_At, Inside, First, Last);
               if Last /= 0 then
                  Instruction.Value := Words'Value (Slice (Text, First, Last));
               else
                  raise Bad_Input with "No or bad immediate value ";
               end if; -- Last /= 0
            else
               Instruction.Immediate := False;
               Instruction.Value := 0;
               Register_IO.Get (Slice (Text, First, Last),
                                Instruction.Source, IO_Last);
            end if; -- Last = 0
         end if; -- Instruction.OP_Code = inp
         Append (Program_Store, Instruction);
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   exception
      when others =>
         Put_Line (Positive_Count'Image (Line (Input_File) - 1) & ": " & Text);
         raise;
   end Get_Input;

   procedure Find_Code_Blocks (Program_Store : in Program_Stores.Vector;
                               Code_Block : out Code_Blocks) is

      -- Finds the start and end of eack of the 14 similar code blocks and the
      -- location of the div z 1 or div z 26 instruction.

      Ic : Program_Stores.Cursor := First (Program_Store);

   begin -- Find_Code_Blocks
      for B in Input_Indices loop
         loop -- until add z y found
            if Program_Store (Ic).OP_Code = inp then
               Code_Block (B).Start := To_Index (Ic);
            elsif Program_Store (Ic).OP_Code = div then
               Code_Block (B).Div_Instruction := To_Index (Ic);
            elsif Program_Store (Ic).OP_Code = add and then
              (Program_Store (Ic).destination = z and
                   Program_Store (Ic).Source = y) then
               Code_Block (B).Finish := To_Index (Ic);
               exit;
            end if; --  Program_Store (Ic).OP_Code = inp
            Next (Ic);
         end loop; -- until add z y found
         Next (Ic);
      end loop; -- B in Input_Indices
   end Find_Code_Blocks;

   procedure Find_Pairs (Program_Store : in Program_Stores.Vector;
                         Code_Block : in Code_Blocks;
                         Place : in out Input_Indices;
                         Model : in out Input_Arrays;
                         Part : in Parts) is

      -- Recursively parses the virtial machine code to find the Push (div z 1)
      -- and Pop (div z 26) pairs. the Part prameter takes a procedure for
      -- part 1 and a different procedure for part 2. It simplifies the part 1
      -- and part part 2 code because they only consider single digit pairs.

      Push : constant Words := 1;
      Pop : constant Words := 26;
      D1, D2 : Input_Indices;

   begin -- Find_Pairs
      while Place < Input_Indices'Last and then
        Program_Store (Code_Block (Place).Div_Instruction).Value =
          Push loop
         D1 := Place;
         Place := Place + 1;
         if Program_Store (Code_Block (Place).Div_Instruction).Value =
           Push then
            -- call next level
            Find_Pairs (Program_Store, Code_Block, Place, Model, Part);
         end if; -- Push
         if Program_Store (Code_Block (Place).Div_Instruction).
           Value = Pop then
            D2 := Place;
            -- evaluate digit pair
            Part (Program_Store, Code_Block, D1, D2, Model);
            if Place < Input_Indices'Last then
               Place := Place + 1;
            end if; -- Place < Input_Indices'Last
         end if; -- Pop
      end loop; -- Place < Input_Indices'Last and not Matched
   end Find_Pairs;

   procedure Run (Program_Store : in Program_Stores.Vector;
                  Input_Array : in Input_Arrays;
                  Register_Array : in out Register_Arrays;
                  Run_From : Source_Lines := Source_Lines'First;
                  Run_Until : Source_Lines := Source_Lines'Last;
                  Input_Start : Input_Indices := 1;
                  Trace_File_Name : in String := "") is

      -- Run has been designed to be able to run any code fragment with any
      -- starting values for registers etc. Note the Input_Start parameter is
      -- necessary because of the way input reading is defined. The input being
      -- processed must match the code fragment being run!

      Reg_Field : constant Positive := 10;

      procedure Put_Instruction (List_File : in File_Type;
                                 Instruction : in Instructions;
                                 Source_Line : in Source_Lines;
                                 Input_Pointer : in Input_Indices) is

         List_Field : constant Positive := 4;

         package Line_Number_IO is new Ada.Text_IO.Integer_IO (Source_Lines);

      begin -- Put_Instruction
         Register_IO.Default_Setting := Lower_Case;
         Line_Number_IO.Put (List_File, Source_Line, List_Field);
         Put (List_File, ": " & OP_Code_Stringe (Instruction.OP_Code) & ' ');
         Register_IO.Put (List_File, Instruction.Destination, List_Field);
         If Instruction.OP_Code = inp then
            Put (List_File, '(');
            Line_Number_IO.Put (List_File, Input_Pointer, List_Field - 2);
            Put (List_File, ')');
         else
            if Instruction.Immediate then
               Word_IO.Put (List_File, Instruction.Value, List_Field);
            else
               Register_IO.Put (List_File, Instruction.Source, List_Field);
            end if; -- Program_Store (I).Immediate
         end if; -- Program_Store (I).OP_Code /= inp
         Put (List_File, ';');
      end Put_Instruction;

      I : Source_Lines := Run_From;
      Input_Pointer : Natural := Input_Start - 1;
      Instruction : Instructions;
      Operand : Words;
      List_File : File_Type;
      Trace : Boolean := Trace_File_Name'Length > 0;

   begin -- Run
      if Trace then
         Create (List_File, Out_File, Trace_File_Name);
         Put_Line (List_File,
                   "Line  Instruction           w         x         y         z"
                     );
      end if; -- Trace
      while I <= Last_Index (Program_Store) and I <= Run_Until loop
         Instruction := Program_Store (I);
         if Instruction.Immediate then
            Operand := Instruction.Value;
         else
            Operand := Register_Array (Instruction.Source);
         end if; -- Instruction.Immediate
         case Instruction.OP_Code is
            when inp =>
               Input_Pointer := Input_Pointer + 1;
               Register_Array (Instruction.Destination) :=
                 Input_Array (Input_Pointer);
            when add =>
               Register_Array (Instruction.Destination) :=
                 Register_Array (Instruction.Destination) + Operand;
            when mul =>
               Register_Array (Instruction.Destination) :=
                 Register_Array (Instruction.Destination) * Operand;
            when div =>
               Register_Array (Instruction.Destination) :=
                 Register_Array (Instruction.Destination) / Operand;
            when mod_op =>
               Register_Array (Instruction.Destination) :=
                 Register_Array (Instruction.Destination) mod Operand;
            when eql =>
               if Register_Array (Instruction.Destination) = Operand then
                  Register_Array (Instruction.Destination) := 1;
               else
                  Register_Array (Instruction.Destination) := 0;
               end if; -- Register_Array (Instruction.Destination) = Operand
         end case; -- Instruction.OP_Code
         if Trace then
            Put_Instruction (List_File, Program_Store (I), I, Input_Pointer);
            Word_IO.Put (List_File, Register_Array (w), Reg_Field);
            Word_IO.Put (List_File, Register_Array (x), Reg_Field);
            Word_IO.Put (List_File, Register_Array (y), Reg_Field);
            Word_IO.Put (List_File, Register_Array (z), Reg_Field);
            New_Line (List_File);
         end if; -- Trace
         I := I + 1;
      end loop; --  I <= Last_Index (Program_Store) and I <= Run_Until
      if Trace then
         Close (List_File);
      end if; -- Trace
   end Run;

   procedure Part_One (Program_Store : in Program_Stores.Vector;
                       Code_Block : in Code_Blocks;
                       D1, D2 : in Input_Indices;
                       Model : in out Input_Arrays) is

      -- For any digit pair D1, D2 it finds the largest Model (D1) and
      -- Model (D2) such that the value of z when Model (D1) inpit is read
      -- is returned after the block of code that reads in Model (D2) runs to
      -- its end.
      -- Note D1 is a more significant digit than D2, Further for the examples
      -- run the starting value of z does not affect the result, 0 is used for
      -- convenience.

      Register_Array : Register_Arrays;
      Found : Boolean := False;
      Out_Z : Words;

   begin -- Part_One
      Model (D1) := Model_Digits'Last;
      loop -- step through MSD
         Register_Array := (others => 0);
         Run (Program_Store => Program_Store,
              Input_Array => Model,
              Register_Array => Register_Array,
              Run_From => Code_Block (D1).Start,
              Run_Until => Code_Block (D1).Finish,
              Input_Start => D1);
         Out_Z := Register_Array (z);
         Model (D2) := Model_Digits'Last;
         loop -- step through LSD
            Register_Array := (z => Out_Z, others => 0);
            Run (Program_Store => Program_Store,
                 Input_Array => Model,
                 Register_Array => Register_Array,
                 Run_From => Code_Block (D2).Start,
                 Run_Until => Code_Block (D2).Finish,
                 Input_Start => D2);
            Found := Register_Array (z) = 0;
            exit when Found or Model (D2) = Model_Digits'First;
            Model (D2) := Model (D2) - 1;
         end loop; -- step through LSD
         exit when Found or Model (D1) = Model_Digits'First;
         Model (D1) := Model (D1) - 1;
      end loop; -- step through MSD
   end Part_One;

   procedure Part_Two (Program_Store : in Program_Stores.Vector;
                       Code_Block : in Code_Blocks;
                       D1, D2 : in Input_Indices;
                       Model : in out Input_Arrays) is

      -- See comments in Part_One above, similar code but in this case the
      -- smallest numbers are found;

      Register_Array : Register_Arrays;
      Found : Boolean := False;
      Out_Z : Words;

   begin -- Part_Two
      Model (D1) := Model_Digits'First;
      loop -- step through MSD
         Register_Array := (others => 0);
         Run (Program_Store => Program_Store,
              Input_Array => Model,
              Register_Array => Register_Array,
              Run_From => Code_Block (D1).Start,
              Run_Until => Code_Block (D1).Finish,
              Input_Start => D1);
         Out_Z := Register_Array (z);
         Model (D2) := Model_Digits'First;
         loop -- step through LSD
            Register_Array := (z => Out_Z, others => 0);
            Run (Program_Store => Program_Store,
                 Input_Array => Model,
                 Register_Array => Register_Array,
                 Run_From => Code_Block (D2).Start,
                 Run_Until => Code_Block (D2).Finish,
                 Input_Start => D2);
            Found := Register_Array (z) = 0;
            exit when Found or Model (D2) = Model_Digits'Last;
            Model (D2) := Model (D2) + 1;
         end loop; -- step through LSD
         exit when Found or Model (D1) = Model_Digits'Last;
         Model (D1) := Model (D1) + 1;
      end loop; -- step through MSD
   end Part_Two;

   Program_Store : Program_Stores.Vector;
   Model : Input_Arrays;
   Code_Block : Code_Blocks;
   Place : Input_Indices;
   Register_Array : Register_Arrays;

begin -- December_24
   Get_Input (Program_Store);
   Find_Code_Blocks (Program_Store, Code_Block);
   Place := Input_Indices'First;
   Find_Pairs (Program_Store, Code_Block, Place, Model, Part_One'Access);
   Register_Array := (others => 0);
   Run (Program_Store => Program_Store,
        Input_Array => Model,
        Register_Array => Register_Array,
        Trace_File_Name => "december_24_output_part_one.txt");
   Put_Line ("Check z:" & Register_Array (z)'Img);
   Put ("Part One Answer: ");
   for D in Input_Indices loop
      Put (Trim (Model (D)'Img, Both));
   end loop; -- D in Input_Indices
   New_Line;
   Place := Input_Indices'First;
   Find_Pairs (Program_Store, Code_Block, Place, Model, Part_Two'Access);
   Register_Array := (others => 0);
   Run (Program_Store => Program_Store,
        Input_Array => Model,
        Register_Array => Register_Array,
        Trace_File_Name => "december_24_output_part_two.txt");
   Put_Line ("Check z:" & Register_Array (z)'Img);
   DJH.Execution_Time.Put_CPU_Time;
   Put ("Part Two Answer: " );
   for D in Input_Indices loop
      Put (Trim (Model (D)'Img, Both));
   end loop; -- D in Input_Indices
   New_Line;
   DJH.Execution_Time.Put_CPU_Time;
end December_24;
