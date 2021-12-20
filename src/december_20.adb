with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Ordered_Maps;
with Ada.Assertions; use Ada.Assertions;
with Interfaces; use Interfaces;
with DJH.Execution_Time;

procedure December_20 is

   Lit_Pixel : constant Character := '#';
   Part_One_Iterations : constant Positive := 2;
   Part_Two_Iterations : constant Positive := 50;


   subtype Pixel_Indices is Unsigned_16 range 0 .. 2#111111111#;
   subtype Bits is Natural range 0 .. 8;

   package Pixel_Index_IO is new Ada.Text_IO.Modular_IO (Pixel_Indices);

   type Enhancement_Arrays is array (Pixel_Indices) of Boolean;

   subtype Coordinates is Integer;

   type Points is record
      X, Y : Coordinates;
   end record; -- Points

   function "<" (Left, Right : Points) return Boolean is

   begin -- "<"
      return Left.Y < Right.Y or (Left.Y = Right.Y and Left.X < Right.X);
   end "<";

   function "=" (Left, Right : Points) return Boolean is

   begin -- "="
      return Left.X = Right.X and Left.Y = Right.Y;
   end "=";

   package Frame_Buffers is new
     Ada.Containers.Ordered_Maps (Points, Boolean);
   use Frame_Buffers;

   type Images is record
      X1, X2, Y1, Y2 : Coordinates := 0;
      Frame_Buffer : Frame_Buffers.Map;
   end record;
   procedure Get_Input (Enhancement_Array : out Enhancement_Arrays;
                       Image : Out Images) is

      -- december_20 [Input_File_Name]

      Input_File : File_Type;
      Text : Unbounded_String;
      Point : Points;

   begin -- Get_Input
      if Argument_Count = 0 then
         Open (Input_File, In_File, "december_20.txt");
      else
         Open (Input_File, In_File, Argument(1));
      end if; -- Argument_Count = 0
      Get_Line (Input_File, Text);
      Assert (Length (Text) = Positive (Pixel_Indices'Last + 1),
              "Wrong size enhancement data, expected" &
                Pixel_Indices'Image (Pixel_Indices'Last + 1) & " and read" &
                Positive'Image (Length (Text)));
      for I in Pixel_Indices loop
         Enhancement_Array (I) := Lit_Pixel = Element (Text, Positive (I + 1));
      end loop; -- I in Pixel_Indices
      Skip_Line (Input_File);
      Point.Y := 0;
      while not End_Of_File (Input_File) loop
         Get_Line (Input_File, Text);
         if Length (Text) > Image.X2 then
            Image.X2 := Length (Text);
         end if; -- Length (Text) > X2
         for I in Positive range 1 .. Length (Text) loop
            Point.X := I - 1;
            Include (Image.Frame_Buffer, Point, Lit_Pixel = Element (Text, I));
         end loop; -- I in Positive range 1 .. Length (Text)
         Point.Y := Point.Y + 1;
      end loop; -- not End_Of_File (Input_File)
      Close (Input_File);
   end Get_Input;

   procedure Create_Boarder (Image : in Out Images;
                             Boarder : in Coordinates) is

   begin -- Create_Boarder
      Image.X1 := Coordinates'Last;
      Image.Y1 := Coordinates'Last;
      Image.X2 := Coordinates'First;
      Image.Y2 := Coordinates'First;
      for P in Iterate (Image.Frame_Buffer) loop
         if Image.Frame_Buffer (P) then
            If Key (P).X < Image.X1 then
               Image.X1 := Key (P).X;
            end if; -- Key (P).X < Image.X1
            If Key (P).Y < Image.Y1 then
               Image.Y1 := Key (P).Y;
            end if; -- Key (P).Y < Image.X1
            if Key (P).X > Image.X2 then
               Image.X2 := Key (P).X;
            end if; -- Key (P).X > Image.X2
            if Key (P).Y > Image.Y2 then
               Image.Y2 := Key (P).Y;
            end if; -- Key (P).Y > Image.Y2
         end if; -- Image.Frame_Buffer (P)
      end loop; -- P in Iterate (Image.Frame_Buffer)
      Image.X1 := Image.X1 - Boarder;
      Image.Y1 := Image.Y1 - Boarder;
      Image.X2 := Image.X2 + Boarder;
      Image.Y2 := Image.Y2 + Boarder;
   end; -- Create_Boarder

   procedure Enhance (Enhancement_Array : in Enhancement_Arrays;
                      Image : in Images;
                      Enhanced_Image : out Images) is

      procedure Set_Bit (X, Y : in Coordinates;
                         Bit : in Bits;
                         Image : in Images;
                         Pixel_Index : in out Pixel_Indices) is

         Bit_Mask : constant Pixel_Indices := 1;
         Point : constant Points := (X, Y);

      begin -- Set_Bit
         if Contains (Image.Frame_Buffer, (Point)) and then
           Image.Frame_Buffer (Point) then
            Pixel_Index := Pixel_Index or Shift_Left (Bit_Mask, Bit);
         end if; -- Contains (Image.Frame_Buffer (Point)) and then ...
      end Set_Bit;

      Pixel_Index : Pixel_Indices;

   begin -- Enhance
      Enhanced_Image.X1 := Image.X1;
      Enhanced_Image.Y1 := Image.Y1;
      Enhanced_Image.X2 := Image.X2;
      Enhanced_Image.Y2 := Image.Y2;
      Enhanced_Image.Frame_Buffer := Frame_Buffers.Empty_Map;
      for Y in Coordinates range Enhanced_Image.Y1 .. Enhanced_Image.Y2 loop
         for X in Coordinates range Enhanced_Image.X1 .. Enhanced_Image.X2 loop
            Pixel_index := 0;
            Set_Bit ( X - 1, Y - 1, 8, Image, Pixel_Index);
            Set_Bit ( X,     Y - 1, 7, Image, Pixel_Index);
            Set_Bit ( X + 1, Y - 1, 6, Image, Pixel_Index);
            Set_Bit ( X - 1, Y,     5, Image, Pixel_Index);
            Set_Bit ( X,     Y,     4, Image, Pixel_Index);
            Set_Bit ( X + 1, Y,     3, Image, Pixel_Index);
            Set_Bit ( X - 1, Y + 1, 2, Image, Pixel_Index);
            Set_Bit ( X,     Y + 1, 1, Image, Pixel_Index);
            Set_Bit ( X + 1, Y + 1, 0, Image, Pixel_Index);
            Include (Enhanced_Image.Frame_Buffer, (X, Y),
                     Enhancement_Array (Pixel_Index));
         end loop; -- X in Coordinates range Image.X1 .. Image.X2
      end loop; -- Y in Coordinates range Imaeg.Y1 .. Image.Y2
   end Enhance;

   procedure Clear_Edges (Image : in out Images) is

   begin -- Clear_Edges
   for P in Iterate (Image.Frame_Buffer) loop
      if Key (P).X = Image.X1 or Key (P).X = Image.X2 or
        Key (P).Y = Image.Y1 or Key (P).Y = Image.Y2 then
         Image.Frame_Buffer (P) := False;
      end if; -- Key (P).X = Image.X1 or Key (P).X = ...
   end loop; -- P in Iterate (Image.Frame_Buffer)
   end Clear_Edges;

   function Count_Pixels (Image : in Images) return Natural is

      Result : Natural := 0;

   begin -- Count_Pixels
      for P in Iterate (Image.Frame_Buffer) loop
         if Image.Frame_Buffer (P) then
            Result := Result + 1;
         end if; -- Key (P).X = Image.X1 or Key (P).X = ...
      end loop; -- P in Iterate (Image.Frame_Buffer)
      return Result;
   end Count_Pixels;

   Enhancement_Array : Enhancement_Arrays;
   Input_Image, Image, Enhanced_Image : Images;

begin -- December_20
   Get_Input (Enhancement_Array, Input_Image);
   Image := Input_Image;
   Create_Boarder (Image, Part_One_Iterations + 1);
   for I in positive range 1 .. Part_One_Iterations loop
      Enhance (Enhancement_Array, Image, Enhanced_Image);
      if I mod 2 = 0 then
         Clear_Edges (Enhanced_Image);
      end if; -- I mod 2 = 0
      Image := Enhanced_Image;
   end loop; -- I in positive range 1 .. Part_One_Iterations
   Put_Line ("Part One Answer:" &
               Count_Pixels (Enhanced_Image)'Img);
   Image := Input_Image;
   Create_Boarder (Image, Part_Two_Iterations + 1);
   for I in positive range 1 .. Part_Two_Iterations loop
      Enhance (Enhancement_Array, Image, Enhanced_Image);
      if I mod 2 = 0 then
         Clear_Edges (Enhanced_Image);
      end if; --  I mod 2 = 0
      Image := Enhanced_Image;
   end loop; -- I in positive range 1 .. Part_Two_Iterations
   Put_Line ("Part Two Answer:" &
               Count_Pixels (Enhanced_Image)'Img);
   DJH.Execution_Time.Put_CPU_Time;
end December_20;
