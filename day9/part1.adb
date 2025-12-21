with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Containers.Vectors;

procedure part1 is
   use Ada.Text_IO;
   use Ada.Strings.Fixed;

   type Long_Int is range -2**63 .. 2**63 - 1;
   
   package Long_IO is new Ada.Text_IO.Integer_IO (Long_Int);
   use Long_IO;

   type Point is record
      X, Y : Long_Int;
   end record;

   package Point_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Point);
   use Point_Vectors;

   function lte (Left, Right : Point) return Boolean is
   begin
      if Left.X /= Right.X then
         return Left.X < Right.X;
      else
         return Left.Y < Right.Y;
      end if;
   end lte;

   package Sorter is new Point_Vectors.Generic_Sorting (lte);

   File           : File_Type;
   Buffer         : String (1 .. 1024);
   Length         : Natural;
   Idx            : Natural;
   List           : Vector;
   Current        : Point;
   Max_Area       : Long_Int := 0;
   W, H           : Long_Int;
   Temp           : Long_Int;
   Max_X          : Long_Int := -2**63;
   Max_Y          : Long_Int := -2**63;
   Min_Y          : Long_Int := 2**63 - 1;
   Max_Possible_W : Long_Int;
   Max_Possible_H : Long_Int;

begin
   Open (File, In_File, "../inputs/day9.txt");
   while not End_Of_File (File) loop
      Get_Line (File, Buffer, Length);
      if Length > 0 then
         Idx := Index (Buffer (1 .. Length), ",");
         if Idx > 0 then
            begin
               Current.X := Long_Int'Value (Buffer (1 .. Idx - 1));
               Current.Y := Long_Int'Value (Buffer (Idx + 1 .. Length));
               List.Append (Current);
               
               if Current.X > Max_X then Max_X := Current.X; end if;
               if Current.Y > Max_Y then Max_Y := Current.Y; end if;
               if Current.Y < Min_Y then Min_Y := Current.Y; end if;
            exception
               when others => null;
            end;
         end if;
      end if;
   end loop;
   Close (File);

   Sorter.Sort (List);

   for I in List.First_Index .. List.Last_Index loop
      declare
         P1 : Point := List (I);
      begin
         Max_Possible_W := abs (Max_X - P1.X) + 1;
         Max_Possible_H := abs (Max_Y - Min_Y) + 1; 

         if (Max_Possible_W * Max_Possible_H) > Max_Area then
            for J in I + 1 .. List.Last_Index loop
               declare
                  P2 : Point := List (J);
               begin
                  W := abs (P1.X - P2.X) + 1;
                  H := abs (P1.Y - P2.Y) + 1;
                  Temp := W * H;

                  if Temp > Max_Area then
                     Max_Area := Temp;
                  end if;
               end;
            end loop;
         end if;
      end;
   end loop;

   Put (Max_Area, Width => 1);
   New_Line;
end part1;
