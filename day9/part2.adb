with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Containers.Vectors;

procedure part2 is
   use Ada.Text_IO;
   use Ada.Strings.Fixed;

   type Long_Int is range -2**63 .. 2**63 - 1;
   
   package Long_IO is new Ada.Text_IO.Integer_IO (Long_Int);
   use Long_IO;

   type Point is record
      X, Y : Long_Int;
   end record;

   type Edge is record
      P1, P2 : Point;
      Is_Vertical : Boolean;
      Min_C, Max_C : Long_Int; 
      Fixed_C : Long_Int;      
   end record;

   package Point_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Point);

   package Edge_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => Edge);

   function lte (Left, Right : Point) return Boolean is
   begin
      if Left.X /= Right.X then
         return Left.X < Right.X;
      else
         return Left.Y < Right.Y;
      end if;
   end lte;

   package Sorter is new Point_Vectors.Generic_Sorting (lte);

   Poly_Points : Point_Vectors.Vector;
   Candidates  : Point_Vectors.Vector;
   Poly_Edges  : Edge_Vectors.Vector;
   
   File           : File_Type;
   Buffer         : String (1 .. 1024);
   Length         : Natural;
   Idx            : Natural;
   Current        : Point;
   Max_Area       : Long_Int := 0;
   
   Max_X          : Long_Int := Long_Int'First;
   Max_Y          : Long_Int := Long_Int'First;
   Min_Y          : Long_Int := Long_Int'Last;

   function Make_Edge (P_A, P_B : Point) return Edge is
      E : Edge;
   begin
      E.P1 := P_A;
      E.P2 := P_B;
      if P_A.X = P_B.X then
         E.Is_Vertical := True;
         E.Fixed_C := P_A.X;
         E.Min_C := Long_Int'Min (P_A.Y, P_B.Y);
         E.Max_C := Long_Int'Max (P_A.Y, P_B.Y);
      else
         E.Is_Vertical := False;
         E.Fixed_C := P_A.Y;
         E.Min_C := Long_Int'Min (P_A.X, P_B.X);
         E.Max_C := Long_Int'Max (P_A.X, P_B.X);
      end if;
      return E;
   end Make_Edge;

   function Is_Valid_Rect (R1, R2 : Point) return Boolean is
      Rx_Min, Rx_Max, Ry_Min, Ry_Max : Long_Int;
      Cx, Cy : Long_Float; 
      Intersections : Integer := 0;
      E : Edge;
   begin
      Rx_Min := Long_Int'Min (R1.X, R2.X);
      Rx_Max := Long_Int'Max (R1.X, R2.X);
      Ry_Min := Long_Int'Min (R1.Y, R2.Y);
      Ry_Max := Long_Int'Max (R1.Y, R2.Y);

      for K in Edge_Vectors.First_Index (Poly_Edges) .. Edge_Vectors.Last_Index (Poly_Edges) loop
         E := Edge_Vectors.Element (Poly_Edges, K);
         if E.Is_Vertical then
            if E.Fixed_C > Rx_Min and then E.Fixed_C < Rx_Max then
               if Long_Int'Max (Ry_Min, E.Min_C) < Long_Int'Min (Ry_Max, E.Max_C) then
                  return False;
               end if;
            end if;
         else
            if E.Fixed_C > Ry_Min and then E.Fixed_C < Ry_Max then
               if Long_Int'Max (Rx_Min, E.Min_C) < Long_Int'Min (Rx_Max, E.Max_C) then
                  return False;
               end if;
            end if;
         end if;
      end loop;

      Cx := Long_Float (Rx_Min + Rx_Max) / 2.0;
      Cy := Long_Float (Ry_Min + Ry_Max) / 2.0;

      for K in Edge_Vectors.First_Index (Poly_Edges) .. Edge_Vectors.Last_Index (Poly_Edges) loop
         E := Edge_Vectors.Element (Poly_Edges, K);
         
         if E.Is_Vertical then
            if abs (Long_Float (E.Fixed_C) - Cx) < 0.001 and then
               Cy >= Long_Float (E.Min_C) and then Cy <= Long_Float (E.Max_C) then
               return True; 
            end if;
         else
            if abs (Long_Float (E.Fixed_C) - Cy) < 0.001 and then
               Cx >= Long_Float (E.Min_C) and then Cx <= Long_Float (E.Max_C) then
               return True;
            end if;
         end if;

         if E.Is_Vertical then
            if Long_Float (E.Fixed_C) > Cx then
               if Long_Float (E.Min_C) <= Cy and then Long_Float (E.Max_C) > Cy then
                  Intersections := Intersections + 1;
               end if;
            end if;
         end if;
      end loop;

      return (Intersections mod 2 /= 0);
   end Is_Valid_Rect;

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
               Point_Vectors.Append (Poly_Points, Current);
               Point_Vectors.Append (Candidates, Current);
               
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

   for I in Point_Vectors.First_Index (Poly_Points) .. Point_Vectors.Last_Index (Poly_Points) - 1 loop
      Edge_Vectors.Append (Poly_Edges, Make_Edge (Point_Vectors.Element (Poly_Points, I), Point_Vectors.Element (Poly_Points, I + 1)));
   end loop;
   Edge_Vectors.Append (Poly_Edges, Make_Edge (Point_Vectors.Last_Element (Poly_Points), Point_Vectors.First_Element (Poly_Points)));

   Sorter.Sort (Candidates);

   for I in Point_Vectors.First_Index (Candidates) .. Point_Vectors.Last_Index (Candidates) loop
      declare
         P1 : Point := Point_Vectors.Element (Candidates, I);
         Max_Possible_W, Max_Possible_H : Long_Int;
         W, H, Temp : Long_Int;
      begin
         Max_Possible_W := abs (Max_X - P1.X) + 1;
         Max_Possible_H := abs (Max_Y - Min_Y) + 1; 

         if (Max_Possible_W * Max_Possible_H) > Max_Area then
            for J in I + 1 .. Point_Vectors.Last_Index (Candidates) loop
               declare
                  P2 : Point := Point_Vectors.Element (Candidates, J);
               begin
                  W := abs (P1.X - P2.X) + 1;
                  H := abs (P1.Y - P2.Y) + 1;
                  Temp := W * H;

                  if Temp > Max_Area then
                     if Is_Valid_Rect (P1, P2) then
                        Max_Area := Temp;
                     end if;
                  end if;
               end;
            end loop;
         end if;
      end;
   end loop;

   Put (Max_Area, Width => 1);
   New_Line;
end part2;
