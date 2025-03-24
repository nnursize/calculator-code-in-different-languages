with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Simple_Calculator is
   Expression : String (1 .. 100);
   A, B, Result : Integer;
   Op : Character;
   Last : Natural;  -- Last position of the string
   Index : Integer := 1;
   Temp_Str : String (1 .. 10);
   Temp_Index : Integer;
begin
   Put_Line ("Enter expression (e.g., 3+5+8): ");
   Get_Line (Expression, Last);
   
   -- Parse the first operand
   Temp_Index := 1;
   while Expression(Temp_Index) in '0'..'9' loop
      Temp_Str(Temp_Index) := Expression(Temp_Index);
      Temp_Index := Temp_Index + 1;
   end loop;
   A := Integer'Value (Temp_Str(1 .. Temp_Index - 1));
   
   -- Start parsing the rest of the expression
   Index := Temp_Index;
   
   loop
      -- Get operator
      Op := Expression(Index);
      Index := Index + 1;
      
      -- Get next operand
      Temp_Index := 1;
      while Expression(Index) in '0'..'9' loop
         Temp_Str(Temp_Index) := Expression(Index);
         Temp_Index := Temp_Index + 1;
         Index := Index + 1;
      end loop;
      B := Integer'Value (Temp_Str(1 .. Temp_Index - 1));
      
      -- Perform operation
      case Op is
         when '+' => Result := A + B;
         when '-' => Result := A - B;
         when '*' => Result := A * B;
         when '/' =>
            if B /= 0 then
               Result := A / B;
            else
               Put_Line ("Error: Division by zero!");
               return;
            end if;
         when others =>
            Put_Line ("Invalid operator!");
            return;
      end case;
      
      A := Result;  -- Update the result for the next iteration
      exit when Index > Last;  -- End when no more operators
   end loop;
   
   Put_Line ("Result: " & Integer'Image(Result));
end Simple_Calculator;
