with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Simple_Calculator is
   Expr :String (1 .. 100);
   A, B, Result :Integer;
   Oprtr :Character;
   Last :Natural;  -- holds string's last position
   Index :Integer :=1;
   Temp_Str :String (1 .. 10);
   Temp_Index :Integer;
begin
   Put_Line ("Enter expression: ");
   Get_Line (Expr, Last);
   
   -- parsing first part
   Temp_Index :=1;
   while Expr(Temp_Index) in '0'..'9' loop
      Temp_Str(Temp_Index) :=Expr(Temp_Index);
      Temp_Index :=Temp_Index+ 1;
   end loop;
   
   A := Integer'Value (Temp_Str(1 .. Temp_Index - 1));
   
   -- parsing rest of the expression
   Index :=Temp_Index;
   
   loop
      Oprtr :=Expr(Index);
      Index :=Index+1;
      Temp_Index :=1;
      
      while Expr(Index) in '0'..'9' loop
         Temp_Str(Temp_Index) :=Expr(Index);
         Temp_Index :=Temp_Index+ 1;
         Index :=Index+ 1;
      end loop;
      
      B := Integer'Value (Temp_Str(1 .. Temp_Index - 1));
      
      case Oprtr is
         when '+' => Result :=A+B;
         when '-' => Result :=A-B;
         when '*' => Result :=A*B;
         when '/' =>
            if B/=0 then
               Result :=A/B;
            else
               Put_Line ("Division by zero!");
               return;
            end if;
         when others =>
            Put_Line ("Invalid operator");
            return;
      end case;
      
      A :=Result;  -- update the result for the next iteration
      exit when Index>Last;  -- end when no more operators
   end loop;
   
   Put_Line ("Result: " & Integer'Image(Result));
end Simple_Calculator;
