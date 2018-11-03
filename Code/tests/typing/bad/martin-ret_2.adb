with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   function f (x : integer) return integer is 
   begin if x=4 then return 0; 
   else New_Line; End if; -- Une branche else mais pas de return
   end;
begin
   new_line;
end;
