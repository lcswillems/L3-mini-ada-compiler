with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   function f (x : integer) return integer is 
   begin if x=x then return 0; end if;
-- pas de else avec un return
   end;
begin
   new_line;
end;
