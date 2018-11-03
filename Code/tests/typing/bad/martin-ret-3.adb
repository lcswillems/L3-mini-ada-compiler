with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   function f (x : integer) return integer is 
   begin for I in 1 .. x
   loop return 42; end loop;
   -- idem un return manque, on ne peut etre sur de rentrer dans la boucle !
   end;
begin
   new_line;
end;
