with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   function f (x : integer) return integer is 
      Y : Integer;
   begin 
      while Y < x
      loop return 42; end loop;
      -- idem un return manque, on ne peut etre sur de rentrer dans la boucle !
   end;
begin
   new_line;
end;
