with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   procedure X (y : integer) is 
      x : character := 'a'; -- x cache la procédure
   begin Put (x) ; end;
begin
   X(3);
end;
