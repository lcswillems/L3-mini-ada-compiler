with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   type r is record int : integer; end record;
   procedure P is
      procedure r is
         x : r; -- r est une procédure et non plus un type
         begin new_line ; end;
   begin
      new_line;
   end;
begin
   P;
end;
