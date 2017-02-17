with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   type r is record x : access r; end record;
   a : access r := null;
begin
   (a).x := a; -- bon au sens des valeurs gauches de Mini-Ada, refusé par gnat
end;
