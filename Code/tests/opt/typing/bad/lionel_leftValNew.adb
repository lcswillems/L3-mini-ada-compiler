with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   type r is record x:integer; end record;
   y : integer := (new r).x;
begin
   new_line;
end;
