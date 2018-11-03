with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   type r is record x:integer; end record;
   function f return r is
	t:r;
	begin return t; end;
   y : integer := f.x;
begin
   new_line;
end;
