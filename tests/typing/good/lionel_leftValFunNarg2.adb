with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   type t is record z:integer; end record;
   type r is record x:access r; y:t; end record;
   function f return access r is 
	a:r;
	begin return a.x; end;
begin
   f.y.z := 3;
end;
