with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   type t is record z:integer; end record;
   type r is record x:access r; y:t; end record;
   function f (a:r) return access r is 
	begin return a.x; end;
   b:r;
begin
   f(b).y.z := 3;
end;
