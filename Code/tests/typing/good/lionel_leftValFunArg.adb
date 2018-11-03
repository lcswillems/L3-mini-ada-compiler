with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   type r is record x:access r; end record;
   function f (a:r) return access r is 
	begin return a.x; end;
   y:r;
begin
   f(y).x := null;
end;
