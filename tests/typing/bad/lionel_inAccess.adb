with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   type r is record x : access r; end record;
   function f(a:access r; b:r) return r is
	begin
	   b.x := a;
	   return b;
	end;
begin
   new_line;
end;
