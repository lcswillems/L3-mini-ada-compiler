with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   type r is record x : access r; end record;
   function f(a:access r) return access r is
	begin 
	   a.x := a;
	   return a; 
	end;
   b : access r;
begin
   b := f(b); -- et hop, un pointeur sur soi-même :)
end;
