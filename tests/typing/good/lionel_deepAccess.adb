with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   type r is record x : access r; end record;
   function f(a:access r; b:r) return r is
	begin
	   a.x := b.x;
	   b.x.x := a; 
	-- donc b.x.x.x = b.x et a.x.x = a
	   return b;
	end;
   c : access r;
   d : r;
begin
   d := f(c  , d);
   d := f(d.x, d);
end;
