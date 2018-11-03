with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   type t;
   type r is record x : access t; end record;
   type t is record y : r       ; end record;
   function f(a:t) return r is
	begin
	   return a.y.x.y;
	end;
   d : t;
begin
   f(d) := d.y;
end;
