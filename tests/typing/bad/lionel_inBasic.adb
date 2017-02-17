with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   type t;
   type r is record x : access t  ; end record;
   type t is record y : r         ; end record;
   type u is record z : t ; t : r ; end record;
   function f(a: u) return u is
	begin
	   a.z.y   := a.t;
	   a.t.x.y := a.t;
	   return a;
	end;
   b : u;
begin
   b := f(b);
end;
