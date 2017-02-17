with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   type u is record z:integer ; end record;
   type t is record y:access u; end record;
   type r is record x:t       ; end record;
   function f (a:r) return t is 
	begin return a.x; end;
   b:r;
begin
   f(b).y.z := 3;
end;
