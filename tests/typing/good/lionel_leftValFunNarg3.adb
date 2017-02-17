with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   type u is record z:integer ; end record;
   type t is record y:access u; end record;
   type r is record x:t       ; end record;
   function f return t is 
	a:r;
	begin return a.x; end;
begin
   f.y.z := 3;
end;
