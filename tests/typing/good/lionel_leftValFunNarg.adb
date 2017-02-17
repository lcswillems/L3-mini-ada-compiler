with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   type r is record x:access r; end record;
   function f return access r is 
	a:r;
	begin return a.x; end;
begin
   f.x := null;
end;
