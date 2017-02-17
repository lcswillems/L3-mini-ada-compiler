with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   type t is record z: integer ; end record;
   type r is record x: access t; end record;
   function f return r is 
	f:r;
	begin return f; end;
begin
   f.x.z := 3;
end;
