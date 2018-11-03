with Ada.Text_IO; use Ada.Text_IO;

procedure P is
   type r is record x:integer; end record;
   type t is access r;
   function f (r : t) return integer is -- shadow de l'ancien r
      begin return r.x; end; -- l'ancien r reste accessible grâce au pointeur
begin
   New_Line;
end;
