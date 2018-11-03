with Ada.Text_IO; use Ada.Text_IO;

procedure P is
   type r is record x:integer; end record;
   type t is access r;
   function f (t : access r) return integer is -- shadow de t
      begin return t.x; end; -- t est maintenant une variable
begin
   New_Line;
end;
