with Ada.Text_IO; use Ada.Text_IO;
procedure Test is
   type a;
   type b;
   L : a; -- a n'est pas encore défini
   type a is record x: access b ; y:integer; end record;
   type b is access a;
begin new_line; end;
