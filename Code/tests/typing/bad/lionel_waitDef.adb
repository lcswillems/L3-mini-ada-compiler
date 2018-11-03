with Ada.Text_IO; use Ada.Text_IO;
procedure Test is
   type a;
   type b;
   type a is record x: b ; y:integer; end record; -- b n'est pas encore défini
   type b is access a;
begin new_line; end;
