-- types mutuellement récursifs

with Ada.Text_IO; use Ada.Text_IO;
procedure Test is
   type b;
   type a is record x:access b ; y: access a; end record;
   type b is record x:a; end record;
begin new_line; end;
