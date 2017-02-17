with Ada.Text_IO; use Ada.Text_IO;

procedure type_for is
   type r;
   type r is record a : boolean; end record;
   x : r;
-- on se fiche du type passé de la variable de la boucle for
begin for x in 1 .. 2 loop new_line; end loop; End;
