with Ada.Text_IO; use Ada.Text_IO;
-- un appel de fonction renvoyant un type record n'est pas une valeur gauche.
procedure type_fonction_return is
   type r;
   type r is record x : boolean; end record;
   b : r;
   
   function f (a : In Out r) return r is
   	begin
   	return a;
        end;

begin new_line; f(b).x := true; End;
