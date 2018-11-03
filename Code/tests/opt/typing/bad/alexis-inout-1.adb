with Ada.Text_IO;
use Ada.Text_IO;

procedure Horrible is
  procedure double(A : in out integer ; B : in out integer) is
    begin
      B := B+1;
      put(character'val(A));
      -- A := A+1;
    end double;
    V : integer := 42;
  begin
    double(V,V); -- Devrait émettre une erreur ici, selon gnatmake
                 -- Pour éviter une ambiguite dans l'ordre possible d'opérations cachées dans des fonctions, ce qui ne doit en pratique pas arriver souvent.
  end horrible;
