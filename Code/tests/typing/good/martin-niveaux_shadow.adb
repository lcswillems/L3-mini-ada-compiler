with Ada.Text_IO; use Ada.Text_IO;

procedure niveaux_shadow is
   type r;
   type r is record a : boolean; end record;
   
   procedure f is
   	type x is access r;
   	y : x := new r;
        type r is record b : integer; end record;

   	begin
-- y fait reference a un type r shadow a ce niveau
	y.a := true; 
    end;

begin New_Line; End;
