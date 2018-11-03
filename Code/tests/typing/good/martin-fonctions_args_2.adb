with Ada.Text_IO; use Ada.Text_IO;
-- Le nom de la fonction ne shadow qu'apres la declaration du type du return
procedure fonctions_args_2 is
	type r is record x : integer; end record;

	procedure g is
        function r (a: r) return r is
   			begin
			return a;
			end;
	begin new_line; end;

begin New_Line; End;
