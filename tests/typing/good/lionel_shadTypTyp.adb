with Ada.Text_IO; use Ada.Text_IO;
procedure Test is
   type R is record T:integer; end record;
   procedure Ok is
	type R is record T: access R; end record; -- construction autorisée, le r d'avant a complètement disparu
	L:R;
	begin L.T := null; end; -- L.T est un access r et non pas un integer
begin new_line; end;
