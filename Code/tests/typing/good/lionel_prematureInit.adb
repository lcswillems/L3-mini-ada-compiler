with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   function f return integer is 
	a:integer := f; -- gnat l'accepte... Mais c'est douteux, non ?
	begin return a; end;
begin
   new_line;
end;
