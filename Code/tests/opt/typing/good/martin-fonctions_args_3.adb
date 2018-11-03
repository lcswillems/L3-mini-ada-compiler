with Ada.Text_IO; use Ada.Text_IO;
-- La fonction est shadow par ses arguments, et non l'inverse
procedure fonctions_args_3 is
   	function f (f: integer) return integer is
   	begin return f;	end;

begin New_Line; End;
