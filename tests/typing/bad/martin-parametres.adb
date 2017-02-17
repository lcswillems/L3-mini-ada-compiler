with Ada.Text_IO; use Ada.Text_IO;
-- pas deux fois le meme argument
procedure parametres is
   procedure f (x : integer; x : integer) is
   begin new_line; end;
begin  new_line; End;
