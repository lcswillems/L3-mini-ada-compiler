with Ada.Text_IO; use Ada.Text_IO;
procedure Test is
   function quatre return character is
	x : character := 'd';
	begin return x; end;
begin put (quatre); end; -- quatre est une fonction qui renvoie un character
