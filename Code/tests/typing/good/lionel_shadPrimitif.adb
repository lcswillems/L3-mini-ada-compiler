with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   integer : character := 'i'; -- integer est maintenant une variable
   type character; -- le character primitif est caché
   type boolean is access character; -- boolean est maintenant un pointeur
   -- on ne peut plus du tout accéder aux types primitifs désormais
   type character is record boolChar : boolean; end record;
   c : character;
   b : boolean;
begin
   put(integer);
   b:=null;
   c.boolChar := b;
end;
