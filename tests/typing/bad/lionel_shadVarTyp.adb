with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   character : integer := 3;
   procedure P is
      procedure X (y : Character) is begin Put (y) ; end; -- character est une variable et non plus un type
   begin
      X('a');
   end;
begin
   P;
end;
