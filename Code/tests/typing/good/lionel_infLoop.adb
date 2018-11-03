with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   procedure P is
      procedure X is begin New_line ; X ; end; -- à l'exécution ça ne sera pas joli...
   begin
      X;
   end;
begin
   P;
end;
