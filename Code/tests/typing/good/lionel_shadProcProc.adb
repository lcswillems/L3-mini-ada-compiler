with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   procedure P is
      procedure p (x : Character) is -- p cache l'ancienne procédure
      begin Put (x) ; end;
   begin
      p('&');
   end;
begin
   P;
end;
