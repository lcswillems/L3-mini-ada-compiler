with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   procedure P is
      procedure X (p : integer ; x : Character) is -- x et p cachent les procédures
      begin Put(cHaRaCtEr'val(p)); Put (x) ; end;
   begin
      X(0,'&');
   end;
begin
   P;
end;
