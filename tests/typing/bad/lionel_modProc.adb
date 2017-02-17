with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   procedure P is
      procedure X is begin New_line ; X:=3 ; end; -- x est une procédure, donc non modifiable
   begin
      X;
   end;
begin
   P;
end;
