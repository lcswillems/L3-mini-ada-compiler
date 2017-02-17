-- Une variable shadow son initialisation
-- Ici gnat dit que X ne peut dépendre de sa propre déclaration

with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   X : Integer := 0;
   procedure P is
      X : Integer := X + 1;
   begin
      New_Line;
   end;
begin
   New_Line;
end;
