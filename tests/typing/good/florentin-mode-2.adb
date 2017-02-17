-- On peut shadow une variable in, que l'on peut donc modifier

with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   X : Integer := 0;
   procedure P (X : in Integer) is
      procedure Triche is
         X : Integer := 0;
      begin
         X := 2;
      end;
   begin
      Triche;
   end;
begin
   P(X);
end;
