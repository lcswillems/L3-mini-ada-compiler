-- Les variables peuvent avoir des noms de type
-- On peut renommer les types primitifs

with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   type Integer is record Integer : Character; end record;
   Character : Integer;
     
   procedure Aux(X : Integer) is
   begin
      Put(X.Integer);
   end;
     
begin
   Character.Integer := 'a';
   Aux(Character);
end;
