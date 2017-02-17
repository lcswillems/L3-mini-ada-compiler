-- Selon gnat, X et Y n'ont pas le même type...

with Ada.Text_IO; use Ada.Text_IO;

procedure P is
   type T is record A: Integer; end record;
   type A is access T;
   type B is access T;
   X : A := new T;
   Y : B := new T;
   R : Boolean;
begin
   R := (X = Y);
end;
