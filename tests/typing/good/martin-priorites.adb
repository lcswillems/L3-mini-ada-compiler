with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   X : Integer := 42;
   Y : Integer := 37;
   type R is record X : Integer; end record;
   Z : R;
   A : Boolean := true;
   B : Boolean := false;
begin
   A := X + Y = 4;
   B := Y > -Z.X + 4;
   A := Y * X = 27 or Z.X / 29 < Y rem X * 4;
   A := B and then not (4 = x);
end;
