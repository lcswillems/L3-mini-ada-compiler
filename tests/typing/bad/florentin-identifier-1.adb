-- Un identifier shadow un type (et vice-versa)

with Ada.Text_IO; use Ada.Text_IO;

procedure P is
   type T is record A: Integer; end record;
   type R is record B: Integer; end record;
   
   procedure Aux is
      T : R;
      R : T;
   begin
      New_Line;
   end;
begin
   New_Line;
end;
