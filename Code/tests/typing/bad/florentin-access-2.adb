-- gnat dit : access type cannot designate itself

with Ada.Text_IO; use Ada.Text_IO;

procedure P is
   type R;
   type R is access R;
begin
   New_Line;
end;
