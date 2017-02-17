with Ada.Text_IO;
use Ada.Text_IO;

procedure Shadowing is
   procedure P is
   begin
      Put('a');
   end;
   function P return Integer is
   begin
      Put('b');
      return 42;
   end;
   V : Integer;
begin
   P;
   V := P;
end;
