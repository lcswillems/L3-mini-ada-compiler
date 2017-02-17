with Ada.Text_IO; use Ada.Text_IO;

procedure Test3 is
   V : Integer := 3;
   U : Integer := 39;
begin
   if (U+V) then
      Put('A');
   else
      Put('B');
   end if;
end Test3;
