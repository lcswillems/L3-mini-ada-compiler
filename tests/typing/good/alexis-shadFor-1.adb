with Ada.Text_IO; use Ada.Text_IO;

procedure ShadFor is
   X : Integer := 12;
begin
   for X in 3..7 loop
      if X >= 10 then
	 Put('N');
      end if;
   end loop;
end ShadFor;
