with Ada.Text_IO; use Ada.Text_IO;

procedure Shad is
   procedure T1 is
      X : Integer := 29;
   begin
      X := X+3;
      for X in 1..10 loop
	 X := X+1;
	 Put('a');
      end loop;
      Put(Character'Val(X));
   end;
begin
   T1;
end;
