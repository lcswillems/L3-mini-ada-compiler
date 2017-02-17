with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   function f (x : integer) return integer is 
   begin 
      begin New_Line; return 27; end; -- petit bloc d'instructions gentil
   end;
begin
   new_line;
end;
