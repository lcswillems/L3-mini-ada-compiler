with Ada.Text_IO; use Ada.Text_IO;

procedure types is
   type r is record a : boolean; end record;
   
   procedure f is
        -- On ne peut faire cela directement, car on essaie de faire un acces au nouveau r defini qui n'est plus un record déclare.
   	type r is access r;

   	begin
   	new_line; 
    end;

begin New_Line; End;
