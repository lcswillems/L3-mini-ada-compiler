with Ada.Text_IO; use Ada.Text_IO;

procedure Test is
   r : integer := 3;
   function f return boolean is
      
      type R is record bool : boolean; end record; -- shadow du r=3
      type T is access R;
      x : integer := 0;
      procedure f (i : In Out integer) is --shadow de f (fonction)
         x : T := null; --shadow de x=0
         function f(r : In T ; t : In Out integer) return character is 
         -- shadow de f (procédure), de R, puis de T
         begin r.bool := true; t:=0; return 'a'; end;
         -- on en profite pour vérifier qu'on peut modifier en place
         -- le déréférencement d'un pointeur passé en In :-) 
      begin
         Put( f(x,i) ); -- Passage du In Out de i
      end;

   begin
      for t in 0 .. 4 loop f(x); end loop; -- shadow de t
      return true;
   end;

begin
   if f then put('v'); else put('f'); end if;
end;
