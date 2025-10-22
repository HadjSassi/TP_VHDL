------------------------------
-- entree-sortie sur fichiers
------------------------------

library ieee;
use	ieee.std_logic_1164.all;
use 	ieee.std_logic_arith.all;
use 	ieee.std_logic_unsigned.all;
use	std.TEXTIO.all;

--------------------------------------------

package entree_sortie_pkg is

type IMAGE2D is array (natural range <>, natural range <>) of integer;
type MEM_FIFO_LIGNE is array (natural range <>) of std_logic_vector (7 downto 0);


function get_pixels(entree : string; N_col : integer; N_ligne : integer) return  IMAGE2D;

procedure put_pixels(sortie : string; IM2D : IMAGE2D; n_pix_x : integer; n_pix_y : integer; n_bit_pixel : integer);

end entree_sortie_pkg;

---------------------------------------------

package body entree_sortie_pkg is

function get_pixels(entree : string; N_col : integer; N_ligne : integer) return  IMAGE2D is
variable ligne : LINE;
file FI        : TEXT is in entree;
variable IM2D  : IMAGE2D(1 to N_ligne, 1 to N_col);
variable I     : natural:=0;
begin
-- enlever l'entête
READLINE(FI,ligne);
READLINE(FI,ligne);  
READLINE(FI,ligne);
READLINE(FI,ligne); 
READLINE(FI,ligne);

loop
	if not ENDFILE(FI) then
		READLINE(FI,ligne);
		I := I + 1;
		
		else return IM2D;
	
	end if;
	
	for j in 1 to N_col loop
		READ(ligne, IM2D(I,J));
	end loop;
	
end loop;
end get_pixels;
------------------------------------------------
procedure put_pixels(sortie : string; IM2D : IMAGE2D; n_pix_x : integer; n_pix_y : integer; n_bit_pixel : integer)  is
variable ligne : LINE;
file FO        : TEXT is out sortie;
begin
WRITE(LIGNE, STRING'("Type=GDR 2.0"));WRITELINE(FO,LIGNE);
	WRITE(LIGNE,  STRING'("Nb.Pixel/Ligne="));WRITE(LIGNE,n_pix_x);WRITELINE(FO,LIGNE);	
	WRITE(LIGNE,  STRING'("Nb.Lignes="));WRITE(LIGNE,n_pix_x);WRITELINE(FO,LIGNE);
	WRITE(LIGNE,  STRING'("Nb.Bits="));WRITE(LIGNE,n_bit_pixel);WRITELINE(FO,LIGNE);
	WRITE(LIGNE, STRING'(" "));WRITELINE(FO,LIGNE);		
		 
 for I in 1 to n_pix_y loop	
	for J in 1 to n_pix_x loop
 		WRITE(LIGNE, IM2D(I,J));
		WRITE(LIGNE, STRING'(" "));	-- Separateur
	end loop;
	WRITELINE(FO,LIGNE);
 end loop;
end put_pixels;

end entree_sortie_pkg;

