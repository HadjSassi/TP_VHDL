library ieee;
use ieee.std_logic_1164.all;

library work;
use work.entree_sortie_pkg.all;

entity traitement_inv is
  generic(
    N_col   : natural := 256;
    N_ligne : natural := 256;
    BPP     : natural := 8
  );
end traitement_inv;

architecture rtl of traitement_inv is
begin

  P_BUREAU : process
    variable img_in   : IMAGE2D(1 to N_ligne, 1 to N_col);
    variable img_out  : IMAGE2D(1 to N_ligne, 1 to N_col);
  begin
    img_in := get_pixels("bureau.txt", N_col, N_ligne);

    for i in 1 to N_ligne loop
      for j in 1 to N_col loop
        img_out(i, j) := 255 - img_in(i, j);
      end loop;
    end loop;

    put_pixels("bureau_inv.txt", img_out, N_col, N_ligne, BPP);
    wait; 
  end process;

  P_FEMME : process
    variable img_in   : IMAGE2D(1 to N_ligne, 1 to N_col);
    variable img_out  : IMAGE2D(1 to N_ligne, 1 to N_col);
  begin
    img_in := get_pixels("femme.txt", N_col, N_ligne);

    for i in 1 to N_ligne loop
      for j in 1 to N_col loop
        img_out(i, j) := 255 - img_in(i, j);
      end loop;
    end loop;

    put_pixels("femme_inv.txt", img_out, N_col, N_ligne, BPP);
    wait;
  end process;

  P_BLOC : process
    variable img_in   : IMAGE2D(1 to N_ligne, 1 to N_col);
    variable img_out  : IMAGE2D(1 to N_ligne, 1 to N_col);
  begin
    img_in := get_pixels("bloc.txt", N_col, N_ligne);

    for i in 1 to N_ligne loop
      for j in 1 to N_col loop
        img_out(i, j) := 255 - img_in(i, j);
      end loop;
    end loop;

    put_pixels("bloc_inv.txt", img_out, N_col, N_ligne, BPP);
    wait;
  end process;

end architecture rtl;
