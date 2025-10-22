library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.entree_sortie_pkg.all;

entity traitement_gradx is
  generic(
    N_col   : natural := 256;
    N_ligne : natural := 256;
    BPP     : natural := 8
  );
end traitement_gradx;

architecture rtl of traitement_gradx is
begin
  P_BUREAU : process
    variable img_in   : IMAGE2D(1 to N_ligne, 1 to N_col);
    variable img_out  : IMAGE2D(1 to N_ligne, 1 to N_col);
    variable s : integer;
  begin
    img_in := get_pixels("bureau.txt", N_col, N_ligne);

    for i in 1 to N_ligne loop
      for j in 1 to N_col loop
        if (i = 1) or (i = N_ligne) or (j = 1) or (j = N_col) then
          img_out(i, j) := 0;
        else
          s :=  (+1)*img_in(i-1, j-1) + 0*img_in(i-1, j) + (-1)*img_in(i-1, j+1)
              + (+2)*img_in(i,   j-1) + 0*img_in(i,   j) + (-2)*img_in(i,   j+1)
              + (+1)*img_in(i+1, j-1) + 0*img_in(i+1, j) + (-1)*img_in(i+1, j+1);
          img_out(i, j) := abs(s);
        end if;
      end loop;
    end loop;

    put_pixels("bureau_gx.txt", img_out, N_col, N_ligne, BPP);
    wait;
  end process;

  P_FEMME : process
    variable img_in   : IMAGE2D(1 to N_ligne, 1 to N_col);
    variable img_out  : IMAGE2D(1 to N_ligne, 1 to N_col);
    variable s : integer;
  begin
    img_in := get_pixels("femme.txt", N_col, N_ligne);

    for i in 1 to N_ligne loop
      for j in 1 to N_col loop
        if (i = 1) or (i = N_ligne) or (j = 1) or (j = N_col) then
          img_out(i, j) := 0;
        else
          s :=  (+1)*img_in(i-1, j-1) + 0*img_in(i-1, j) + (-1)*img_in(i-1, j+1)
              + (+2)*img_in(i,   j-1) + 0*img_in(i,   j) + (-2)*img_in(i,   j+1)
              + (+1)*img_in(i+1, j-1) + 0*img_in(i+1, j) + (-1)*img_in(i+1, j+1);
          img_out(i, j) := abs(s);
        end if;
      end loop;
    end loop;

    put_pixels("femme_gx.txt", img_out, N_col, N_ligne, BPP);
    wait;
  end process;

  P_BLOC : process
    variable img_in   : IMAGE2D(1 to N_ligne, 1 to N_col);
    variable img_out  : IMAGE2D(1 to N_ligne, 1 to N_col);
    variable s : integer;
  begin
    img_in := get_pixels("bloc.txt", N_col, N_ligne);

    for i in 1 to N_ligne loop
      for j in 1 to N_col loop
        if (i = 1) or (i = N_ligne) or (j = 1) or (j = N_col) then
          img_out(i, j) := 0;
        else
          s :=  (+1)*img_in(i-1, j-1) + 0*img_in(i-1, j) + (-1)*img_in(i-1, j+1)
              + (+2)*img_in(i,   j-1) + 0*img_in(i,   j) + (-2)*img_in(i,   j+1)
              + (+1)*img_in(i+1, j-1) + 0*img_in(i+1, j) + (-1)*img_in(i+1, j+1);
          img_out(i, j) := abs(s);
        end if;
      end loop;
    end loop;

    put_pixels("bloc_gx.txt", img_out, N_col, N_ligne, BPP);
    wait;
  end process;

end architecture rtl;
