library ieee;
use ieee.std_logic_1164.all;

library work;
use work.entree_sortie_pkg.all;

entity traitement_masque3x3 is
  generic(
    N_col   : natural := 256;
    N_ligne : natural := 256;
    BPP     : natural := 8
  );
end traitement_masque3x3;

architecture rtl of traitement_masque3x3 is
  -- clamp helper
  function clamp8(x : integer) return integer is
  begin
    if x < 0 then
      return 0;
    elsif x > 255 then
      return 255;
    else
      return x;
    end if;
  end function;
begin

  P_BUREAU : process
    variable img_in   : IMAGE2D(1 to N_ligne, 1 to N_col);
    variable img_out  : IMAGE2D(1 to N_ligne, 1 to N_col);
    variable s : integer;
  begin
    img_in := get_pixels("bureau.txt", N_col, N_ligne);

    -- set borders to 0 and convolve interior
    for i in 1 to N_ligne loop
      for j in 1 to N_col loop
        if (i = 1) or (i = N_ligne) or (j = 1) or (j = N_col) then
          img_out(i, j) := 0;  -- border/corners forced to 0
        else
          -- 3x3 mean (integer division by 9)
          s :=  img_in(i-1, j-1) + img_in(i-1, j) + img_in(i-1, j+1)
              + img_in(i,   j-1) + img_in(i,   j) + img_in(i,   j+1)
              + img_in(i+1, j-1) + img_in(i+1, j) + img_in(i+1, j+1);
          img_out(i, j) := s / 9;
        end if;
      end loop;
    end loop;

    put_pixels("bureau_m3x3.txt", img_out, N_col, N_ligne, BPP);
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
          s :=  img_in(i-1, j-1) + img_in(i-1, j) + img_in(i-1, j+1)
              + img_in(i,   j-1) + img_in(i,   j) + img_in(i,   j+1)
              + img_in(i+1, j-1) + img_in(i+1, j) + img_in(i+1, j+1);
          img_out(i, j) := s / 9;
        end if;
      end loop;
    end loop;

    put_pixels("femme_m3x3.txt", img_out, N_col, N_ligne, BPP);
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
          s :=  img_in(i-1, j-1) + img_in(i-1, j) + img_in(i-1, j+1)
              + img_in(i,   j-1) + img_in(i,   j) + img_in(i,   j+1)
              + img_in(i+1, j-1) + img_in(i+1, j) + img_in(i+1, j+1);
          img_out(i, j) := s / 9;
        end if;
      end loop;
    end loop;

    put_pixels("bloc_m3x3.txt", img_out, N_col, N_ligne, BPP);
    wait;
  end process;

end architecture rtl;
