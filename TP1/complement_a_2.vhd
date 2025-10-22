library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use work.mes_fonctions.all ;

entity complement_a_2 is
    GENERIC (N : INTEGER := 8);
    Port ( nombre : in STD_LOGIC_VECTOR (N-1 downto 0);
           sortie : out STD_LOGIC_VECTOR (N-1 downto 0));
end complement_a_2;

architecture archi_complement_a_2 of complement_a_2 is
begin
    sortie <= cpl2(nombre, N);
end archi_complement_a_2;
