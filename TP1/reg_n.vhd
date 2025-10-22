library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.CHECK_PKG.all;

entity reg_n is
    generic (
        N : natural := 8
    );
    port (
        reset : in std_logic;
        clk   : in std_logic;
        ent   : in std_logic_vector(N-1 downto 0);
        sort  : out std_logic_vector(N-1 downto 0)
    );
end reg_n;

architecture archi_reg_n of reg_n is
begin
    check_setup(clk, ent, t_setup => 5 ns);
    check_hold(clk, ent, t_hold => 3 ns);

    process(clk)
    begin
        if rising_edge(clk) then
            if reset = '1' then
                sort <= (others => '0');
            else
                sort <= ent;
            end if;
        end if;
    end process;

end archi_reg_n;