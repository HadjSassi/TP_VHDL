library IEEE;
use IEEE.std_logic_1164.all;
use ieee.numeric_std.all;

entity dcpt_m is
    generic(
    M : natural := 8
    );
    port (
    reset : in std_logic;
    ud  : in  std_logic;
    clk    : in  std_logic;
    enable    : in  std_logic;
    dc_out: out std_logic_vector(M-1 downto 0)
    );
end entity;

architecture rtl of dcpt_m is
    signal cnt : unsigned(M-1 downto 0) := (others => '0');
begin
    process(clk)
    begin
    if rising_edge(clk) then
        if reset = '1' then
        cnt <= (others => '0');
        elsif enable = '1' then
        if ud = '1' then
            cnt <= cnt + 1;                  
        else
            cnt <= cnt - 1;                   
        end if;
        end if;
    end if;
    end process;

    dc_out <= std_logic_vector(cnt);

end architecture;