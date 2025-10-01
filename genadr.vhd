library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

use work.elfifo_pkg.all;

entity genadr is
    generic (
        M : natural := 4 
    );
    port (
        reset    : in  std_logic;
        clk      : in  std_logic;
        incwrite : in  std_logic;
        incread  : in  std_logic;
        selread  : in  std_logic;
        adrg     : out std_logic_vector(M-1 downto 0)
    );
end genadr;

architecture archi_genadr of genadr is
    signal write_ptr : std_logic_vector(M-1 downto 0);
    signal read_ptr  : std_logic_vector(M-1 downto 0);
begin
    dcpt_write: dcpt_m
        generic map (M => M)
        port map (
            reset  => reset,
            clk    => clk,
            ud     => '1',
            enable => incwrite,
            cptr   => write_ptr
        );

    dcpt_read: dcpt_m
        generic map (M => M)
        port map (
            reset  => reset,
            clk    => clk,
            ud     => '1',
            enable => incread,
            cptr   => read_ptr
        );

    process(selread, write_ptr, read_ptr)
    begin
        if selread = '1' then
            adrg <= read_ptr;
        else
            adrg <= write_ptr;
        end if;
    end process;

end archi_genadr;