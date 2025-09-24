library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ram is
    generic (
        M : natural := 4;
        N : natural := 8
    );
    port (
        clk   : in  std_logic;
        cs_n  : in  std_logic;
        rw_n  : in  std_logic;
        oe    : in  std_logic;
        adr   : in  std_logic_vector(M-1 downto 0);
        din   : in  std_logic_vector(N-1 downto 0);
        dout  : out std_logic_vector(N-1 downto 0)
    );
end entity ram;

architecture behavioral of ram is
    type ram_type is array (0 to 2**M - 1) of std_logic_vector(N-1 downto 0);
    signal ram_data : ram_type := (others => (others => '0'));  
begin
    write_proc : process (clk)
    begin
        if rising_edge(clk) then
            if cs_n = '0' and rw_n = '0' then
                ram_data(to_integer(unsigned(adr))) <= din;
            end if;
        end if;
    end process write_proc;

    read_proc : process (cs_n, rw_n, oe, adr, ram_data)
    begin
        if cs_n = '0' and rw_n = '1' and oe = '1' then
            dout <= ram_data(to_integer(unsigned(adr)));
        else
            dout <= (others => 'Z');
        end if;
    end process read_proc;

end architecture behavioral;
