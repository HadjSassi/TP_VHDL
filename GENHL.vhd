library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity GENHL is
    port (
        CLK   : in  std_logic;
        RESET : in  std_logic;
        ENREAD : out std_logic;
        ENWRITE : out std_logic
    );
end entity GENHL;

architecture behavioral of GENHL is
    component DCPT_M is
        generic (
            M : natural
        );
        port (
            CLK    : in  std_logic;
            RESET  : in  std_logic;
            UD     : in  std_logic;
            ENABLE : in  std_logic;
            CPTR   : out std_logic_vector(M-1 downto 0)
        );
    end component;

    -- Signals
    signal counter : std_logic_vector(7 downto 0);
    signal enable_counter : std_logic := '1';
    constant MAX_COUNT : integer := 199;

begin
    counter_inst : DCPT_M
        generic map (
            M => 8
        )
        port map (
            CLK    => CLK,
            RESET  => RESET,
            UD     => '1',
            ENABLE => enable_counter,
            CPTR   => counter
        );

    process (counter, RESET)
    begin
        if RESET = '1' then
            ENREAD <= '0';
            ENWRITE <= '0';
        else
            if unsigned(counter) = MAX_COUNT then
                ENREAD <= '1';
                ENWRITE <= '0';
            else
                ENREAD <= '0';
                ENWRITE <= '1';
            end if;
        end if;
    end process;

end architecture behavioral;