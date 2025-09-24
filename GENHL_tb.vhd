library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity GENHL_tb is
end entity GENHL_tb;

architecture tb of GENHL_tb is
    constant CLK_PERIOD : time := 100 ns;  -- 10 MHz clock

    -- Test signals
    signal CLK   : std_logic := '0';
    signal RESET : std_logic := '1';
    signal ENREAD : std_logic;
    signal ENWRITE : std_logic;

    -- Component declaration
    component GENHL
        port (
            CLK    : in  std_logic;
            RESET  : in  std_logic;
            ENREAD : out std_logic;
            ENWRITE : out std_logic
        );
    end component;

begin
    -- Instantiate GENHL
    uut: GENHL
        port map (
            CLK    => CLK,
            RESET  => RESET,
            ENREAD => ENREAD,
            ENWRITE => ENWRITE
        );

    -- Clock generation
    clk_gen : process
    begin
        while true loop
            CLK <= '0';
            wait for CLK_PERIOD / 2;
            CLK <= '1';
            wait for CLK_PERIOD / 2;
        end loop;
    end process clk_gen;

    -- Stimulus process
    stimulus : process
    begin
        -- Reset phase
        RESET <= '1';
        wait for 200 ns;
        RESET <= '0';
        wait for 200 ns;

        -- Run for a few cycles to observe ENREAD/ENWRITE
        wait for 1000 ns;  -- Covers multiple 200-cycle periods
        assert false report "Simulation completed." severity note;
        wait;
    end process stimulus;

end architecture tb;
