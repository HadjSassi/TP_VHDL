-- File: tb_reg_n.vhd
-- Description: Testbench for reg_n, provoking setup and hold violations.
-- Setup violation: Change data <5ns before rising edge.
-- Hold violation: Change data <3ns after rising edge.
-- Observe assertion messages in simulation transcript.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;

entity tb_reg_n is
end tb_reg_n;

architecture archi_tb_reg_n of tb_reg_n is
    component reg_n
        generic (N : natural);
        port (
            reset : in std_logic;
            clk   : in std_logic;
            ent   : in std_logic_vector(N-1 downto 0);
            sort  : out std_logic_vector(N-1 downto 0)
        );
    end component;

    constant N_BITS : natural := 8;
    constant CLK_PERIOD : time := 20 ns;  -- 50 MHz clock

    signal reset_tb : std_logic := '1';
    signal clk_tb   : std_logic := '0';
    signal ent_tb   : std_logic_vector(N_BITS-1 downto 0) := (others => '0');
    signal sort_tb  : std_logic_vector(N_BITS-1 downto 0);

begin
    -- Instantiate DUT
    dut: reg_n
        generic map (N => N_BITS)
        port map (
            reset => reset_tb,
            clk   => clk_tb,
            ent   => ent_tb,
            sort  => sort_tb
        );

    -- Clock generator
    clk_gen: process
    begin
        clk_tb <= '0';
        wait for CLK_PERIOD / 2;
        clk_tb <= '1';
        wait for CLK_PERIOD / 2;
    end process;

    -- Stimulus
    stimulus: process
    begin
        -- Reset
        reset_tb <= '1';
        wait for 2 * CLK_PERIOD;
        reset_tb <= '0';

        -- Normal operation: Change data mid-cycle, should capture correctly, no violations
        ent_tb <= "00000001";
        wait until rising_edge(clk_tb);
        wait for CLK_PERIOD / 2;  -- Mid low, far from edges
        ent_tb <= "00000101";
        wait until rising_edge(clk_tb);
        assert sort_tb = "00000001" report "Capture error in normal" severity error;
        wait until rising_edge(clk_tb);
        assert sort_tb = "00000101" report "Capture error in normal" severity error;

        -- Provoke setup violation: Change data 2 ns before rising edge (<5 ns)
        wait for CLK_PERIOD / 2 - 2 ns;  -- 2 ns before rising
        ent_tb <= "00001010";
        wait until rising_edge(clk_tb);  -- Violation should trigger message
        wait until rising_edge(clk_tb);
        -- Note: Capture may be wrong, but focus on assertion

        -- Provoke hold violation: Change data 1 ns after rising edge (<3 ns)
        wait until rising_edge(clk_tb);
        wait for 1 ns;  -- 1 ns after rising
        ent_tb <= "11111111";  -- Change immediately after
        wait until rising_edge(clk_tb);  -- Violation message
        wait until rising_edge(clk_tb);
        assert sort_tb /= "00001010" report "Unexpected capture on hold violation" severity note;

        -- More cycles to observe
        wait for 10 * CLK_PERIOD;

        report "Testbench completed. Check transcript for violation messages." severity note;
        wait;
    end process;

end archi_tb_reg_n;