library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use std.textio.all;  -- For output if needed

-- Assuming package for components
use work.elfifo_pkg.all;

entity tb_genadr is
end tb_genadr;

architecture archi_tb_genadr of tb_genadr is
    -- Component declaration (should be in package, but for completeness)
    component genadr
        generic (
            M : natural
        );
        port (
            reset    : in  std_logic;
            clk      : in  std_logic;
            incwrite : in  std_logic;
            incread  : in  std_logic;
            selread  : in  std_logic;
            adrg     : out std_logic_vector(M-1 downto 0)
        );
    end component;

    constant M_BITS : natural := 4;  -- For 16 addresses
    constant CLK_PERIOD : time := 100 ns;  -- 10 MHz clock

    signal reset_tb    : std_logic := '1';
    signal clk_tb      : std_logic := '0';
    signal incwrite_tb : std_logic := '0';
    signal incread_tb  : std_logic := '0';
    signal selread_tb  : std_logic := '0';
    signal adrg_tb     : std_logic_vector(M_BITS-1 downto 0);

begin
    -- Instantiate DUT
    dut: genadr
        generic map (M => M_BITS)
        port map (
            reset    => reset_tb,
            clk      => clk_tb,
            incwrite => incwrite_tb,
            incread  => incread_tb,
            selread  => selread_tb,
            adrg     => adrg_tb
        );

    -- Clock generator
    clk_process: process
    begin
        clk_tb <= '0';
        wait for CLK_PERIOD / 2;
        clk_tb <= '1';
        wait for CLK_PERIOD / 2;
    end process;

    -- Stimulus process
    stimulus: process
    begin
        -- Initial reset
        reset_tb <= '1';
        wait for 2 * CLK_PERIOD;
        reset_tb <= '0';

        -- Test write increments (selread=0, incwrite=1 for 5 cycles)
        selread_tb <= '0';
        for i in 0 to 4 loop
            incwrite_tb <= '1';
            wait for CLK_PERIOD;
            incwrite_tb <= '0';
            wait for CLK_PERIOD;
        end loop;
        -- Check adrg should be 0,1,2,3,4 (but since pulsed, verify in waveform)

        -- Switch to read select and increment read (selread=1, incread=1 for 3 cycles)
        selread_tb <= '1';
        for i in 0 to 2 loop
            incread_tb <= '1';
            wait for CLK_PERIOD;
            incread_tb <= '0';
            wait for CLK_PERIOD;
        end loop;
        -- adrg should now show read_ptr values

        -- Test simultaneous? But since separate enables, alternate
        -- Reset and test write again
        reset_tb <= '1';
        wait for CLK_PERIOD;
        reset_tb <= '0';
        incwrite_tb <= '1';
        wait for CLK_PERIOD;
        incwrite_tb <= '0';
        selread_tb <= '0';  -- Select write
        wait for CLK_PERIOD;

        -- Run longer to observe
        wait for 50 * CLK_PERIOD;

        -- End simulation
        report "Testbench completed" severity note;
        wait;
    end process;

end archi_tb_genadr;