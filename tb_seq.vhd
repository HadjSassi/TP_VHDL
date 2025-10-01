library ieee;
use ieee.std_logic_1164.all;
use std.textio.all;

entity tb_seq is
end tb_seq;

architecture archi_tb_seq of tb_seq is
    component seq
        port (
            clk      : in  std_logic;
            reset    : in  std_logic;
            enread   : in  std_logic;
            enwrite  : in  std_logic;
            req      : in  std_logic;
            ack      : out std_logic;
            rw_n     : out std_logic;
            oe       : out std_logic;
            incwrite : out std_logic;
            incread  : out std_logic;
            hl       : out std_logic;
            selread  : out std_logic;
            cs_n     : out std_logic
        );
    end component;

    constant CLK_PERIOD : time := 100 ns;  -- 10 MHz

    signal clk_tb      : std_logic := '0';
    signal reset_tb    : std_logic := '1';
    signal enread_tb   : std_logic := '0';
    signal enwrite_tb  : std_logic := '1';  -- Default high, as per GENHL (high most time)
    signal req_tb      : std_logic := '0';
    signal ack_tb      : std_logic;
    signal rw_n_tb     : std_logic;
    signal oe_tb       : std_logic;
    signal incwrite_tb : std_logic;
    signal incread_tb  : std_logic;
    signal hl_tb       : std_logic;
    signal selread_tb  : std_logic;
    signal cs_n_tb     : std_logic;

begin
    -- Instantiate DUT (will use configuration for archi_moore or archi_mealy)
    dut: seq
        port map (
            clk      => clk_tb,
            reset    => reset_tb,
            enread   => enread_tb,
            enwrite  => enwrite_tb,
            req      => req_tb,
            ack      => ack_tb,
            rw_n     => rw_n_tb,
            oe       => oe_tb,
            incwrite => incwrite_tb,
            incread  => incread_tb,
            hl       => hl_tb,
            selread  => selread_tb,
            cs_n     => cs_n_tb
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
        -- Initial reset (stay in repos)
        reset_tb <= '1';
        wait for 5 * CLK_PERIOD;
        reset_tb <= '0';
        wait for CLK_PERIOD;
        -- Check repos: ack=1, others=0 (approx)
        assert ack_tb = '1' report "Ack not 1 in repos" severity error;
        assert incwrite_tb = '0' report "Incwrite not 0 in repos" severity error;
        assert incread_tb = '0' report "Incread not 0 in repos" severity error;
        assert oe_tb = '0' report "OE not 0 in repos" severity error;
        wait for CLK_PERIOD;

        -- Test read priority: enread=1, ignore req (set req=1 too)
        req_tb <= '1';
        enread_tb <= '1';
        enwrite_tb <= '0';  -- During read slot
        wait for CLK_PERIOD;  -- Go to lect1
        assert ack_tb = '0' report "Ack not delayed in lect1" severity error;
        assert rw_n_tb = '1' report "RW_n not 1 in lect1" severity error;
        assert oe_tb = '1' report "OE not 1 in lect1" severity error;
        assert incread_tb = '1' report "Incread not 1 in lect1" severity error;
        assert hl_tb = '1' report "HL not 1 in lect1" severity error;
        assert selread_tb = '1' report "Selread not 1 in lect1" severity error;
        assert cs_n_tb = '0' report "CS_n not 0 in lect1" severity error;
        wait for CLK_PERIOD;  -- Go to lect2
        assert ack_tb = '0' report "Ack not 0 in lect2" severity error;
        assert oe_tb = '1' report "OE not 1 in lect2 (Mealy may differ)" severity note;
        assert selread_tb = '1' report "Selread not 1 in lect2" severity error;
        enread_tb <= '0';
        enwrite_tb <= '1';
        wait for CLK_PERIOD;  -- Go to attente (since req=1)
        assert ack_tb = '0' report "Ack not 0 in attente" severity error;
        assert selread_tb = '1' report "Selread not 1 in attente" severity error;
        wait for CLK_PERIOD;  -- Go to ecrire (enwrite=1)
        assert ack_tb = '1' report "Ack not 1 in ecrire" severity error;
        assert rw_n_tb = '0' report "RW_n not 0 in ecrire" severity error;
        assert incwrite_tb = '1' report "Incwrite not 1 in ecrire" severity error;
        assert cs_n_tb = '0' report "CS_n not 0 in ecrire" severity error;
        req_tb <= '0';  -- End req
        wait for CLK_PERIOD;  -- Back to repos
        assert ack_tb = '1' report "Ack not 1 back to repos" severity error;
        wait for 2 * CLK_PERIOD;

        -- Test write from repos (no read pending)
        req_tb <= '1';
        wait for CLK_PERIOD;  -- Stay repos (enread=0, but enwrite=1, req=1 -> go to ecrire next)
        wait for CLK_PERIOD;  -- In ecrire
        assert incwrite_tb = '1' report "Incwrite not 1 in ecrire" severity error;
        assert ack_tb = '1' report "Ack not 1 in ecrire" severity error;
        req_tb <= '0';
        wait for CLK_PERIOD;  -- Back to repos
        wait for 2 * CLK_PERIOD;

        -- Test idle in repos (no enread, no req)
        assert ack_tb = '1' report "Ack not 1 in idle repos" severity error;
        wait for 5 * CLK_PERIOD;

        report "Testbench completed - verify waveform for states/transitions" severity note;
        wait;
    end process;

end archi_tb_seq;