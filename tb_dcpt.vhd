library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.elfifo_pkg.all;  

entity tb_dcpt_m is

end entity;

architecture tb of tb_dcpt_m is
  constant M_TB         : natural := 4;
  constant CLK_PERIOD   : time    := 10 ns;

  signal clk    : std_logic := '0';
  signal reset  : std_logic := '0';
  signal enable : std_logic := '0';
  signal ud     : std_logic := '1'; 
  signal dc_out      : std_logic_vector(M_TB-1 downto 0);

  -- ref module
  signal ref_cnt : unsigned(M_TB-1 downto 0) := (others => '0');

begin
  --------------------------------------------------------------------
  -- clock
  clk <= not clk after CLK_PERIOD/2;

  --------------------------------------------------------------------
  -- dut
  U_DUT : dcpt_m
    generic map ( M => M_TB )
    port map (
      clk    => clk,
      reset  => reset,
      enable => enable,
      ud     => ud,
      dc_out      => dc_out
    );

  --------------------------------------------------------------------
  -- reference counter generation (mimics dut)
  ref_proc : process(clk)
  begin
    if rising_edge(clk) then
      if reset = '1' then
        ref_cnt <= (others => '0');
      elsif enable = '1' then
        if ud = '1' then
          ref_cnt <= ref_cnt + 1;
        else
          ref_cnt <= ref_cnt - 1;
        end if;
      end if;
    end if;
  end process;

  --------------------------------------------------------------------
  -- check dut vs ref counter
  check_proc : process(clk)
  begin
    if rising_edge(clk) then
        assert dc_out = std_logic_vector(ref_cnt)
        report "Mismatch: dc_out=" & integer'image(to_integer(unsigned(dc_out)))
            & " ref="    & integer'image(to_integer(ref_cnt))
    severity error;

    end if;
  end process;

  --------------------------------------------------------------------
  -- stimuli
  stim_proc : process
  begin
    -- active reset
    reset  <= '1';
    enable <= '0';
    ud     <= '1';
    wait for 3*CLK_PERIOD;

    -- clear reset
    reset <= '0';
    wait for 1*CLK_PERIOD;

    -- 2) count up for 10 cycles (includes wrap 0xF->0x0)
    enable <= '1';
    ud     <= '1';
    wait for 10*CLK_PERIOD;

    -- 3) enable = 0
    enable <= '0';
    wait for 5*CLK_PERIOD;

    -- 4) count down for 10 cycles (includes wrap 0x0->0xF)
    enable <= '1';
    ud     <= '0';
    wait for 10*CLK_PERIOD;

    -- 5) change direction in middle of count
    ud <= '1';
    wait for 6*CLK_PERIOD;
    ud <= '0';
    wait for 6*CLK_PERIOD;

    -- 6) try sync reset
    reset <= '1';         -- next edge
    wait for 1*CLK_PERIOD;
    reset <= '0';
    -- after reset, count up for a few cycles
    ud     <= '1';
    enable <= '1';
    wait for 8*CLK_PERIOD;

    -- end test
	report "Testbench fini sans erreurs." severity note;
	assert false report "Erreur. Testbench fini." severity failure;
	wait;
  end process;

end architecture;
