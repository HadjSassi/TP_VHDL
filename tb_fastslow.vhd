library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.elfifo_pkg.all;

entity tb_fastslow is
end entity;

architecture tb of tb_fastslow is
  constant M_TB       : natural := 4;
  constant CLK_PERIOD : time    := 10 ns;

  signal CLK      : std_logic := '0';
  signal Reset    : std_logic := '0';
  signal incread  : std_logic := '0';
  signal incwrite : std_logic := '0';
  signal fast     : std_logic;
  signal slow     : std_logic;

  -- reference counter
  signal ref_cnt  : unsigned(M_TB-1 downto 0) := (others => '0');
  signal dut_cnt_msb1, dut_cnt_msb2 : std_logic;
  signal ref_fast, ref_slow : std_logic;

begin
  -- clock
  CLK <= not CLK after CLK_PERIOD/2;

  -- DUT
  U_DUT : entity work.fastslow
    generic map ( M => M_TB )
    port map (
      Reset    => Reset,
      CLK      => CLK,
      incread  => incread,
      incwrite => incwrite,
      fast     => fast,
      slow     => slow
    );

  -- fifo word counter reference module
  ref_proc : process(CLK)
  begin
    if rising_edge(CLK) then
      if Reset = '1' then
        ref_cnt <= (others => '0');
      else
        -- enable = incwrite xor incread ; ud = incwrite
        if (incwrite = '1' and incread = '0') then
          ref_cnt <= ref_cnt + 1;         -- up
        elsif (incwrite = '0' and incread = '1') then
          ref_cnt <= ref_cnt - 1;         -- down
        else
          -- if both 0 or both 1 -> mantain
          ref_cnt <= ref_cnt;
        end if;
      end if;
    end if;
  end process;

  -- check msb ref flags
  ref_fast <= '1' when (ref_cnt(M_TB-1) = '0' and ref_cnt(M_TB-2) = '0') else '0';
  ref_slow <= '1' when (ref_cnt(M_TB-1) = '1' and ref_cnt(M_TB-2) = '1') else '0';

  -- reference counter msbs to compare
  dut_cnt_msb1 <= ref_cnt(M_TB-1);
  dut_cnt_msb2 <= ref_cnt(M_TB-2);

  -- checkers
  check_flags : process(CLK)
  begin
    if rising_edge(CLK) then
      -- fast/slow
      assert fast = ref_fast
        report "FAST mismatch: fast=" & std_logic'image(fast) &
               " ref=" & std_logic'image(ref_fast) &
               " cnt=" & integer'image(to_integer(ref_cnt))
        severity error;

      assert slow = ref_slow
        report "SLOW mismatch: slow=" & std_logic'image(slow) &
               " ref=" & std_logic'image(ref_slow) &
               " cnt=" & integer'image(to_integer(ref_cnt))
        severity error;
    end if;
  end process;

  -- stimuli
  stim : process
  begin
    -- reset
    Reset    <= '1';
    incread  <= '0';
    incwrite <= '0';
    wait for 3*CLK_PERIOD;
    Reset <= '0';
    wait for 1*CLK_PERIOD;

    -- 1) increase incwrite until over fast threshold
    incwrite <= '1'; incread <= '0';
    wait for 6*CLK_PERIOD;

    -- 2) both at 0, nothing changes
    incwrite <= '0'; incread <= '0';
    wait for 3*CLK_PERIOD;

    -- 3) increase incwrite until over slow threshold
    incwrite <= '1'; incread <= '0';
    wait for 8*CLK_PERIOD;

    -- 4) both at 0, cancel changes
    incwrite <= '1'; incread <= '1';
    wait for 4*CLK_PERIOD;

    -- 5) decrease until off slow and in fast
    incwrite <= '0'; incread <= '1';
    wait for 14*CLK_PERIOD;

    -- 6) reset in middle of process
    Reset <= '1';
    wait for 1*CLK_PERIOD;
    Reset <= '0';
    -- up
    incwrite <= '1'; incread <= '0';
    wait for 5*CLK_PERIOD;


    report "Testbench fastslow fini sans erreurs." severity note;
    wait;
  end process;

end architecture;
