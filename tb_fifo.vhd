library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.elfifo_pkg.all;

entity tb_fifo is
end tb_fifo;

architecture archi_tb of tb_fifo is
  component fifo
    port (
      clk          : in  std_logic;
      reset        : in  std_logic;
      req          : in  std_logic;
      din          : in  std_logic_vector(7 downto 0);
      ack          : out std_logic;
      hl           : out std_logic;
      dout         : out std_logic_vector(7 downto 0);
      enread_dbg   : out std_logic;
      enwrite_dbg  : out std_logic;
      adrg_dbg     : out std_logic_vector(3 downto 0);
      selread_dbg  : out std_logic  -- Added for verification
    );
  end component;

  constant CLK_PERIOD : time := 100 ns;
  constant M         : natural := 4;  -- For 16 words (2^M)

  signal clk_tb, reset_tb, req_tb : std_logic := '0';
  signal din_tb                   : std_logic_vector(7 downto 0) := (others => '0');
  signal ack_tb, hl_tb            : std_logic;
  signal dout_tb                  : std_logic_vector(7 downto 0);
  signal enread_dbg_tb, enwrite_dbg_tb : std_logic;
  signal adrg_dbg_tb              : std_logic_vector(M-1 downto 0);
  signal selread_dbg_tb           : std_logic;
  signal done : std_logic := '0';

  -- Trackers for expected pointers (manual simulation in TB)
  signal expected_write_ptr : unsigned(M-1 downto 0) := (others => '0');
  signal expected_read_ptr  : unsigned(M-1 downto 0) := (others => '0');

  constant ZZZ : std_logic_vector(7 downto 0) := (others => 'Z');

begin
  -- DUT
  u_dut : fifo
    port map (
      clk          => clk_tb,
      reset        => reset_tb,
      req          => req_tb,
      din          => din_tb,
      ack          => ack_tb,
      hl           => hl_tb,
      dout         => dout_tb,
      enread_dbg   => enread_dbg_tb,
      enwrite_dbg  => enwrite_dbg_tb,
      adrg_dbg     => adrg_dbg_tb,
      selread_dbg  => selread_dbg_tb
    );

  -- clock
  clk_proc : process
  begin
    while done = '0' loop
      clk_tb <= '0'; wait for CLK_PERIOD/2;
      clk_tb <= '1'; wait for CLK_PERIOD/2;
    end loop;
    wait;
  end process;

  -- stimuli
  stim : process
    procedure wait_cycles(n : natural) is
    begin
      for i in 1 to n loop
        wait until rising_edge(clk_tb);
      end loop;
    end procedure;

    procedure wait_hl_pulse(max_cycles : natural) is
      variable k : natural := 0;
    begin
      loop
        wait until rising_edge(clk_tb);
        if hl_tb = '1' then
          exit;
        end if;
        k := k + 1;
        assert k <= max_cycles report "Timeout waiting for HL pulse" severity error;
      end loop;
    end procedure;

    -- Procedure to verify address after inc or sel change
    procedure verify_address_after_cycle(msg : string) is
      variable expected_adrg : std_logic_vector(M-1 downto 0);
    begin
      wait until rising_edge(clk_tb);
      if selread_dbg_tb = '1' then
        expected_adrg := std_logic_vector(expected_read_ptr);
      else
        expected_adrg := std_logic_vector(expected_write_ptr);
      end if;
      assert adrg_dbg_tb = expected_adrg
        report msg & ": adrg mismatch. Expected " & integer'image(to_integer(unsigned(expected_adrg))) &
        " (selread=" & std_logic'image(selread_dbg_tb) & "), got " & integer'image(to_integer(unsigned(adrg_dbg_tb)))
        severity error;
      report msg & ": adrg OK (" & integer'image(to_integer(unsigned(adrg_dbg_tb))) & "), selread=" & std_logic'image(selread_dbg_tb) severity note;
    end procedure;

    -- Procedure for write (incwrite, selread=0 expected)
    procedure do_write_and_verify is
    begin
      req_tb <= '0';  -- Trigger ecrire
      verify_address_after_cycle("Before write inc");  -- Should be in ecrire next cycle
      expected_write_ptr <= expected_write_ptr + 1;    -- Simulate inc
      verify_address_after_cycle("After write inc");
      wait until rising_edge(clk_tb);  -- To attente
    end procedure;

    -- Procedure for read (incread, selread=1 expected)
    procedure do_read_and_verify is
    begin
      wait_hl_pulse(40);
      assert hl_tb = '1' report "HL must be 1 during read" severity error;
      expected_read_ptr <= expected_read_ptr + 1;      -- Simulate inc
      verify_address_after_cycle("After read inc");
      wait until rising_edge(clk_tb);  -- Back to previous state
    end procedure;

  begin
    -- reset -> Repos (ptrs=0, selread=0)
    req_tb   <= '1';
    din_tb   <= (others => '0');
    expected_write_ptr <= (others => '0');
    expected_read_ptr  <= (others => '0');
    reset_tb <= '1';   wait_cycles(2);
    reset_tb <= '0';   wait until rising_edge(clk_tb);
    verify_address_after_cycle("After reset");

    -- Scenario 1: Idle read (lect1: incread, selread=1)
    wait_cycles(20);
    do_read_and_verify;
    assert hl_tb = '0' report "Back to Repos: HL/=0" severity error;

    -- Scenario 2: Write then read (ecrire: incwrite sel=0 -> attente sel=1 -> lect2: incread)
    do_write_and_verify;  -- incwrite
    do_read_and_verify;   -- incread
    req_tb <= '1'; wait until rising_edge(clk_tb);  -- To repos

    -- Additional: Multiple writes (test incwrite chain, sel=0)
    wait_cycles(5);
    for i in 1 to 8 loop
      do_write_and_verify;
    end loop;
    req_tb <= '1'; wait until rising_edge(clk_tb);  -- Repos (write_ptr=1+8=9)

    -- Multiple reads (test incread, sel=1)
    wait_cycles(5);
    for i in 1 to 6 loop
      do_read_and_verify;
    end loop;

    -- Test sel switch without inc (in attente: sel=1, no inc)
    req_tb <= '0'; do_write_and_verify;  -- To attente (sel=1 after ecrire)
    verify_address_after_cycle("In attente: sel switch, no inc expected");  -- adrg should stay on read_ptr
    req_tb <= '1'; wait until rising_edge(clk_tb);  -- Back repos (sel=0)

    -- Scenario 3: Reset in attente (ptrs reset to 0)
    req_tb <= '0'; do_write_and_verify;  -- Ecrire + attente
    wait until rising_edge(clk_tb);
    reset_tb <= '1'; verify_address_after_cycle("During reset");
    reset_tb <= '0'; req_tb <= '1'; wait until rising_edge(clk_tb);
    assert to_integer(unsigned(adrg_dbg_tb)) = 0 report "Address not reset" severity error;
    expected_write_ptr <= (others => '0');
    expected_read_ptr  <= (others => '0');

    -- Run longer
    wait_cycles(100);

    report "TB fifo (GENHL + SEQ + GENADR only) finished OK." severity note;
    done <= '1';
    wait;
  end process;

end architecture;