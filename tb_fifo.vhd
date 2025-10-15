library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.elfifo_pkg.all;
use work.tb_log_pkg.all;

entity tb_fifo is
end tb_fifo;

architecture archi_tb of tb_fifo is
  component FIFO
    port(
      clk   : in  std_logic;
      reset : in  std_logic;
      req   : in  std_logic;
      din   : in  std_logic_vector(7 downto 0);

      ack   : out std_logic;
      HL    : out std_logic;
      dout  : out std_logic_vector(7 downto 0);

      enread_dbg   : out std_logic;
      enwrite_dbg  : out std_logic;
      adrg_dbg     : out std_logic_vector(3 downto 0);
      selread_dbg  : out std_logic;

      rw_n_dbg     : out std_logic;
      cs_n_dbg     : out std_logic;
      oe_dbg       : out std_logic;
      incwrite_dbg : out std_logic;
      incread_dbg  : out std_logic;

      fast         : out std_logic;
      slow         : out std_logic
    );
  end component;

  constant CLK_PERIOD : time := 100 ns;
  constant M : natural := 4;
  constant TH_FAST : integer := 2**(M-2);           -- 4
  constant TH_SLOW : integer := 2**M - 2**(M-2);    -- 12

  signal clk_tb, reset_tb, req_tb : std_logic := '0';
  signal din_tb                   : std_logic_vector(7 downto 0) := (others => '0');
  signal ack_tb, hl_tb            : std_logic;
  signal dout_tb                  : std_logic_vector(7 downto 0);

  signal enread_dbg_tb, enwrite_dbg_tb : std_logic;
  signal adrg_dbg_tb              : std_logic_vector(M-1 downto 0);
  signal selread_dbg_tb           : std_logic;

  signal rw_n_dbg_tb, cs_n_dbg_tb, oe_dbg_tb : std_logic;
  signal incwrite_dbg_tb, incread_dbg_tb : std_logic;

  signal fast_dbg_tb, slow_dbg_tb : std_logic;

  signal done : std_logic := '0';

  -- Shadow RAM
  type ram_type is array (0 to 2**M - 1) of std_logic_vector(7 downto 0);
  signal shadow_ram : ram_type := (others => (others => '0'));

  signal expected_write_ptr : unsigned(M-1 downto 0) := (others => '0');
  signal expected_read_ptr  : unsigned(M-1 downto 0) := (others => '0');

  signal hl_prev : std_logic := '0';

begin
  --------------------------------------------------------------------
  -- DUT
  --------------------------------------------------------------------
  u_dut : FIFO
    port map (
      clk          => clk_tb,
      reset        => reset_tb,
      req          => req_tb,
      din          => din_tb,
      ack          => ack_tb,
      HL           => hl_tb,
      dout         => dout_tb,
      enread_dbg   => enread_dbg_tb,
      enwrite_dbg  => enwrite_dbg_tb,
      adrg_dbg     => adrg_dbg_tb,
      selread_dbg  => selread_dbg_tb,
      rw_n_dbg     => rw_n_dbg_tb,
      cs_n_dbg     => cs_n_dbg_tb,
      oe_dbg       => oe_dbg_tb,
      incwrite_dbg => incwrite_dbg_tb,
      incread_dbg  => incread_dbg_tb,
      fast         => fast_dbg_tb,
      slow         => slow_dbg_tb
    );

  --------------------------------------------------------------------
  -- Clock
  --------------------------------------------------------------------
  clk_proc : process
  begin
    while done = '0' loop
      clk_tb <= '0'; wait for CLK_PERIOD/2;
      clk_tb <= '1'; wait for CLK_PERIOD/2;
    end loop;
    wait;
  end process;

  --------------------------------------------------------------------
  -- Mirror writes exactly when RAM writes
  --------------------------------------------------------------------
  model_proc : process(clk_tb)
    variable addr_i : integer;
  begin
    if rising_edge(clk_tb) then
      if reset_tb = '1' then
        shadow_ram         <= (others => (others => '0'));
        expected_write_ptr <= (others => '0');
        expected_read_ptr  <= (others => '0');
      else
        if (cs_n_dbg_tb = '0' and rw_n_dbg_tb = '0') then
          addr_i := to_integer(unsigned(adrg_dbg_tb));
          shadow_ram(addr_i) <= din_tb;
          expected_write_ptr <= expected_write_ptr + 1;
          log_info("Mirrored WRITE @ addr=" & integer'image(addr_i) &
                   " data=" & slv_to_hex(din_tb));
        end if;
        if hl_tb = '1' then
          expected_read_ptr <= expected_read_ptr + 1;
        end if;
      end if;
    end if;
  end process;

  --------------------------------------------------------------------
  -- HL logs + read-mode checks
  --------------------------------------------------------------------
  state_logger : process(clk_tb)
  begin
    if rising_edge(clk_tb) then
      if (hl_prev = '0' and hl_tb = '1') then
        log_state("READ_WINDOW_OPEN (HL=1) addr=" & slv_to_bin(adrg_dbg_tb));
        assert_eq_sl("cs_n (read)", cs_n_dbg_tb, '0', "SEQ/RAM");
        assert_eq_sl("rw_n (read)", rw_n_dbg_tb, '1', "SEQ/RAM");
        assert_eq_sl("oe   (read)", oe_dbg_tb,   '1', "SEQ/RAM");
      elsif (hl_prev = '1' and hl_tb = '0') then
        log_state("READ_WINDOW_CLOSE (HL=0)");
      end if;
      hl_prev <= hl_tb;
    end if;
  end process;

  --------------------------------------------------------------------
  -- Stimuli
  --------------------------------------------------------------------
  stim : process
    procedure wait_cycles(n : natural) is
    begin
      for i in 1 to n loop
        wait until rising_edge(clk_tb);
      end loop;
    end procedure;

    procedure request_write(word : std_logic_vector(7 downto 0)) is
    begin
      log_state("REQUEST_WRITE data=" & slv_to_hex(word));
      din_tb <= word;
      req_tb <= '0';  -- Repos + enwrite=1 + req=0 -> Ecrire next
    end procedure;

    procedure stop_write is
    begin
      req_tb <= '1';
    end procedure;

    procedure wait_ecrire_and_attente is
    begin
      wait until rising_edge(clk_tb);  -- Ecrire (RAM write)
      wait until rising_edge(clk_tb);  -- Attente
    end procedure;

    -- Read window helper (wait until HL and check data from shadow RAM)
    procedure wait_read_at_addr(target : std_logic_vector(M-1 downto 0);
                                max_pulses : natural) is
      variable p : natural := 0;
    begin
      loop
        -- wait HL=1
        loop
          wait until rising_edge(clk_tb);
          exit when hl_tb = '1';
        end loop;
        exit when adrg_dbg_tb = target;
        p := p + 1;
        assert p < max_pulses
          report "Timeout waiting read @ addr=" &
                 integer'image(to_integer(unsigned(target)))
          severity error;
      end loop;
    end procedure;

    procedure check_read_data(tag : string) is
      variable raddr : integer;
    begin
      log_assert(tag & " -> RAM read check");
      assert_eq_sl("HL", hl_tb, '1', "SEQ");
      assert_eq_sl("cs_n (read)", cs_n_dbg_tb, '0', "SEQ/RAM");
      assert_eq_sl("rw_n (read)", rw_n_dbg_tb, '1', "SEQ/RAM");
      assert_eq_sl("oe   (read)", oe_dbg_tb,   '1', "SEQ/RAM");
      raddr := to_integer(unsigned(adrg_dbg_tb));
      if dout_tb /= shadow_ram(raddr) then
        log_error("RAM: mismatched data @ addr=" & integer'image(raddr) &
                  " exp=" & slv_to_hex(shadow_ram(raddr)) &
                  " got=" & slv_to_hex(dout_tb));
      else
        log_success("RAM: read OK @ addr=" & integer'image(raddr) &
                    " data=" & slv_to_hex(dout_tb));
      end if;
    end procedure;

    -- NEW: burst writes then assert fast/slow after each write
    procedure burst_writes_check_fastslow(num_writes : natural) is
      variable occ : integer := 0;
      variable word : std_logic_vector(7 downto 0);
    begin
      log_info("FAST/SLOW test: burst of " & integer'image(num_writes) & " writes");
      for i in 1 to num_writes loop
        -- distinct data pattern
        word := std_logic_vector(to_unsigned(i, 8));
        request_write(word);
        wait_ecrire_and_attente;
        stop_write;                     -- allow Attente -> Repos in next cycle
        wait until rising_edge(clk_tb); -- back to Repos
        occ := occ + 1;

        -- Expected fast/slow based on occupancy (no reads during burst)
        if occ < TH_FAST then
          assert_eq_sl("fast", fast_dbg_tb, '1', "FASTSLOW");
          assert_eq_sl("slow", slow_dbg_tb, '0', "FASTSLOW");
        elsif occ >= TH_SLOW then
          assert_eq_sl("fast", fast_dbg_tb, '0', "FASTSLOW");
          assert_eq_sl("slow", slow_dbg_tb, '1', "FASTSLOW");
        else
          assert_eq_sl("fast", fast_dbg_tb, '0', "FASTSLOW");
          assert_eq_sl("slow", slow_dbg_tb, '0', "FASTSLOW");
        end if;
      end loop;
    end procedure;

    -- variable to hold an address across waits
    variable wr_addr_v : std_logic_vector(M-1 downto 0) := (others => '0');

  begin
    log_info("=== TB START (FIFO + RAM + FASTSLOW) ===");

    ----------------------------------------------------------------
    -- First: minimal run to ensure everything toggles
    ----------------------------------------------------------------
    req_tb   <= '1';
    din_tb   <= (others => '0');
    reset_tb <= '1'; wait_cycles(2);
    reset_tb <= '0'; wait until rising_edge(clk_tb);
    log_success("Came out of reset");

    -- Consume addr 0 once (optional)
    request_write(x"55");  wait_ecrire_and_attente; stop_write;

    -- Read that back when HL hits addr 0
    wait_read_at_addr("0000", 40); check_read_data("Initial read @0");

    ----------------------------------------------------------------
    -- FAST/SLOW test: do a fresh reset, then burst 13 writes
    -- The burst takes ~39 cycles << 200, so no reads in between.
    ----------------------------------------------------------------
    log_state("Reset before FAST/SLOW burst");
    reset_tb <= '1'; wait until rising_edge(clk_tb);
    reset_tb <= '0'; req_tb <= '1'; wait until rising_edge(clk_tb);

    burst_writes_check_fastslow(13);  -- should hit fast region, mid region, and slow region

    ----------------------------------------------------------------
    -- Post-burst: do one read window and see slow eventually clears
    ----------------------------------------------------------------
    wait_read_at_addr(adrg_dbg_tb, 100);  -- wait next read (address not critical here)
    check_read_data("Read after burst (slow may still be 1 until enough reads)");
    wait_read_at_addr(adrg_dbg_tb, 200);
    ----------------------------------------------------------------
    -- Reset-during-Attente scenario (like before)
    ----------------------------------------------------------------
    request_write(x"B7");
    wait until rising_edge(clk_tb);      -- Ecrire now
    wr_addr_v := adrg_dbg_tb;            -- capture effective write address
    wait until rising_edge(clk_tb);      -- Attente
    log_state("RESET during Attente");
    reset_tb <= '1';
    wait until rising_edge(clk_tb);      -- Repos
    reset_tb <= '0'; req_tb <= '1';

    wait_read_at_addr(wr_addr_v, 120);
    check_read_data("Post-reset read of pre-reset write (B7)");

    log_info("=== TB END ===");
    done <= '1';
    wait;
  end process;

end architecture;

