library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.elfifo_pkg.all;     -- component decls (ram, seq, genhl, genadr, etc)
use work.tb_log_pkg.all;     -- pretty logging/assert helpers

entity tb_fifo is
end tb_fifo;

architecture archi_tb of tb_fifo is
  ------------------------------------------------------------------
  -- Helper: two's complement for TB (no waits; pure function)
  ------------------------------------------------------------------
  function cpl2_tb(x : std_logic_vector) return std_logic_vector is
    variable u : unsigned(x'range);
  begin
    u := unsigned(not x) + 1;
    return std_logic_vector(u);
  end function;

  ------------------------------------------------------------------
  -- DUT component (top FIFO)
  ------------------------------------------------------------------
  component FIFO
    port(
      clk   : in  std_logic;
      reset : in  std_logic;
      req   : in  std_logic;
      din   : in  std_logic_vector(7 downto 0);

      ack   : out std_logic;
      HL    : out std_logic;
      dout  : out std_logic_vector(7 downto 0);

      -- GENHL / GENADR / SEQ debug
      enread_dbg   : out std_logic;
      enwrite_dbg  : out std_logic;
      adrg_dbg     : out std_logic_vector(3 downto 0);
      selread_dbg  : out std_logic;

      -- RAM control debug
      rw_n_dbg     : out std_logic;
      cs_n_dbg     : out std_logic;
      oe_dbg       : out std_logic;
      incwrite_dbg : out std_logic;
      incread_dbg  : out std_logic;

      -- fast/slow
      fast         : out std_logic;
      slow         : out std_logic
    );
  end component;

  ------------------------------------------------------------------
  -- Constants
  ------------------------------------------------------------------
  constant CLK_PERIOD : time    := 100 ns;
  constant M          : natural := 4;  -- 2^M addresses (16)
  constant ZERO8      : std_logic_vector(7 downto 0) := (others => '0');

  -- fast/slow thresholds for M=4
  constant TH_FAST : integer := 2**(M-2);            -- 4
  constant TH_SLOW : integer := 2**M - 2**(M-2);     -- 12

  ------------------------------------------------------------------
  -- Signals
  ------------------------------------------------------------------
  signal clk_tb, reset_tb, req_tb : std_logic := '0';
  signal din_tb                   : std_logic_vector(7 downto 0) := (others => '0');

  signal ack_tb, hl_tb            : std_logic;
  signal dout_tb                  : std_logic_vector(7 downto 0);

  signal enread_dbg_tb, enwrite_dbg_tb : std_logic;
  signal adrg_dbg_tb : std_logic_vector(M-1 downto 0);
  signal selread_dbg_tb : std_logic;

  signal rw_n_dbg_tb, cs_n_dbg_tb, oe_dbg_tb : std_logic;
  signal incwrite_dbg_tb, incread_dbg_tb     : std_logic;

  signal fast_dbg_tb, slow_dbg_tb : std_logic;

  signal done : std_logic := '0';

  -- Shadow RAM (stores what the real RAM stores = two's complement of DIN_REG)
  type ram_type is array (0 to 2**M - 1) of std_logic_vector(7 downto 0);
  signal shadow_ram : ram_type := (others => (others => '0'));

  -- Expected pointers (info only)
  signal expected_write_ptr : unsigned(M-1 downto 0) := (others => '0');
  signal expected_read_ptr  : unsigned(M-1 downto 0) := (others => '0');

  -- For HL edge logs
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
  -- Mirror the exact RAM behavior into shadow_ram
  --  - On Écrire (cs_n=0 & rw_n=0) @ rising_edge: write C2(DIN) at ADR
  --  - Also track read pointer count by HL
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
          shadow_ram(addr_i) <= cpl2_tb(din_tb);  -- matches real RAM input (C2 of DIN_REG)
          expected_write_ptr <= expected_write_ptr + 1;
          log_info("Mirrored WRITE @ addr=" & integer'image(addr_i) &
                   " data(C2)=" & slv_to_hex(shadow_ram(addr_i)));
        end if;
        if hl_tb = '1' then
          expected_read_ptr <= expected_read_ptr + 1;
        end if;
      end if;
    end if;
  end process;

  --------------------------------------------------------------------
  -- HL edge logs + RAM read-mode checks
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
  -- Optional: log fast/slow changes
  --------------------------------------------------------------------
  fastslow_logger : process(clk_tb)
    variable fast_prev, slow_prev : std_logic := 'X';
  begin
    if rising_edge(clk_tb) then
      if fast_dbg_tb /= fast_prev then
        log_state("FAST change -> " & std_logic'image(fast_dbg_tb));
        fast_prev := fast_dbg_tb;
      end if;
      if slow_dbg_tb /= slow_prev then
        log_state("SLOW change -> " & std_logic'image(slow_dbg_tb));
        slow_prev := slow_dbg_tb;
      end if;
    end if;
  end process;

  --------------------------------------------------------------------
  -- Stimuli
  --------------------------------------------------------------------
  stim : process
    ----------------------------------------------------------------
    -- tiny helpers
    ----------------------------------------------------------------
    procedure wait_cycles(n : natural) is
    begin
      for i in 1 to n loop
        wait until rising_edge(clk_tb);
      end loop;
    end procedure;

    -- wait until we are in Repos with writes allowed (HL=0, ack=1, ENWRITE=1)
    procedure wait_repos_writable is
    begin
      loop
        wait until rising_edge(clk_tb);
        exit when (ack_tb = '1' and enwrite_dbg_tb = '1' and hl_tb = '0');
      end loop;
    end procedure;

    -- write one word between HL pulses:
    -- Repos --(req=0)--> Écrire (RAM writes) --> Attente --(req=1, ENREAD=0)--> Repos
    procedure quick_write(word : std_logic_vector(7 downto 0)) is
    begin
      wait_repos_writable;

      log_state("REQUEST_WRITE data=" & slv_to_hex(word));
      din_tb <= word;
      req_tb <= '0';
      wait until rising_edge(clk_tb);      -- Écrire

      wait until rising_edge(clk_tb);      -- Attente
      req_tb <= '1';                       -- allow Attente->Repos
      wait until rising_edge(clk_tb);      -- Repos again
    end procedure;

    -- wait next HL on a given address (or timeout)
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

    -- check that current read returns the expected shadow value
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

    -- thresholds check for fast/slow given an occupancy value
    procedure check_fastslow(occ : integer) is
    begin
      if occ < TH_FAST then               -- 0..3
        assert_eq_sl("fast(<4)", fast_dbg_tb, '1', "FASTSLOW");
        assert_eq_sl("slow(<4)", slow_dbg_tb, '0', "FASTSLOW");
      elsif occ >= TH_SLOW then           -- >=12
        assert_eq_sl("fast(>=12)", fast_dbg_tb, '0', "FASTSLOW");
        assert_eq_sl("slow(>=12)", slow_dbg_tb, '1', "FASTSLOW");
      else                                -- 4..11
        assert_eq_sl("fast(mid)", fast_dbg_tb, '0', "FASTSLOW");
        assert_eq_sl("slow(mid)", fast_dbg_tb, '0', "FASTSLOW"); -- both 0
      end if;
    end procedure;

    -- local variables for loops
    variable occ : integer := 0;

  begin
    log_info("=== TB START (FIFO + REG + C2 + RAM + FASTSLOW) ===");

    ----------------------------------------------------------------
    -- Reset
    ----------------------------------------------------------------
    req_tb   <= '1';
    din_tb   <= ZERO8;
    reset_tb <= '1'; wait_cycles(2);
    reset_tb <= '0'; wait until rising_edge(clk_tb);
    log_success("Came out of reset");

    ----------------------------------------------------------------
    -- 1) Sanity: single write/read (validates REG + C2 + RAM path)
    ----------------------------------------------------------------
    quick_write(x"5A");  -- write once between HL pulses
    -- wait the next HL and check
    wait until rising_edge(clk_tb);
    while hl_tb = '0' loop
      wait until rising_edge(clk_tb);
    end loop;
    check_read_data("Single write/read with C2");
    -- close HL
    wait until rising_edge(clk_tb);
    while hl_tb = '1' loop
      wait until rising_edge(clk_tb);
    end loop;

    ----------------------------------------------------------------
    -- 2) Burst of 12 writes between HL pulses -> check fast/slow while growing occupancy
    ----------------------------------------------------------------
    -- ensure we are right after a HL (so ENREAD=0 window begins)
    if hl_tb = '1' then
      wait until rising_edge(clk_tb);
    end if;

    occ := 0;
    for i in 0 to 11 loop
      check_fastslow(occ);
      quick_write(std_logic_vector(to_unsigned(i, 8)));
      occ := occ + 1;
      -- ensure we stayed in write-only region
      assert enread_dbg_tb = '0'
        report "[SEQ] ENREAD should remain 0 during the burst (writes only)"
        severity error;
    end loop;

    ----------------------------------------------------------------
    -- 3) Now wait the next 12 HL windows and check the reads match
    ----------------------------------------------------------------
    for k in 1 to 12 loop
      -- wait next HL=1
      wait until rising_edge(clk_tb);
      while hl_tb = '0' loop
        wait until rising_edge(clk_tb);
      end loop;
      -- check RAM read
      assert_eq_sl("cs_n (read)", cs_n_dbg_tb, '0', "SEQ/RAM");
      assert_eq_sl("rw_n (read)", rw_n_dbg_tb, '1', "SEQ/RAM");
      assert_eq_sl("oe   (read)", oe_dbg_tb,   '1', "SEQ/RAM");
      check_read_data("Burst read #" & integer'image(k));

      -- wait HL close
      wait until rising_edge(clk_tb);
      while hl_tb = '1' loop
        wait until rising_edge(clk_tb);
      end loop;
    end loop;

    log_info("=== TB END ===");
    done <= '1';
    wait;
  end process;

end architecture;
