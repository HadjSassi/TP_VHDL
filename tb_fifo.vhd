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
      incread_dbg  : out std_logic
    );
  end component;

  constant CLK_PERIOD : time := 100 ns;
  constant M : natural := 4;

  signal clk_tb, reset_tb, req_tb : std_logic := '0';
  signal din_tb                   : std_logic_vector(7 downto 0) := (others => '0');
  signal ack_tb, hl_tb            : std_logic;
  signal dout_tb                  : std_logic_vector(7 downto 0);

  signal enread_dbg_tb, enwrite_dbg_tb : std_logic;
  signal adrg_dbg_tb              : std_logic_vector(M-1 downto 0);
  signal selread_dbg_tb           : std_logic;

  signal rw_n_dbg_tb, cs_n_dbg_tb, oe_dbg_tb : std_logic;
  signal incwrite_dbg_tb, incread_dbg_tb : std_logic;

  signal done : std_logic := '0';

  -- TB shadow RAM (mirrors DUT writes when RAM truly writes)
  type ram_type is array (0 to 2**M - 1) of std_logic_vector(7 downto 0);
  signal shadow_ram : ram_type := (others => (others => '0'));

  -- optional expected ptrs
  signal expected_write_ptr : unsigned(M-1 downto 0) := (others => '0');
  signal expected_read_ptr  : unsigned(M-1 downto 0) := (others => '0');

  -- HL edge detect
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
      incread_dbg  => incread_dbg_tb
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
  -- TB model: mirror writes exactly when RAM writes (cs_n=0 & rw_n=0)
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
        -- Real write cycle to RAM:
        if (cs_n_dbg_tb = '0' and rw_n_dbg_tb = '0') then
          addr_i := to_integer(unsigned(adrg_dbg_tb));
          shadow_ram(addr_i) <= din_tb;
          expected_write_ptr <= expected_write_ptr + 1;
          log_info("Mirrored WRITE @ addr=" & integer'image(addr_i) &
                   " data=" & slv_to_hex(din_tb));
        end if;

        -- Read pointer increment modelling (optional)
        if hl_tb = '1' then
          expected_read_ptr <= expected_read_ptr + 1;
        end if;
      end if;
    end if;
  end process;

  --------------------------------------------------------------------
  -- HL ?state? logs + read-mode checks
  --------------------------------------------------------------------
  state_logger : process(clk_tb)
  begin
    if rising_edge(clk_tb) then
      if (hl_prev = '0' and hl_tb = '1') then
        log_state("READ_WINDOW_OPEN (HL=1) addr=" & slv_to_bin(adrg_dbg_tb));
        -- RAM must be enabled for read on this cycle
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

    procedure wait_hl_pulse(max_cycles : natural) is
      variable k : natural := 0;
    begin
      loop
        wait until rising_edge(clk_tb);
        exit when hl_tb = '1';
        k := k + 1;
        assert k <= max_cycles report "Timeout waiting for HL pulse" severity error;
      end loop;
    end procedure;

    procedure wait_read_at_addr(target : std_logic_vector(M-1 downto 0);
                                max_pulses : natural) is
      variable p : natural := 0;
    begin
      loop
        wait_hl_pulse(800);
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
      -- Ensure read-mode signals are correct
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

    procedure request_write(word : std_logic_vector(7 downto 0)) is
    begin
      log_state("REQUEST_WRITE data=" & slv_to_hex(word));
      din_tb <= word;
      req_tb <= '0';  -- Repos + enwrite=1 + req=0 -> Ecrire on next cycle
    end procedure;

    procedure stop_write is
    begin
      req_tb <= '1';
    end procedure;

    procedure wait_ecrire_and_attente is
    begin
      wait until rising_edge(clk_tb);  -- Ecrire (cs_n=0 & rw_n=0)
      wait until rising_edge(clk_tb);  -- Attente
    end procedure;

    variable wr_addr_v : std_logic_vector(M-1 downto 0);

  begin
    log_info("=== TB START (FIFO + RAM) ===");

    -- Reset ? Repos
    req_tb   <= '1';
    din_tb   <= (others => '0');
    reset_tb <= '1'; wait_cycles(2);
    reset_tb <= '0'; wait until rising_edge(clk_tb);
    log_success("Came out of reset");

    ----------------------------------------------------------------
    -- ?Consume? addr 0 so first tested write goes to non-zero addr
    ----------------------------------------------------------------
    request_write(x"55");           -- write @ addr 0 (just to advance)
    wait_ecrire_and_attente;
    stop_write;

    ----------------------------------------------------------------
    -- 1) Write @ non-zero ? read it ? write another ? read it ? later read first again
    ----------------------------------------------------------------
    -- Expect addr 1
    request_write(x"A5");
    wait_ecrire_and_attente;
    stop_write;

    -- Read A5
    wait_read_at_addr("0001", 30);
    check_read_data("Read back A5 (addr=1)");

    -- Expect addr 2
    request_write(x"3C");
    wait_ecrire_and_attente;
    stop_write;

    -- Read 3C
    wait_read_at_addr("0010", 30);
    check_read_data("Read back 3C (addr=2)");

    -- Later, confirm addr 1 still holds A5
    wait_read_at_addr("0001", 60);
    check_read_data("Verify addr=1 still A5");

    ----------------------------------------------------------------
    -- 2) Start write, reset during Attente, later read that address
    --    (write happens in Ecrire before reset, so data persists)
    ----------------------------------------------------------------
    request_write(x"B7");
    wait until rising_edge(clk_tb);  -- Ecrire now
    -- Capture the address we just wrote (effective write address)
    wr_addr_v := adrg_dbg_tb;
    wait until rising_edge(clk_tb);  -- Attente
    log_state("RESET during Attente");
    reset_tb <= '1';
    wait until rising_edge(clk_tb);  -- Repos
    reset_tb <= '0'; req_tb <= '1';

    -- Read back that same address later
    wait_read_at_addr(wr_addr_v, 80);
    check_read_data("Post-reset read of pre-reset write (B7)");

    log_info("=== TB END ===");
    done <= '1';
    wait;
  end process;

end architecture;

