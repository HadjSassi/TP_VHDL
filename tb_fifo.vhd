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
      selread_dbg  : out std_logic
    );
  end component;

  constant CLK_PERIOD : time := 100 ns;
  constant M : natural := 4;  -- 16 words

  signal clk_tb, reset_tb, req_tb : std_logic := '0';
  signal din_tb                   : std_logic_vector(7 downto 0) := (others => '0');
  signal ack_tb, hl_tb            : std_logic;
  signal dout_tb                  : std_logic_vector(7 downto 0);
  signal enread_dbg_tb, enwrite_dbg_tb : std_logic;
  signal adrg_dbg_tb              : std_logic_vector(M-1 downto 0);
  signal selread_dbg_tb           : std_logic;
  signal done : std_logic := '0';

  -- Expected pointers (tracked by TB)
  signal expected_write_ptr : unsigned(M-1 downto 0) := (others => '0');
  signal expected_read_ptr  : unsigned(M-1 downto 0) := (others => '0');

  -- TB flag to increment write_ptr at the next rising edge after requesting a write
  signal pending_write : std_logic := '0';

begin
  --------------------------------------------------------------------
  -- DUT
  --------------------------------------------------------------------
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
  -- Expected pointers model (clocked)
  -- - Write: increment on the *next* rising edge after we assert req='0' (pending_write='1').
  -- - Read : increment on the same cycle where HL='1' (Lect1/2).
  --------------------------------------------------------------------
  exp_ptrs_proc : process(clk_tb)
  begin
    if rising_edge(clk_tb) then
      if reset_tb = '1' then
        expected_write_ptr <= (others => '0');
        expected_read_ptr  <= (others => '0');
        pending_write      <= '0';
      else
        -- consume pending write (this edge is Ecrire)
        if pending_write = '1' then
          expected_write_ptr <= expected_write_ptr + 1;
          pending_write      <= '0';
        end if;

        -- read increments on HL=1 (Lect1/2)
        if hl_tb = '1' then
          expected_read_ptr <= expected_read_ptr + 1;
        end if;
      end if;
    end if;
  end process;

  --------------------------------------------------------------------
  -- Stimuli
  --------------------------------------------------------------------
  stim : process
    -- wait N rising edges
    procedure wait_cycles(n : natural) is
    begin
      for i in 1 to n loop
        wait until rising_edge(clk_tb);
      end loop;
    end procedure;

    -- wait until HL=1 (with safety cap)
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

    -- Compare adrg against expected pointer selected by selread
    procedure check_adrg(msg : string) is
      variable exp_adrg : std_logic_vector(M-1 downto 0);
    begin
      if selread_dbg_tb = '1' then
        exp_adrg := std_logic_vector(expected_read_ptr);
      else
        exp_adrg := std_logic_vector(expected_write_ptr);
      end if;

      assert adrg_dbg_tb = exp_adrg
        report msg & ": ADRG mismatch. selread=" & std_logic'image(selread_dbg_tb) &
               " exp=" & integer'image(to_integer(unsigned(exp_adrg))) &
               " got=" & integer'image(to_integer(unsigned(adrg_dbg_tb)))
        severity error;
    end procedure;

    -- Request a write from Repos (will set pending_write so the model increments next edge)
    procedure request_write is
    begin
      -- By spec: Repos -> Ecrire when enread=0 and enwrite=1 and req=0.
      -- Here we assume we're in Repos and enread_dbg_tb='0'.
      req_tb       <= '0';
      pending_write <= '1';   -- TB model: increment expected_write_ptr on next rising edge
    end procedure;

  begin
    ----------------------------------------------------------------
    -- Reset -> Repos
    ----------------------------------------------------------------
    req_tb    <= '1';            -- no immediate write
    din_tb    <= (others => '0');
    reset_tb  <= '1'; wait_cycles(2);
    reset_tb  <= '0'; wait until rising_edge(clk_tb);  -- now in Repos

    -- First check (Repos: selread=0)
    check_adrg("After reset");
    assert hl_tb = '0' report "After reset: HL must be 0" severity error;

    ----------------------------------------------------------------
    -- Scenario 1: Idle ~200 cycles, then read pulse (Lect1) -> Repos
    ----------------------------------------------------------------
    wait_cycles(200);            -- idle while GENHL runs
    wait_hl_pulse(400);          -- pulse arrives (Lect1)
    check_adrg("Lect1 (during HL=1)");
    wait until rising_edge(clk_tb);  -- back to Repos
    check_adrg("Back to Repos after Lect1");
    assert hl_tb = '0' report "Back to Repos: HL/=0" severity error;

    ----------------------------------------------------------------
    -- Scenario 2: Write -> Attente -> (HL pulse) Lect2 -> Attente -> Repos
    ----------------------------------------------------------------
    request_write;                    -- ask for a write; next edge is Ecrire
    wait until rising_edge(clk_tb);   -- Ecrire (incwrite happens; TB model increments here)
    check_adrg("Ecrire");
    wait until rising_edge(clk_tb);   -- Attente
    check_adrg("Attente before read");

    wait_hl_pulse(400);               -- Lect2 pulse
    check_adrg("Lect2 (during HL=1)");
    wait until rising_edge(clk_tb);   -- back to Attente for 1 cycle
    check_adrg("Back to Attente after Lect2");
    req_tb <= '1';                    -- leave Attente to Repos (per spec: enread=0 & req=1)
    wait until rising_edge(clk_tb);   -- Repos
    check_adrg("Back to Repos after Attente");
    assert hl_tb = '0' report "Repos after Attente: HL/=0" severity error;

    ----------------------------------------------------------------
    -- Scenario 3: Write then reset while in Attente -> Repos
    ----------------------------------------------------------------
    request_write;                    -- request another write
    wait until rising_edge(clk_tb);   -- Ecrire
    wait until rising_edge(clk_tb);   -- Attente
    reset_tb <= '1';
    wait until rising_edge(clk_tb);   -- synchronous reset -> Repos
    reset_tb <= '0'; req_tb <= '1';
    -- Expected pointers model was reset as well; ADRG should be 0 with selread=0
    assert to_integer(unsigned(adrg_dbg_tb)) = 0
      report "After reset in Attente: ADRG must be 0 (Repos/write_ptr)" severity error;

    ----------------------------------------------------------------
    report "TB fifo (GENHL + SEQ + GENADR) completed successfully." severity note;
    done <= '1';
    wait;
  end process;

end architecture;

