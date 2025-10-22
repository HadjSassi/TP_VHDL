library ieee;
use ieee.std_logic_1164.all;

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

  constant CLK_PERIOD : time := 100 ns;

  signal clk_tb      : std_logic := '0';
  signal reset_tb    : std_logic := '1';
  signal enread_tb   : std_logic := '0';
  signal enwrite_tb  : std_logic := '0';
  signal req_tb      : std_logic := '1';  -- default: no immediate write request

  signal ack_tb      : std_logic;
  signal rw_n_tb     : std_logic;
  signal oe_tb       : std_logic;
  signal incwrite_tb : std_logic;
  signal incread_tb  : std_logic;
  signal hl_tb       : std_logic;
  signal selread_tb  : std_logic;
  signal cs_n_tb     : std_logic;

  signal done        : std_logic := '0';  -- stop the clock for a clean finish

begin
  -- select fsm architecture here
  dut: entity work.seq(archi_moore)
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

  -- controlled clock (stops with done=1)
  clk_proc : process
  begin
    while done = '0' loop
      clk_tb <= '0'; wait for CLK_PERIOD/2;
      clk_tb <= '1'; wait for CLK_PERIOD/2;
    end loop;
    wait;
  end process;

  -- Stimuli and checks
  stimulus : process
    -- helper procedures: wait for N rising edges
    procedure wait_cycles(n : natural) is
    begin
      for i in 1 to n loop
        wait until rising_edge(clk_tb);
      end loop;
    end procedure;

    -- helper: drive enread='1' for 1 clock cycle
    procedure pulse_enread_1clk is
    begin
      enread_tb <= '1';
      wait until rising_edge(clk_tb);
      enread_tb <= '0';
    end procedure;

  begin
    -- set to repos
    enread_tb  <= '0';
    enwrite_tb <= '0';
    req_tb     <= '1';         -- no immediate write
    reset_tb   <= '1';
    wait_cycles(2);
    reset_tb   <= '0';
    wait until rising_edge(clk_tb);  -- now in repos

    -- repos checks (Moore)
    assert ack_tb      = '1' report "Repos: ack/=1" severity error;
    assert rw_n_tb     = '1' report "Repos: rw_n/=1" severity error;
    assert oe_tb       = '0' report "Repos: oe/=0" severity error;
    assert incwrite_tb = '0' report "Repos: incwrite/=0" severity error;
    assert incread_tb  = '0' report "Repos: incread/=0" severity error;
    assert hl_tb       = '0' report "Repos: hl/=0" severity error;
    assert selread_tb  = '0' report "Repos: selread/=0" severity error;
    assert cs_n_tb     = '1' report "Repos: cs_n/=1" severity error;

    -- scenario 1: test enread waiting 200 cycles (as expected from GENHL)
    wait_cycles(200);

    pulse_enread_1clk;               -- enter lect1 on this cycle

    -- check lect1 during the pulse
    assert ack_tb      = '1' report "Lect1: ack/=1" severity error;
    assert rw_n_tb     = '1' report "Lect1: rw_n/=1" severity error;
    assert oe_tb       = '1' report "Lect1: oe/=1" severity error;
    assert incread_tb  = '1' report "Lect1: incread/=1" severity error;
    assert hl_tb       = '1' report "Lect1: hl/=1" severity error;
    assert selread_tb  = '1' report "Lect1: selread/=1" severity error;
    assert cs_n_tb     = '0' report "Lect1: cs_n/=0" severity error;

    wait until rising_edge(clk_tb);  -- back to repos
    assert ack_tb      = '1' report "Back to Repos after Lect1: ack/=1" severity error;
    assert selread_tb  = '0' report "Back to Repos after Lect1: selread/=0" severity error;

    --scenario 2: write, wait x cycles (mimic genhl) and generate read pulse 
    --expected: repos->ecrire->attente->lect2->attente->repos
    -- trigger writing (condition: enread=0, enwrite=1, req=0)
    enread_tb  <= '0';
    enwrite_tb <= '1';
    req_tb     <= '0';
    wait until rising_edge(clk_tb);  -- Ecrire

    -- ecrire checks
    assert rw_n_tb     = '0' report "Ecrire: rw_n/=0" severity error;
    assert incwrite_tb = '1' report "Ecrire: incwrite/=1" severity error;
    assert cs_n_tb     = '0' report "Ecrire: cs_n/=0" severity error;
    assert ack_tb      = '0' report "Ecrire: ack/=0 (Moore)" severity error;

    wait until rising_edge(clk_tb);  -- attente

    -- attente checks
    assert selread_tb  = '1' report "Attente: selread/=1" severity error;
    assert cs_n_tb     = '1' report "Attente: cs_n/=1" severity error;
    assert ack_tb      = '0' report "Attente: ack/=0 (Moore)" severity error;

    -- wait X cycles (< 200 here)
    wait_cycles(50);

    -- one read pulse -> lect2
    pulse_enread_1clk;

    -- lect2 checks
    assert oe_tb       = '1' report "Lect2: oe/=1" severity error;
    assert incread_tb  = '1' report "Lect2: incread/=1" severity error;
    assert hl_tb       = '1' report "Lect2: hl/=1 (errata)" severity error;
    assert selread_tb  = '1' report "Lect2: selread/=1" severity error;
    assert cs_n_tb     = '0' report "Lect2: cs_n/=0" severity error;
    assert ack_tb      = '0' report "Lect2: ack/=0 (Moore)" severity error;

    -- end of pulse → back to attente for 1 cycle
    wait until rising_edge(clk_tb);
    assert selread_tb  = '1' report "Back to Attente: selread/=1" severity error;
    assert cs_n_tb     = '1' report "Back to Attente: cs_n/=1" severity error;

    -- exit attente: enread=0 and req=1 -> repos
    req_tb <= '1';
    wait until rising_edge(clk_tb);  -- repos
    assert ack_tb     = '1' report "Back to Repos after Attente: ack/=1" severity error;
    assert selread_tb = '0' report "Back to Repos after Attente: selread/=0" severity error;

    --scenario 3: test reset interrupting write cycle in attente
    --repos->ecrire->attente->reset->repos with enread still at 1
    -- prepare another write
    enread_tb  <= '0';
    enwrite_tb <= '1';
    req_tb     <= '0';
    wait until rising_edge(clk_tb);  -- ecrire
    wait until rising_edge(clk_tb);  -- attente

    -- synchronous reset while in attente
    reset_tb <= '1';
    wait until rising_edge(clk_tb);  -- on this edge fsm must go to repos
    reset_tb <= '0';

    -- confirm repos right after reset (no enread needed)
    assert ack_tb     = '1' report "Reset from Attente: not Repos (ack/=1)" severity error;
    assert selread_tb = '0' report "Reset from Attente: selread/=0" severity error;
    assert cs_n_tb    = '1' report "Reset from Attente: cs_n/=1" severity error;

    ------------------------------------------------------------------
    report "TB seq: 3 scenarios completed without errors (Moore FSM). For Mealy, 'ack' in Repos state may differ." severity note;
    done <= '1';
    wait;
  end process;

end archi_tb_seq;
