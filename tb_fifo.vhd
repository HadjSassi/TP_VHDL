library ieee;
use ieee.std_logic_1164.all;

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
      ack          : out std_logic;
      hl           : out std_logic;
      enread_dbg   : out std_logic;
      enwrite_dbg  : out std_logic
    );
  end component;

  constant CLK_PERIOD : time := 100 ns;

  signal clk_tb, reset_tb, req_tb : std_logic := '0';
  signal ack_tb, hl_tb            : std_logic;
  signal enread_dbg_tb, enwrite_dbg_tb : std_logic;
  signal done : std_logic := '0';
begin
  -- DUT
  u_dut : fifo
    port map (
      clk          => clk_tb,
      reset        => reset_tb,
      req          => req_tb,
      ack          => ack_tb,
      hl           => hl_tb,
      enread_dbg   => enread_dbg_tb,
      enwrite_dbg  => enwrite_dbg_tb
    );

  -- bind seq architecture here (Moore by default)
  --for u_dut.u_seq: seq use entity work.seq(archi_moore);
  -- for u_dut.u_seq: seq use entity work.seq(archi_mealy);

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
  begin
    -- reset -> Repos
    req_tb   <= '1';   -- no immediate write
    reset_tb <= '1';   wait_cycles(2);
    reset_tb <= '0';   wait until rising_edge(clk_tb);

    -- Scenario 1: idle ~20 cycles then read pulse (Lect1)
    wait_cycles(20);
    wait_hl_pulse(40);
    assert hl_tb = '1' report "Lect1: HL must be 1" severity error;
    wait until rising_edge(clk_tb); -- back to Repos
    assert hl_tb = '0' report "Back to Repos: HL/=0" severity error;

    -- Scenario 2: write -> attente ~20 cycles -> pulse -> lect2 -> attente -> repos
    req_tb <= '0';                                  -- trigger write (Ecrire next)
    wait until rising_edge(clk_tb);                 -- Ecrire
    wait until rising_edge(clk_tb);                 -- Attente
    wait_hl_pulse(40);                             -- Lect2
    assert hl_tb = '1' report "Lect2: HL must be 1" severity error;
    wait until rising_edge(clk_tb);                 -- back to Attente
    req_tb <= '1';                                  -- leave Attente to Repos
    wait until rising_edge(clk_tb);                 -- Repos

    -- Scenario 3: write then reset while in Attente (return to Repos)
    req_tb <= '0';
    wait until rising_edge(clk_tb);                 -- Ecrire
    wait until rising_edge(clk_tb);                 -- Attente
    wait until rising_edge(clk_tb);
    reset_tb <= '1';                                -- sync reset
    wait until rising_edge(clk_tb);                 -- Repos
    reset_tb <= '0'; req_tb <= '1';

    report "TB fifo (GENHL + SEQ) finished OK." severity note;
    done <= '1';
    wait;
  end process;

end architecture;
