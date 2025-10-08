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
      adrg_dbg     : out std_logic_vector(3 downto 0)
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
  signal done : std_logic := '0';

  -- Function to convert std_logic_vector(7:0) to hex string
  function to_hex(slv : std_logic_vector(7 downto 0)) return string is
    subtype nibble is std_logic_vector(3 downto 0);
    function nib_to_hex(n : nibble) return character is
      constant chars : string(0 to 15) := "0123456789ABCDEF";
    begin
      return chars(to_integer(unsigned(n)));
    end function;
  begin
    return "" & nib_to_hex(slv(7 downto 4)) & nib_to_hex(slv(3 downto 0));
  end function;

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
      adrg_dbg     => adrg_dbg_tb
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

    -- Procedure to check address change
    procedure check_address_change(expected_inc : boolean) is
      variable prev_adrg : std_logic_vector(M-1 downto 0);
    begin
      prev_adrg := adrg_dbg_tb;
      wait until rising_edge(clk_tb);
      if expected_inc then
        assert adrg_dbg_tb = std_logic_vector(unsigned(prev_adrg) + 1) 
          report "Address did not increment" severity error;
      else
        assert adrg_dbg_tb = prev_adrg 
          report "Address changed unexpectedly" severity error;
      end if;
    end procedure;

    -- Procedure to write data and check read
    procedure write_and_read_data(data_in : std_logic_vector(7 downto 0); expected_complement : std_logic_vector(7 downto 0)) is
    begin
      din_tb <= data_in;
      req_tb <= '0';  -- trigger write
      wait until rising_edge(clk_tb);  -- Ecrire
      check_address_change(true);
      wait until rising_edge(clk_tb);  -- Attente
      wait_hl_pulse(40);  -- Lect
      assert hl_tb = '1' report "HL must be 1 during read" severity error;
      assert dout_tb = expected_complement report "Read data mismatch: expected " & to_hex(expected_complement) & ", got " & to_hex(dout_tb) severity error;
      report "Written: " & to_hex(data_in) & ", Read (complemented): " & to_hex(dout_tb) severity note;
      wait until rising_edge(clk_tb);  -- back to Attente
      req_tb <= '1';  -- leave Attente to Repos
      wait until rising_edge(clk_tb);  -- Repos
    end procedure;

  begin
    -- reset -> Repos (address should be 0)
    req_tb   <= '1';
    din_tb   <= (others => '0');
    reset_tb <= '1';   wait_cycles(2);
    reset_tb <= '0';   wait until rising_edge(clk_tb);
    assert to_integer(unsigned(adrg_dbg_tb)) = 0 report "Address not reset to 0" severity error;

    -- Scenario 1: idle read (empty, dout Z after)
    wait_cycles(20);
    wait_hl_pulse(40);
    assert hl_tb = '1' report "Lect1: HL must be 1" severity error;
    wait until rising_edge(clk_tb); -- back to Repos
    assert dout_tb = "ZZZZZZZZ" report "Dout should be Z after read" severity error;
    assert hl_tb = '0' report "Back to Repos: HL/=0" severity error;

    -- Scenario 2: write 'A' (41), read complemented
    write_and_read_data(x"41", x"BF");  -- ~41 +1 = BF ( -65 )

    -- Additional: write 'B' (42)
    wait_cycles(5);
    write_and_read_data(x"42", x"BE");

    -- Scenario 3: reset during attente
    req_tb <= '0';
    din_tb <= x"43";  -- 'C'
    wait until rising_edge(clk_tb);  -- Ecrire
    check_address_change(true);
    wait until rising_edge(clk_tb);  -- Attente
    wait until rising_edge(clk_tb);
    reset_tb <= '1';
    wait until rising_edge(clk_tb);  -- Repos
    reset_tb <= '0'; req_tb <= '1';
    assert to_integer(unsigned(adrg_dbg_tb)) = 0 report "Address not reset" severity error;

    -- Run longer
    wait_cycles(100);

    report "TB fifo (with RAM) finished OK." severity note;
    done <= '1';
    wait;
  end process;

end architecture;