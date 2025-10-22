library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.elfifo_pkg.all;

entity FIFO is
  port(
    clk   : in  std_logic;
    reset : in  std_logic;
    req   : in  std_logic;
    din   : in  std_logic_vector(7 downto 0);

    ack   : out std_logic;
    HL    : out std_logic;
    dout  : out std_logic_vector(7 downto 0);

    -- debug (GENHL / GENADR / SEQ)
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

    -- fast/slow (exposed)
    fast         : out std_logic;
    slow         : out std_logic
  );
end entity FIFO;

architecture rtl of FIFO is
  -- GENHL
  signal enread_s, enwrite_s : std_logic;

  -- SEQ
  signal rw_n_s, oe_s, incwrite_s, incread_s, selread_s, cs_n_s : std_logic;

  -- GENADR
  signal adrg_s : std_logic_vector(3 downto 0);  -- M=4

  -- REG + C2
  signal din_reg_s : std_logic_vector(7 downto 0);
  signal din_c2_s  : std_logic_vector(7 downto 0);

  -- RAM
  signal dout_s : std_logic_vector(7 downto 0);

  -- FAST/SLOW
  signal fast_s, slow_s : std_logic;
begin
  -- GENHL
  u_genhl : GENHL
    port map (
      CLK     => clk,
      RESET   => reset,
      ENREAD  => enread_s,
      ENWRITE => enwrite_s
    );

  -- SEQ (Moore)
  u_seq : entity work.seq(archi_moore)
    port map (
      clk      => clk,
      reset    => reset,
      enread   => enread_s,
      enwrite  => enwrite_s,
      req      => req,
      ack      => ack,
      rw_n     => rw_n_s,
      oe       => oe_s,
      incwrite => incwrite_s,
      incread  => incread_s,
      hl       => HL,
      selread  => selread_s,
      cs_n     => cs_n_s
    );

  -- Address generator
  u_genadr : genadr
    generic map ( M => 4 )
    port map (
      reset    => reset,
      clk      => clk,
      incwrite => incwrite_s,
      incread  => incread_s,
      selread  => selread_s,
      adrg     => adrg_s
    );

  -- Input register (with setup/hold checks via CHECK_PKG)
  u_reg : reg_n
    generic map ( N => 8 )
    port map (
      reset => reset,
      clk   => clk,
      ent   => din,
      sort  => din_reg_s
    );

  -- Two's complement of registered input
  u_c2 : complement_a_2
    generic map ( N => 8 )
    port map (
      nombre => din_reg_s,
      sortie => din_c2_s
    );

  -- RAM gets the complemented value
  u_ram : ram
    generic map ( M => 4, N => 8 )
    port map (
      clk  => clk,
      cs_n => cs_n_s,
      rw_n => rw_n_s,
      oe   => oe_s,
      adr  => adrg_s,
      din  => din_c2_s,
      dout => dout_s
    );

  -- Fast/Slow
  u_fastslow : fastslow
    generic map ( M => 4 )
    port map (
      Reset    => reset,
      CLK      => clk,
      incread  => incread_s,
      incwrite => incwrite_s,
      fast     => fast_s,
      slow     => slow_s
    );

  -- Outputs
  dout <= dout_s;
  fast <= fast_s;
  slow <= slow_s;

  -- Debug
  enread_dbg   <= enread_s;
  enwrite_dbg  <= enwrite_s;
  adrg_dbg     <= adrg_s;
  selread_dbg  <= selread_s;

  rw_n_dbg     <= rw_n_s;
  cs_n_dbg     <= cs_n_s;
  oe_dbg       <= oe_s;
  incwrite_dbg <= incwrite_s;
  incread_dbg  <= incread_s;
end architecture rtl;
