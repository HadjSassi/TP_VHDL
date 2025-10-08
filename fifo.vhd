library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.elfifo_pkg.all; 

entity FIFO is
    port(
        clk : in std_logic;
        reset: in std_logic;
        req: in std_logic;
        din : in std_logic_vector(7 downto 0);  -- Data input (unused without RAM)

        ack: out std_logic;
        HL: out std_logic;
        dout : out std_logic_vector(7 downto 0);  -- Always Z without RAM
        
        --genhl dbg
        enread_dbg  : out std_logic;
        enwrite_dbg : out std_logic;
        -- genadr dbg
        adrg_dbg    : out std_logic_vector(3 downto 0);
        selread_dbg : out std_logic  -- Added for verification
    );
end entity FIFO;

architecture rtl of fifo is
  signal enread_s, enwrite_s : std_logic;
  -- seq outputs
  signal rw_n_s, oe_s, incwrite_s, incread_s, selread_s, cs_n_s : std_logic;
  -- genadr signal
  signal adrg_s : std_logic_vector(3 downto 0);  -- M=4 for 16 words
begin
  -- dout always Z without RAM
  dout <= (others => 'Z');

  -- GENHL
  u_genhl : GENHL
    port map (
      CLK     => clk,
      RESET   => reset,
      ENREAD  => enread_s,
      ENWRITE => enwrite_s
    );

  -- SEQ (change here if moore or mealy)
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
      hl       => hl,
      selread  => selread_s,
      cs_n     => cs_n_s
    );

  -- GENADR
  u_genadr : genadr
    generic map (
      M => 4
    )
    port map (
      reset    => reset,
      clk      => clk,
      incwrite => incwrite_s,
      incread  => incread_s,
      selread  => selread_s,
      adrg     => adrg_s
    );

  enread_dbg   <= enread_s;
  enwrite_dbg  <= enwrite_s;
  adrg_dbg     <= adrg_s;
  selread_dbg  <= selread_s;  -- Debug for selread
end architecture;