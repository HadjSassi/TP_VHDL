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

        ack: out std_logic;
        HL: out std_logic;
        
        --genhl dbg
        enread_dbg  : out std_logic;
        enwrite_dbg : out std_logic
    );
end entity FIFO;

architecture rtl of fifo is
  signal enread_s, enwrite_s : std_logic;
  -- unused seq outputs for now
  signal rw_n_s, oe_s, incwrite_s, incread_s, selread_s, cs_n_s : std_logic;
begin
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

  enread_dbg  <= enread_s;
  enwrite_dbg <= enwrite_s;
end architecture;