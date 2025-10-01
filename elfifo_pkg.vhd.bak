library IEEE;
use IEEE.std_logic_1164.all;
use ieee.numeric_std.all;

package elfifo_pkg is

    component ram is
    generic (
        M : natural := 4;
        N : natural := 8
    );
    port (
        clk   : in  std_logic;
        cs_n  : in  std_logic;
        rw_n  : in  std_logic;
        oe    : in  std_logic;
        adr   : in  std_logic_vector(M-1 downto 0);
        din   : in  std_logic_vector(N-1 downto 0);
        dout  : out std_logic_vector(N-1 downto 0)
    );
    end component ram;

    component dcpt_m is
    generic (
        M : natural := 8
    );
    port (
        reset : in std_logic;
        ud  : in  std_logic;
        clk    : in  std_logic;
        enable    : in  std_logic;
        cptr: out std_logic_vector(M-1 downto 0)
    );
    end component dcpt_m;

    component fastslow is
    generic ( 
        M : natural := 8 
    );
    port (
        Reset    : in  std_logic;
        CLK      : in  std_logic;
        incread  : in  std_logic;
        incwrite : in  std_logic;
        fast     : out std_logic;
        slow     : out std_logic
    );
    end component fastslow;

    component GENHL is
    port (
        CLK   : in  std_logic;
        RESET : in  std_logic;
        ENREAD : out std_logic;
        ENWRITE : out std_logic
    );
    end component GENHL;

end package elfifo_pkg;


package body elfifo_pkg is

end package body;
