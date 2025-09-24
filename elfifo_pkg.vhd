library IEEE;
use IEEE.std_logic_1164.all;

package elfifo_pkg is

    component dcpt_m is
    generic (
        M : natural := 8
    );
    port (
        reset : in std_logic;
        ud  : in  std_logic;
        clk    : in  std_logic;
        enable    : in  std_logic;
        dc_out: out std_logic_vector(M-1 downto 0);
    );
    end component dcpt_m;

end package elfifo_pkg;


package body elfifo_pkg is
    
end package body;
