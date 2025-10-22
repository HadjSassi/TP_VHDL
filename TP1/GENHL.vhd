library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.elfifo_pkg.all;  

entity GENHL is
  port (
    CLK     : in  std_logic;
    RESET   : in  std_logic;   
    ENREAD  : out std_logic;
    ENWRITE : out std_logic
  );
end entity GENHL;

architecture behavioral of GENHL is
  signal counter         : std_logic_vector(7 downto 0);
  signal enable_counter  : std_logic := '1';
  constant MAX_COUNT     : integer := 199;  -- generates a 1-cycle pulse when counter == 199
  signal tick200 : std_logic;
  signal enread_r  : std_logic := '0';
  signal enwrite_r : std_logic := '0';

begin

  tick200 <= '1' when unsigned(counter) = MAX_COUNT else '0'; -- clears counter with reset at 200
  
  ENREAD  <= enread_r;
  ENWRITE <= enwrite_r;

  counter_inst : dcpt_m
    generic map ( M => 8 )
    port map (
      clk    => CLK,
      reset  => (RESET or tick200),     
      ud     => '1',
      enable => enable_counter,
      cptr   => counter
    );

  process (CLK)
  begin
    if rising_edge(CLK) then
      if RESET = '1' then
        enread_r  <= '0';
        enwrite_r <= '0';
      else
        if unsigned(counter) = MAX_COUNT then
          enread_r  <= '1';   -- 1-cycle read window
          enwrite_r <= '0';
        else
          enread_r  <= '0';
          enwrite_r <= '1';
        end if;
      end if;
    end if;
  end process;
  
end architecture behavioral;
