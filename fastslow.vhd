library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.elfifo_pkg.all;  

entity fastslow is
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
end entity;

architecture rtl of fastslow is
  signal en      : std_logic;
  signal ud      : std_logic;
  signal cnt_vec : std_logic_vector(M-1 downto 0);
begin
  -- counter control
  en <= incread xor incwrite;
  ud <= incwrite;

  -- M bit u/d counter
  U_CNT : dcpt_m
    generic map ( M => M )
    port map (
      reset   => Reset,
      ud      => ud,
      clk     => CLK,
      enable  => en,
      cptr  => cnt_vec
    );

  -- fast = 1 if both MSBs are 0
  fast <= cnt_vec(M-1) nor cnt_vec(M-2);


  -- slow = 1 if both MSBs are 1
  slow <= cnt_vec(M-1) and cnt_vec(M-2);

end architecture;
