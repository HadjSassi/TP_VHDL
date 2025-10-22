library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ram_tb is
end entity ram_tb;

architecture tb of ram_tb is
    constant M         : natural := 4;  -- 16 words
    constant N         : natural := 8;  -- 8 bits
    constant CLK_PERIOD: time    := 100 ns;  -- 10 MHz clock

    -- Test signals
    signal clk   : std_logic := '0';
    signal cs_n  : std_logic := '1';
    signal rw_n  : std_logic := '1';
    signal oe    : std_logic := '0';
    signal adr   : std_logic_vector(M-1 downto 0) := (others => '0');
    signal din   : std_logic_vector(N-1 downto 0) := (others => '0');
    signal dout  : std_logic_vector(N-1 downto 0);

    -- Component declaration
    component ram
        generic (
            M : natural;
            N : natural
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

begin
    -- Instantiate the RAM
    uut: ram
        generic map (
            M => M,
            N => N
        )
        port map (
            clk   => clk,
            cs_n  => cs_n,
            rw_n  => rw_n,
            oe    => oe,
            adr   => adr,
            din   => din,
            dout  => dout
        );

    -- Clock generation
    clk_gen : process
    begin
        while true loop
            clk <= '0';
            wait for CLK_PERIOD / 2;
            clk <= '1';
            wait for CLK_PERIOD / 2;
        end loop;
    end process clk_gen;

    -- Stimulus process
    stimulus : process
    begin
        -- Initial state
        cs_n <= '1';
        rw_n <= '1';
        oe   <= '0';
        adr  <= "0000";
        din  <= x"00";
        wait for 20 ns;

        -- Write to address 0 (AA)
        adr <= "0000";
        din <= x"AA";
        cs_n <= '0';
        rw_n <= '0';
        wait for CLK_PERIOD;

        -- Write to address 1 (55)
        adr <= "0001";
        din <= x"55";
        wait for CLK_PERIOD;

        -- Write to address 2 (FF)
        adr <= "0010";
        din <= x"FF";
        wait for CLK_PERIOD;

        -- End write mode
        cs_n <= '1';
        rw_n <= '1';
        wait for 20 ns;

        -- Read from address 0
        adr <= "0000";
        cs_n <= '0';
        rw_n <= '1';
        oe   <= '1';
        wait for 20 ns;
        assert dout = x"AA" report "Error: Expected AA at addr 0" severity error;

        -- Read from address 1
        adr <= "0001";
        wait for 20 ns;
        assert dout = x"55" report "Error: Expected 55 at addr 1" severity error;

        -- Read from address 2
        adr <= "0010";
        wait for 20 ns;
        assert dout = x"FF" report "Error: Expected FF at addr 2" severity error;

        -- Test high-impedance (oe = 0)
        oe <= '0';
        wait for 20 ns;
        assert dout = (dout'range => 'Z') report "Error: Expected high-Z with oe=0" severity error;

        -- Test high-impedance (cs_n = 1)
        cs_n <= '1';
        oe   <= '1';
        wait for 20 ns;
        assert dout = (dout'range => 'Z') report "Error: Expected high-Z with cs_n=1" severity error;

        -- End simulation
        wait for 100 ns;
        report "Testbench completed.";
        wait;
    end process stimulus;

end architecture tb;
