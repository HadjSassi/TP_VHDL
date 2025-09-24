library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity tb_not_component is
end tb_not_component;

architecture behavioral of tb_not_component is
    component not_component
        port(
            input    : in  std_logic;
            result   : out std_logic
        );
    end component;
    
    signal input_tb, result_tb : std_logic;
    
begin
    uut: not_component port map(input_tb, result_tb);
    
    stimulus: process
    begin
        -- Test NOT 0 = 1
        input_tb <= '0';
        wait for 15 ns;
        assert result_tb = '1' report "Error: NOT 0 != 1" severity error;
        
        -- Test NOT 1 = 0
        input_tb <= '1';
        wait for 15 ns;
        assert result_tb = '0' report "Error: NOT 1 != 0" severity error;
        
        -- Test rapide avec transitions
        input_tb <= '0'; wait for 5 ns;
        assert result_tb = '1' report "Error after transition: NOT 0 != 1" severity error;
        
        input_tb <= '1'; wait for 5 ns;
        assert result_tb = '0' report "Error after transition: NOT 1 != 0" severity error;
        
        input_tb <= '0'; wait for 5 ns;
        input_tb <= '1'; wait for 5 ns;
        input_tb <= '0'; wait for 5 ns;
        
        report "NOT component testbench completed successfully!" severity note;
        wait;
    end process;
end behavioral;