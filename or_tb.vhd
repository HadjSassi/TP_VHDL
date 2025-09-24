library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity tb_or_component is
end tb_or_component;

architecture testbench of tb_or_component is
    -- Component declaration
    component or_component
        port(
            input1     : in  std_logic;
            input2     : in  std_logic;
            or_result  : out std_logic
        );
    end component;
    
    -- Signal declarations
    signal input1_tb    : std_logic;
    signal input2_tb    : std_logic;
    signal or_result_tb : std_logic;
    
begin
    -- Instantiate the Unit Under Test (UUT)
    uut: or_component
        port map(
            input1     => input1_tb,
            input2     => input2_tb,
            or_result  => or_result_tb
        );
    
    -- Stimulus process
    stimulus_process: process
    begin
        -- Test case 1: 0 OR 0 = 0
        input1_tb <= '0';
        input2_tb <= '0';
        wait for 10 ns;
        
        -- Test case 2: 0 OR 1 = 1
        input1_tb <= '0';
        input2_tb <= '1';
        wait for 10 ns;
        
        -- Test case 3: 1 OR 0 = 1
        input1_tb <= '1';
        input2_tb <= '0';
        wait for 10 ns;
        
        -- Test case 4: 1 OR 1 = 1
        input1_tb <= '1';
        input2_tb <= '1';
        wait for 10 ns;
        
        -- Additional test cases with transitions
        input1_tb <= '0';
        input2_tb <= '0';
        wait for 5 ns;
        
        input1_tb <= '1';
        wait for 5 ns;
        
        input2_tb <= '1';
        wait for 5 ns;
        
        input1_tb <= '0';
        wait for 5 ns;
        
        -- End simulation
        wait;
    end process stimulus_process;

end testbench;