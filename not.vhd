library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity not_component is 
	port(
		input 	 : in  std_logic;
		result: out std_logic
	);
end not_component;

architecture not_component of not_component is
	signal output : std_logic;
begin 
	output <= not input ;
	result <= output;
end not_component; 