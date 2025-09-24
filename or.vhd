library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity or_component is 
	port(
		input1 	 : in  std_logic;
		input2 	 : in  std_logic;
		or_result: out std_logic
	);
end or_component;

architecture or_component of or_component is
	signal output : std_logic;
begin 
	output <= input1 or input2;
	or_result <= output;
end or_component; 