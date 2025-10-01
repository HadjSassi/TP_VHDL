-- File: tb_complement_a_2.vhd
-- Description: Testbench for complement_a_2 component.
-- Tests various inputs, verifies 2's complement output.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;  -- For conversion if needed
use std.textio.all;

use work.mes_fonctions.all;  -- For cpl2 if direct test, but uses entity

entity tb_complement_a_2 is
end tb_complement_a_2;

architecture archi_tb_complement_a_2 of tb_complement_a_2 is
    component complement_a_2
        generic (N : integer);
        port (
            nombre : in std_logic_vector(N-1 downto 0);
            sortie : out std_logic_vector(N-1 downto 0)
        );
    end component;

    constant N_BITS : integer := 8;
    signal nombre_tb : std_logic_vector(N_BITS-1 downto 0) := (others => '0');
    signal sortie_tb : std_logic_vector(N_BITS-1 downto 0);

begin
    -- Instantiate DUT
    dut: complement_a_2
        generic map (N => N_BITS)
        port map (
            nombre => nombre_tb,
            sortie => sortie_tb
        );

    -- Stimulus process
    stimulus: process
        variable expected : std_logic_vector(N_BITS-1 downto 0);
    begin
        -- Test positive numbers
        nombre_tb <= "00000001";  -- 1 -> -1 = 11111111
        expected := cpl2(nombre_tb, N_BITS);
        wait for 1 ns;
        assert sortie_tb = expected
            report "Mismatch for input " & integer'image(to_integer(unsigned(nombre_tb)))
            severity error;

        nombre_tb <= "00000101";  -- 5 -> -5 = 11111011
        expected := cpl2(nombre_tb, N_BITS);
        wait for 1 ns;
        assert sortie_tb = expected
            report "Mismatch for input " & integer'image(to_integer(unsigned(nombre_tb)))
            severity error;

        -- Test zero
        nombre_tb <= "00000000";  -- 0 -> 0
        expected := cpl2(nombre_tb, N_BITS);
        wait for 1 ns;
        assert sortie_tb = expected severity error;

        -- Test negative (but input is unsigned, but function works on bits)
        nombre_tb <= "11111111";  -- All 1s -> 1 (but 2's comp of -1 is 1)
        expected := cpl2(nombre_tb, N_BITS);
        wait for 1 ns;
        assert sortie_tb = expected
            report "Mismatch for input " & integer'image(to_integer(unsigned(nombre_tb)))
            severity error;

        -- More tests...
        for i in 0 to 255 loop
            nombre_tb <= std_logic_vector(to_unsigned(i, N_BITS));
            expected := cpl2(nombre_tb, N_BITS);
            wait for 1 ns;
            assert sortie_tb = expected
                report "Mismatch for input " & integer'image(i)
                severity error;
        end loop;

        report "Testbench completed successfully" severity note;
        wait;
    end process;

end archi_tb_complement_a_2;