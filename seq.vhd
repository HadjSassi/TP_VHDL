library ieee;
use ieee.std_logic_1164.all;

entity seq is
    port (
        clk      : in  std_logic;
        reset    : in  std_logic;
        enread   : in  std_logic;
        enwrite  : in  std_logic;
        req      : in  std_logic;
        ack      : out std_logic;
        rw_n     : out std_logic;
        oe       : out std_logic;
        incwrite : out std_logic;
        incread  : out std_logic;
        hl       : out std_logic;
        selread  : out std_logic;
        cs_n     : out std_logic
    );
end seq;

architecture archi_moore of seq is
    type state_type is (repos, ecrire, lect1, lect2, attente);
    signal current_state, next_state : state_type := repos;
begin
    process(clk)
    begin
        if rising_edge(clk) then
            if reset = '1' then
                current_state <= repos;
            else
                current_state <= next_state;
            end if;
        end if;
    end process;

    process(current_state, enread, enwrite, req, reset)
    begin
        if reset = '1' then
            next_state <= repos;
        else
            case current_state is
                when repos =>
                    if enread = '1' then
                        next_state <= lect1;
                    elsif enwrite = '1' and req = '1' then
                        next_state <= ecrire;
                    else
                        next_state <= repos;
                    end if;
                when lect1 =>
                    next_state <= lect2;
                when lect2 =>
                    if req = '1' then
                        next_state <= attente;
                    else
                        next_state <= repos;
                    end if;
                when ecrire =>
                    next_state <= repos;
                when attente =>
                    if enwrite = '1' then
                        next_state <= ecrire;
                    else
                        next_state <= repos;
                    end if;
            end case;
        end if;
    end process;

    process(current_state)
    begin
        case current_state is
            when repos =>
                ack <= '1';
                rw_n <= '1';
                oe <= '0';
                incwrite <= '0';
                incread <= '0';
                hl <= '0';
                selread <= '0';
                cs_n <= '1';
            when ecrire =>
                ack <= '1';
                rw_n <= '0';
                oe <= '0';
                incwrite <= '1';
                incread <= '0';
                hl <= '0';
                selread <= '0';
                cs_n <= '0';
            when lect1 =>
                ack <= '0';
                rw_n <= '1';
                oe <= '1';
                incwrite <= '0';
                incread <= '1';
                hl <= '1';
                selread <= '1';
                cs_n <= '0';
            when lect2 =>
                ack <= '0';
                rw_n <= '1';
                oe <= '1';
                incwrite <= '0';
                incread <= '0';
                hl <= '0';
                selread <= '1';
                cs_n <= '0';
            when attente =>
                ack <= '0';
                rw_n <= '1';
                oe <= '0';
                incwrite <= '0';
                incread <= '0';
                hl <= '0';
                selread <= '1';
                cs_n <= '1';
        end case;
    end process;

end archi_moore;

architecture archi_mealy of seq is
    type state_type is (repos, ecrire, lect1, lect2, attente);
    signal current_state, next_state : state_type := repos;
begin
    process(clk)
    begin
        if rising_edge(clk) then
            if reset = '1' then
                current_state <= repos;
            else
                current_state <= next_state;
            end if;
        end if;
    end process;

    process(current_state, enread, enwrite, req, reset)
    begin
        if reset = '1' then
            next_state <= repos;
        else
            case current_state is
                when repos =>
                    if enread = '1' then
                        next_state <= lect1;
                    elsif enwrite = '1' and req = '1' then
                        next_state <= ecrire;
                    else
                        next_state <= repos;
                    end if;
                when lect1 =>
                    next_state <= lect2;
                when lect2 =>
                    if req = '1' then
                        next_state <= attente;
                    else
                        next_state <= repos;
                    end if;
                when ecrire =>
                    next_state <= repos;
                when attente =>
                    if enwrite = '1' then
                        next_state <= ecrire;
                    else
                        next_state <= repos;
                    end if;
            end case;
        end if;
    end process;

    ack <= '1' when ((current_state = repos or current_state = ecrire) and enwrite = '1' and not enread = '1') else '0';
    rw_n <= '1' when (current_state = lect1 or current_state = lect2 or current_state = attente) else '0';
    oe <= '1' when ((current_state = lect1 or current_state = lect2) and enread = '1') else '0';
    incwrite <= '1' when (current_state = ecrire and enwrite = '1') else '0';
    incread <= '1' when (current_state = lect1 and enread = '1') else '0';
    hl <= '1' when ((current_state = lect1 or current_state = lect2) and enread = '1') else '0';
    selread <= '1' when (current_state = lect1 or current_state = lect2 or current_state = attente) else '0';
    cs_n <= '0' when (current_state = ecrire or current_state = lect1 or current_state = lect2) else '1';

end archi_mealy;
