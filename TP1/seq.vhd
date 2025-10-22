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

-- moore state machine
architecture archi_moore of seq is
  type state_type is (repos, ecrire, lect1, lect2, attente);
  signal current_state, next_state : state_type := repos;
begin
  -- state register (sync)
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

  -- next state decoder
  process(current_state, enread, enwrite, req)
  begin
    next_state <= current_state;  -- default
    case current_state is
      when repos =>
        if enread = '1' then
          next_state <= lect1;
        elsif (enread = '0' and req = '0' and enwrite = '1') then
          next_state <= ecrire;
        else
          next_state <= repos;
        end if;

      when ecrire =>
        next_state <= attente;  

      when attente =>
        if enread = '1' then
          next_state <= lect2;
        elsif (enread = '0' and req = '1') then
          next_state <= repos;
        else  -- enread=0 and req=0
          next_state <= attente;
        end if;

      when lect1 =>
        next_state <= repos;    

      when lect2 =>
        next_state <= attente;  
    end case;
  end process;

  -- per state outputs
  process(current_state)
  begin
    -- defaults
    ack      <= '0';
    rw_n     <= '1';
    oe       <= '0';
    incwrite <= '0';
    incread  <= '0';
    hl       <= '0';
    selread  <= '0';
    cs_n     <= '1';

    case current_state is
      when repos =>
        ack      <= '1';
        selread  <= '0';
        cs_n     <= '1';

      when ecrire =>
        rw_n     <= '0';
        incwrite <= '1';
        cs_n     <= '0';

      when lect1 =>
        ack      <= '1';
        oe       <= '1';
        incread  <= '1';
        hl       <= '1';
        selread  <= '1';
        cs_n     <= '0';

      when lect2 =>
        oe       <= '1';
        incread  <= '1';  
        hl       <= '1';  
        selread  <= '1';
        cs_n     <= '0';

      when attente =>
        selread  <= '1';
        cs_n     <= '1';
    end case;
  end process;

end archi_moore;

--mealy fsm
architecture archi_mealy of seq is
  type state_type is (repos, ecrire, lect1, lect2, attente);
  signal current_state, next_state : state_type := repos;
begin
  -- state register
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

  -- next state decoder
  process(current_state, enread, enwrite, req)
  begin
    next_state <= current_state;
    case current_state is
      when repos =>
        if enread = '1' then
          next_state <= lect1;
        elsif (enread = '0' and req = '0' and enwrite = '1') then
          next_state <= ecrire;
        else
          next_state <= repos;
        end if;

      when ecrire =>
        next_state <= attente;

      when attente =>
        if enread = '1' then
          next_state <= lect2;
        elsif (enread = '0' and req = '1') then
          next_state <= repos;
        else
          next_state <= attente;
        end if;

      when lect1 =>
        next_state <= repos;

      when lect2 =>
        next_state <= attente;
    end case;
  end process;

  -- outputs per state
  process(current_state, enread, enwrite, req)
  begin
    -- defaults
    ack      <= '0';
    rw_n     <= '1';
    oe       <= '0';
    incwrite <= '0';
    incread  <= '0';
    hl       <= '0';
    selread  <= '0';
    cs_n     <= '1';

    case current_state is
      when repos =>
        if not (enread = '0' and req = '0' and enwrite = '1') then
          ack <= '1';
        end if;
        selread <= '0';
        cs_n    <= '1';

      when ecrire =>
        rw_n     <= '0'; incwrite <= '1'; cs_n <= '0';

      when lect1 =>
        ack      <= '1';
        oe       <= '1'; incread <= '1'; hl <= '1';
        selread  <= '1'; cs_n <= '0';

      when lect2 =>
        oe       <= '1'; incread <= '1'; hl <= '1';
        selread  <= '1'; cs_n <= '0';

      when attente =>
        selread  <= '1'; cs_n <= '1';
    end case;
  end process;

end archi_mealy;
