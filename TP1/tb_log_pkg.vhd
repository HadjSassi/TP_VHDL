library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package tb_log_pkg is
  -- Toggle ANSI colors in console (if the transcript supports it)
  constant USE_ANSI  : boolean := true;

  -- ANSI escapes (empty when USE_ANSI=false)
  function ansi_green  return string;
  function ansi_yellow return string;
  function ansi_red    return string;
  function ansi_cyan   return string;
  function ansi_reset  return string;

  -- Formatting helpers
  function slv_to_bin(slv : std_logic_vector) return string;
  function slv_to_hex(slv : std_logic_vector) return string;

  -- Log helpers
  procedure log_info(msg : in string);
  procedure log_state(msg : in string);
  procedure log_success(msg : in string);
  procedure log_error(msg : in string);
  procedure log_assert(msg : in string);

  -- Assert helpers (overloads without and with module tag)
  procedure assert_eq_slv(
    constant what     : in string;
    signal   actual   : in std_logic_vector;
    constant expected : in std_logic_vector
  );
  procedure assert_eq_slv(
    constant what     : in string;
    signal   actual   : in std_logic_vector;
    constant expected : in std_logic_vector;
    constant module   : in string
  );

  procedure assert_eq_sl(
    constant what     : in string;
    signal   actual   : in std_logic;
    constant expected : in std_logic
  );
  procedure assert_eq_sl(
    constant what     : in string;
    signal   actual   : in std_logic;
    constant expected : in std_logic;
    constant module   : in string
  );

end package;

package body tb_log_pkg is
  constant ESC : character := character'val(27);

  -- Utility: returns string only when condition is true
  function choose_if(b:boolean; s:string) return string is
  begin
    if b then return s; else return ""; end if;
  end;

  function ansi_green  return string is begin return choose_if(USE_ANSI, ESC & "[32m"); end;
  function ansi_yellow return string is begin return choose_if(USE_ANSI, ESC & "[33m"); end;
  function ansi_red    return string is begin return choose_if(USE_ANSI, ESC & "[31m"); end;
  function ansi_cyan   return string is begin return choose_if(USE_ANSI, ESC & "[36m"); end;
  function ansi_reset  return string is begin return choose_if(USE_ANSI, ESC & "[0m");  end;

  -- Convert SLV to a readable binary string (preserves non-01 std_logic values)
  function slv_to_bin(slv : std_logic_vector) return string is
    variable s : string(1 to slv'length);
    variable i : integer := 1;
  begin
    for idx in slv'range loop
      case slv(idx) is
        when '0' => s(i) := '0';
        when '1' => s(i) := '1';
        when 'U' => s(i) := 'U';
        when 'X' => s(i) := 'X';
        when 'Z' => s(i) := 'Z';
        when 'W' => s(i) := 'W';
        when 'L' => s(i) := 'L';
        when 'H' => s(i) := 'H';
        when '-' => s(i) := '-';
        when others => s(i) := '?';
      end case;
      i := i + 1;
    end loop;
    return s;
  end;

  -- Hex string prefixed with 'x'
  function slv_to_hex(slv : std_logic_vector) return string is
  begin
    return "x" & to_hstring(slv);  -- requires -2008
  end;

  procedure log_info(msg : in string) is
  begin
    report ansi_cyan & "[INFO] " & ansi_reset & msg severity note;
  end;

  procedure log_state(msg : in string) is
  begin
    report ansi_yellow & "[STATE] " & ansi_reset & msg severity note;
  end;

  procedure log_success(msg : in string) is
  begin
    report ansi_green & "[SUCCESS] " & ansi_reset & msg severity note;
  end;

  procedure log_error(msg : in string) is
  begin
    report ansi_red & "[ERROR] " & ansi_reset & msg severity error;
  end;

  procedure log_assert(msg : in string) is
  begin
    report "[ASSERT] " & msg severity note;
  end;

  -- Full versions (with module tag)
  procedure assert_eq_slv(
    constant what     : in string;
    signal   actual   : in std_logic_vector;
    constant expected : in std_logic_vector;
    constant module   : in string
  ) is
  begin
    if actual /= expected then
      log_error(module & ": " & what & " mismatch. exp=" &
                slv_to_hex(expected) & " got=" & slv_to_hex(actual));
    else
      log_success(module & ": " & what & " matched (" & slv_to_hex(actual) & ")");
    end if;
  end;

  procedure assert_eq_sl(
    constant what     : in string;
    signal   actual   : in std_logic;
    constant expected : in std_logic;
    constant module   : in string
  ) is
  begin
    if actual /= expected then
      log_error(module & ": " & what & " mismatch. exp=" &
                std_logic'image(expected) & " got=" & std_logic'image(actual));
    else
      log_success(module & ": " & what & " matched (" & std_logic'image(actual) & ")");
    end if;
  end;

  -- Convenience overloads without module tag (use "TB")
  procedure assert_eq_slv(
    constant what     : in string;
    signal   actual   : in std_logic_vector;
    constant expected : in std_logic_vector
  ) is
  begin
    assert_eq_slv(what, actual, expected, "TB");
  end;

  procedure assert_eq_sl(
    constant what     : in string;
    signal   actual   : in std_logic;
    constant expected : in std_logic
  ) is
  begin
    assert_eq_sl(what, actual, expected, "TB");
  end;

end package body;

