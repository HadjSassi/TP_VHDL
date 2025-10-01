
-- mon_paquetage.vhd
library ieee ;
use ieee.std_logic_1164.all ;
use ieee.std_logic_unsigned.all ;

package mes_fonctions is
function cpl2 (entree†: std_logic_vector†; N†: natural) 
					return std_logic_vector†;
end mes_fonctions†;
-------------------------------------------------------------------------
package body mes_fonctions is
function cpl2 (entree†: std_logic_vector†; N†: natural) 
					return std_logic_vector† is
 
variable temp†: std_logic_vector( N-1 downto 0)†;
begin
temp†:= not entree;
temp†:= temp + '1'†;
return temp†;
end cpl2†;
end mes_fonctions†;
--------------------------------------------------------------------------
library ieee ;
use ieee.std_logic_1164.all ;

package CHECK_PKG is

-- DÈclaration de la procÈdures check_setup. (pre positionnement)
procedure check_setup(
		signal clk 		: in std_logic;
		signal din 		: in std_logic_vector;
			t_setup 	: in time;
			severite	: in severity_level:= warning;
			hdeb 		: in time := time'low;
			hfin		: in time := time'high );
-- DÈclaration de la procÈdures T_hold. (maintien)
procedure check_hold(
		signal clk 		: in std_logic;
		signal din 		: in std_logic_vector;
			t_hold 	: in time;
			severite	: in severity_level:= warning;
			hdeb 		: in time := time'low;
			hfin		: in time := time'high );

end CHECK_PKG;
--------------------------------------------------------------------------
library ieee ;
use ieee.std_logic_1164.all ;

package body CHECK_PKG is		
-- SpÈcification du corps du paquetage.
--la procÈdure check_setup
procedure check_setup(
	signal clk 		: in std_logic;
		signal din 	: in std_logic_vector;
		t_setup 	: in time;
		severite	: in severity_level:= warning;
		hdeb 		: in time := time'low;
		hfin 		: in time := time'high) is
begin
loop
wait on clk;
If clk = '1' and clk'event then
--wait until clk = 'l' and clk'event;
	if now >= hdeb and now <= hfin then
	assert din'last_event >= t_setup and din'event = false
	report "temps de setup non respectÈ" severity severite;
	elsif now > hfin then
	wait;
	end if;
end if;
end loop;	
end check_setup;

procedure check_hold(
		signal clk 		: in std_logic;
		signal din 		: in std_logic_vector;
			t_hold 	: in time;
			severite	: in severity_level:= warning;
			hdeb 		: in time := time'low;
			hfin		: in time := time'high ) is 
begin
--> A completer		--> A completer--> A completer--> A completer 
-- voir annexe énoncé de TP
End check_hold;

End CHECK_PKG;
--------------------------------------------------------------------------


