library IEEE;
use IEEE.STD_LOGIC_1164.all;   
--use work.vhdlUnit.all;
--test


entity architecture_tests is			  
	
end architecture_tests;  


architecture function1 of vhdlUnit_tb is
	signal s : std_logic;
	signal sv: std_logic_vector (3 downto 0);
	signal t : times (3 downto 0);
	constant clkperiod : time := 10 ns;
begin
	testall: process
	begin
		sv <= "0101";
		s <= '1';
		t <= (clkperiod, clkperiod, clkperiod, clkperiod);
		
		wait for clkperiod;
		
		assertVectors(sv, ("0000", "0001", "0010", "0011"), clkperiod);
		assertAll(sv, s, clkperiod);
		assertAll(sv, s, clkperiod);
		assertTimed (s, t, "000000");
		assertTimed (s, t, sv);
		assertTimedVector (sv, t, ("0000", "0001", "0010"));
		assertTimedVector (sv, t, ("0000", "0001", "0010", "0011"));
		
		assert false report std_logic_vector2string(sv) severity note;
		wait;
	end process;
end function1;