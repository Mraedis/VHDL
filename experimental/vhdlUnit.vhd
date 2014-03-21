--	VHDLUnit 
--	Author: Joren Guillaume
--	This is a library intended for use with a python script (test_parser.py) to facilitate testbench development.
--	The functions in the library can be used freely without the script as well.
--	Some elements are inspired by VHDLUnit developed by Przemyslaw Soltan.
--------------------------------------------------------------------------------------------------------------
--	Changelog (YYYY-MM-DD)
--	2014-03-21
--	Initial reportBack, assertVectors, assertAll
--------------------------------------------------------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;

package vhdlUnit is
	type vector_array is array (integer range <>) of std_logic_vector;
	
	function reportBack(boolean : b; string : result) return string;
	
	procedure assertVectors(signal s : std_logic_vector; v : vector_array; clk : std_logic);
	procedure assertAll(signal v : std_logic_vector; s : std_logic ; clk : std_logic);

end package;


package body vhdlUnit is

	function reportBack(boolean : b; string : result) return string is
	begin
		--ToDo: Incorporate result
		if (b) then
			return "test success";
		else
			return "test failed";
		end if;
	end function;

	procedure assertVectors(signal s : std_logic_vector; v : vector_array; clk : std_logic) is
	begin
		for i in v'range loop
			if(s = v(i)) then
				reportBack(true, "testnr: " & i);
			else
				reportBack(false, "testnr: " & i);
			end if;
			wait for rising_edge(clk);
		end loop;
	end procedure;
	
	procedure assertAll(signal v : std_logic_vector; s : std_logic ; clk : std_logic) is
	begin
		for i in v'range loop
			if(s = v(i)) then
				reportBack(true, "testnr: " & i);
			else
				reportBack(false, "testnr: " & i);
			end if;
			wait for rising_edge(clk);
		end loop;
	end procedure;
	
end vhdlUnit;