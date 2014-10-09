--	VHDLUnit 
--	Author: Joren Guillaume
--	This is a library intended for use with a python script (test_parser.py) to facilitate testbench development.
--	The functions in the library can be used freely without the script as well.
--	Some elements are inspired by VHDLUnit developed by Przemyslaw Soltan.
--------------------------------------------------------------------------------------------------------------
--	Changelog (YYYY-MM-DD)
--	2014-03-21
--	Initial reportBack, assertVectors, assertAll
--	2014-03-28
--	assertTimed, assertTimedVector, std_logic_vector2string
--	2014-04-08
--	Edited report functions (reportback, report strings of others)
--------------------------------------------------------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;

package vhdlUnit is
--	type vector_array is array (natural range <>) of std_logic_vector;
--	type times is array (natural range <>) of time;
	
	procedure reportBack(b : boolean; result : string);
	
--	procedure assertVectors(signal v : std_logic_vector; va : vector_array; clkperiod : time);
--	procedure assertAll(signal v : std_logic_vector; s : std_logic ; clkperiod : time);
--	--ToDo: Re-asses the naming of these vectors, procedures and types
--	procedure assertTimed (signal v : std_logic; t : times;  sv : std_logic_vector);
--	procedure assertTimedVector (signal v : std_logic_vector; t : times; va : vector_array);
	
	function std_logic_vector2string(arg : in std_logic_vector) return String;

end package;


package body vhdlUnit is

	procedure reportBack(b : boolean;  result : string) is
	begin
		if (b) then
			assert false report "test success" & ht & "name: " & result severity note;
		else
			assert false report "test failed" & ht & "name: " & result severity note;
		end if;
	end procedure;

--	procedure assertVectors(signal v : std_logic_vector; va : vector_array; clkperiod : time) is
--	begin
--		for i in va'range loop
--			if(v = va(i)) then
--				reportBack(true, "assertVectors (" & integer'image(i + 1) & "/" & integer'image(va'length) & ")");
--			else
--				reportBack(false, "assertVectors (" & integer'image(i + 1) & "/" & integer'image(va'length) & ") - expected: " & std_logic_vector2string(va(i)) & " but received: " & std_logic_vector2string(v));
--			end if;
--			wait for clkperiod;
--		end loop;
--	end procedure;
--	
--	procedure assertAll(signal v : std_logic_vector; s : std_logic ; clkperiod : time) is
--	begin
--		for i in v'range loop
--			if(s = v(i)) then
--				reportBack(true, "assertAll (" & integer'image(i + 1) & "/" & integer'image(v'length) & ") - " & std_logic'image(s) & " = " & std_logic'image(v(i)));
--			else
--				reportBack(false, "assertAll (" & integer'image(i + 1) & "/" & integer'image(v'length) & ") - expected: " & std_logic'image(v(i)) & " but received: " & std_logic'image(s));
--			end if;
--			wait for clkperiod;
--		end loop;
--	end procedure;
--	
--	procedure assertTimed (signal v : std_logic; t : times; sv : std_logic_vector) is
--	begin
--		if (t'length = sv'length) then
--			for i in sv'range loop
--				if (v = sv(i)) then
--					reportBack(true, "assertTimed (" & integer'image(i + 1 ) & "/" & integer'image(sv'length) & ") - " & std_logic'image(sv(i)) & " = " & std_logic'image(v));
--				else
--					reportBack(false, "assertTimed (" & integer'image(i + 1) & "/" & integer'image(sv'length) & ") " & " - expected: " & std_logic'image(sv(i)) & " but received: " & std_logic'image(v));
--				end if;
--				wait for t(i);
--			end loop;
--		else
--			reportBack(false, "assertTimed - mismatched dimensions in procedure - expected " & integer'image(t'length) & " but received " & integer'image(sv'length));
--		end if;
--	end procedure;
--	
--	procedure assertTimedVector (signal v : std_logic_vector; t : times; va : vector_array) is
--	begin
--		if (t'length = va'length) then
--			for i in va'range loop
--				if (v = va(i)) then
--					reportBack(true, "assertTimedVector (" & integer'image(i + 1) & "/" & integer'image(va'length) & ") - " & std_logic_vector2string(v));
--				else
--					reportBack(false, "assertTimedVector (" & integer'image(i + 1) & "/" & integer'image(va'length) & ")"  & " - expected: " & std_logic_vector2string(va(i)) & " but received: " & std_logic_vector2string(v));
--				end if;
--				wait for t(i);
--			end loop;
--		else
--			reportBack(false, "assertTimedVector - mismatched dimensions in procedure - expected " & integer'image(t'length) & " but received " & integer'image(va'length));
--		end if;
--	end procedure;				  
	
	------------------------------------------------------------------------
	-- std_logic_vector2string
	-- Modified version of vector2string by Przemyslaw Soltan
	------------------------------------------------------------------------	  
	function std_logic_vector2string(arg : in std_logic_vector) return String is	  
		variable result : string(1 to arg'LENGTH);
		variable j : integer := 1;
	begin			 
		for i in arg'RANGE loop		
			result(j) := std_logic'image(arg(i))(2); --Takes the string image and then the character needed
			j:=j+1;
		end loop;
		return result;
	end std_logic_vector2string;
	
end vhdlUnit;