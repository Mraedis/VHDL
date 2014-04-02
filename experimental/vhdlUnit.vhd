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
	type vector_array is array (natural range <>) of std_logic_vector;
	type times is array (natural range <>) of time;
	
	procedure reportBack(b : boolean; result : string);
	
	procedure assertVectors(signal v : std_logic_vector; va : vector_array; clkperiod : time);
	procedure assertAll(signal v : std_logic_vector; s : std_logic ; clkperiod : time);
	--ToDo: Re-asses the naming of these vectors, procedures and types
	procedure assertTimed (signal v : std_logic; t : times;  sv : std_logic_vector);
	procedure assertTimedVector (signal v : std_logic_vector; t : times; va : vector_array);
	
	function std_logic_vector2string(arg : in std_logic_vector) return String;

end package;


package body vhdlUnit is

	procedure reportBack(b : boolean;  result : string) is
	begin
		--ToDo: Incorporate result
		if (b) then
			assert false report "test success" & ", result: " & result severity note;
		else
			assert false report "test failed" & ", result: " & result severity note;
		end if;
	end procedure;

	procedure assertVectors(signal v : std_logic_vector; va : vector_array; clkperiod : time) is
	begin
		for i in va'range loop
			if(v = va(i)) then
				reportBack(true, "testnr: " & integer'image(i));
			else
				reportBack(false, "testnr: " & integer'image(i));
			end if;
			wait for clkperiod;
		end loop;
	end procedure;
	
	procedure assertAll(signal v : std_logic_vector; s : std_logic ; clkperiod : time) is
	begin
		for i in v'range loop
			if(s = v(i)) then
				reportBack(true, "testnr: " & integer'image(i));
			else
				reportBack(false, "testnr: " & integer'image(i));
			end if;
			wait for clkperiod;
		end loop;
	end procedure;
	
	procedure assertTimed (signal v : std_logic; t : times; sv : std_logic_vector) is
	begin
		if (t'length = sv'length) then
			for i in sv'range loop
				reportBack(v = sv(i), "testnr: " & integer'image(i) & ", result: " & std_logic'image(v));
				wait for t(i);
			end loop;
		else
			reportBack(false, "mismatched dimensions in procedure assertTimed");
		end if;
	end procedure;
	
	procedure assertTimedVector (signal v : std_logic_vector; t : times; va : vector_array) is
	begin
		if (t'length = va'length) then
			for i in va'range loop
				reportBack(v = va(i), "testnr: " & integer'image(i) & ", result: " & std_logic_vector2string(v));
				wait for t(i);
			end loop;
		else
			reportBack(false, "mismatched dimensions in procedure assertTimedVector");
		end if;
	end procedure;				  
	
	------------------------------------------------------------------------
	-- std_logic_vector2string
	-- Modified version of vector2string by Przemyslaw Soltan
	------------------------------------------------------------------------	  
	function std_logic_vector2string(arg : in std_logic_vector) return String is	  
		variable result : string(1 to arg'LENGTH);
		variable j : integer := 1;
	begin			 
		for i in arg'RANGE loop		
			result(j) := std_logic'image(arg(i))(1); --Takes the string image and then only the first character (which should be the only character)
			j:=j+1;
		end loop;
		return result;
	end std_logic_vector2string;
	
end vhdlUnit;