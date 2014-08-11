library IEEE;
use IEEE.STD_LOGIC_1164.all;  


entity test_parser_tests is			  
	
end test_parser_tests;  


architecture testbench of test_parser_tests is

	procedure test_een(signal s : std_logic) is
	begin
	end test_een;
	--test
	
	procedure test_twee(signal s : std_logic) is
	begin
	end procedure;
	
	function drie(signal s : std_logic) return std_logic is
	begin
		return '0';
	end function;
	
	function test_vier(signal s : std_logic) return std_logic is
	begin
		return '0';
	end test_vier;
	
	procedure test_vijf(signal s : std_logic) is
	begin
	end test_vijf;
	
	signal v : std_logic := '1';
	
begin
	testall: process
	begin
		test_een(v);
		test_twee(v);
		v <= drie(v);
		v <= test_vier(v);
		test_vijf(v);
		wait;
	end process;
end testbench;