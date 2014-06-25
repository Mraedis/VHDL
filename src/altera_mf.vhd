--
-- Copyright (C) 1988-2002 Altera Corporation
--
-- Any megafunction design, and related net list (encrypted or decrypted),
-- support information, device programming or simulation file, and any
-- other associated documentation or information provided by Altera or a
-- partner under Altera's Megafunction Partnership Program may be used only
-- to program PLD devices (but not masked PLD devices) from Altera.  Any
-- other use of such megafunction design, net list, support information,
-- device programming or simulation file, or any other related
-- documentation or information is prohibited for any other purpose,
-- including, but not limited to modification, reverse engineering, de-
-- compiling, or use with any other silicon devices, unless such use is
-- explicitly licensed under a separate agreement with Altera or a
-- megafunction partner.  Title to the intellectual property, including
-- patents, copyrights, trademarks, trade secrets, or maskworks, embodied
-- in any such megafunction design, net list, support information, device
-- programming or simulation file, or any other related documentation or
-- information provided by Altera or a megafunction partner, remains with
-- Altera, the megafunction partner, or their respective licensors.  No
-- other licenses, including any licenses needed under any third party's
-- intellectual property, are provided herein.
----------------------------------------------------------------------------
----------------------------------------------------------------------------
-- ALtera Megafunction Simulation File
--
-- Version QuartusII 2.0 Date: 01/30/02
--
-- 01/30/02: Changed FILE construct to VHDL93 syntax.
-- 01/17/02: FIX SPR 97893
-- 01/15/02: FIX SPR 97167
-- 01/10/02: FIX SPR 97052, 93986
-- 01/08/02: FIX SPR 97388,97142
-- 12/20/01: FIX SPR 96540
-- 12/19/01: FIX SPR 93986 (altcdr)
-- 12/19/01: FIX SPR 87062 on altddio_in: clken affects datain_latched
-- 12/12/01: FIX SPR 96099
-- 12/08/01: FIX SPR 95901
-- 12/06/01: FIX SPR 95636
-- 12/05/01: FIX SPR 95215
-- 12/03/01: FIX SPR 87930, 95192, 95097, 94725
-- 11/30/01: FIX SPR 94914 (altmult_add)
-- 11/27/01: FIX SPR 94963.
----------------------------------------------------------------------------
-- Version QuartusII 1.1  Date: 09/05/01
--
-- 09/05/01: FIX SPR 88972. hssi_rx rlv port.
-- 06/28/01: FIX SPR 87056, 86994.
-- 06/22/01: FIX SPR 86046. Quadport RAM.
-- 06/20/01: FIX SPR 86557, 86256.
-- 06/08/01: FIX SPR 86044. Changes to altqpram.
-- 05/31/01: FIX SPR 85741, 85492. Chenges to altddio_in, and altddio_bidir.
-- 05/18/01: FIX SPR 81990, 84409, 84411, 84564, 84719.
-- 05/07/01: FIX SPR 84223. Fix to apexii altlvds_tx centre_align_msb=on.
-- 05/02/01: FIX SPR 83134. Added APEXII support to altddio_out, and bidir.
--
----------------------------------------------------------------------------
-- 04/27/01: FIX SPR 82919. Corrected the output behavior to include DDIO latch.
-- 04/27/01: FIX SPR 83137. Chnaged the default for run_legth in altcdr_rx.
----------------------------------------------------------------------------
-- Version QuartusII 1.1  Date: 04/25/01
--
-- 04/25/01: Fix SPR83679. Adding APEXII support for altlvds_rx/tx.
-- 04/20/01: Fix SPR83438. altqpram QUAD_PORT and DUAL_PORT problems.
-- 04/19/01: Added alt3pram behavioral model.
-- 04/10/01: Add "use_eab" to altdpram.
-- 03/19/01: Fix SPR81903; altclklock out of phase under some conditions.
-- 03/14/01: Merging SP1 fixes for altcdr and altddio.
-- 03/07/01: Correct behavior of altcam aclr on output reg when clocked by
--           "INCLOCK".
----------------------------------------------------------------------------
-- Version QuartusII 1.0 SP1 Date: 03/05/01
--
-- 03/05/01: SPR 81378: Fix altcam modelling when both output_reg="INCLOCK".
-- 03/05/01: Refix SPR 80417. This also involves a minor improvement to
--           altclklock.
-- 02/28/01: Added intended device family paramater to altddio.
-- 02/12/01: FIX SPR 80417 for altlvds.
----------------------------------------------------------------------------
-- Version QuartusII 1.0 Date: 01/09/01
--
-- 01/09/01: Fix SPR 79392 for altcdr_rx.
-- 01/05/01: Default value of altlvds_tx outclock_divide_by set to zero.
-- 01/04/01: Fix sprs 79179 & 79181 for altddio and altcdr.
--           Also added missing lcell, global, carry and cascade
-- 12/11/00: During SR78303 found a variable initialization VSS problem.
-- 12/05/00: Fix SPR78085. Fix altcam, altdpram and altqpram to support
--           width >32 bit hex files.
-- 12/05/00: Fix SPR77901. Modified altcam warning messages.
-- 11/27/00: Fix SPR75474. Add tx_pll_enable and rx_pll_enable to altlvds_tx
--           and altlvds_rx respectively.
-- 11/27/00: Fix SPR75144. Mercury altlvds_tx and new port tx_coreclock.
-- New components:
--      alt_exc_dpram, alt_exc_upcore, altdido_in, altdido_out,
--      altdido_bidir, altcdr_rx, altcdr_tx
----------------------------------------------------------------------------
-- Version 1.01 Date: 11/08/00
--
-- 11/08/00: Added alt_exc_dpram and alt_exc_upcore.
-- 11/03/00: Fixed altqpram to behave correctly.
-- 10/19/00: Allow write-on-pattern-change for altcam when pattern is
--           unregistered. And provide workaround for when mstart and mnext
--           are unused in fast multiple mode. See SPR76920. 
-- 10/04/00: Fix altclklock valid_lock_cycles counting when inclock starts
--           off low. SPR77687.
-- 10/03/00: Fix altclklock. SPR76627.
-- 09/22/00: Fixed altqpram to get input data at rising clock edge and
--           write data at falling clock edge.
-- 09/20/00: Changed altdpram to output 0 when rden is low for APEX20K
--           device family.  Output remains when rden is low for other
--           device families.
-- 09/19/00: Fixed altdpram to get input data at rising clock edge and
--           write data at falling clock edge.
-- 08/22/00: Added synchronous mode to dcfifo.
-- 08/21/00: Added default values to aclr and sclr of dcfifo and scfifo.
-- 08/18/00: Added INTENDED_DEVICE_FAMILY parameter to altdpram.
-- 08/14/00: Valid values for *_ACLR_* parameters of altqpram have changed
--           from ON and OFF to INACLR_A (or INACLR_B) and NONE
--           respectively.
-- 08/14/00: SPR 74650
--           Removed several internal-use only parameters (MAXIMUM_DEPTH,
--           CASCADE_CHAIN, DEVICE_FAMILY, DATA_INTERLEAVE_OFFSET_IN_BITS,
--           and DATA_INTERLEAVE_WIDTH_IN_BITS) from altqpram so that
--           Quartus won't behave strangely.  Also removed DEVICE_FAMILY
--           and OPTIMIZE_FOR_SPEED parameters from scfifo.
-- 08/11/00: dcfifo and scfifo are fully implemented.
-- 08/03/00: Added default values to all altqpram ports and parameters.
-- 08/02/00: Removed MAXIMIZE_SPEED, ALMOST_FULL_VALUE, ALMOST_EMPTY_VALUE,
--           and ALLOW_RWCYCLE_WHEN_FULL parameters from dcfifo.  Added
--           LPM_HINT parameter to dcfifo.
-- 08/02/00: Removed MAXIMIZE_SPEED parameter from scfifo.
-- 07/29/00: Added WRADDRESS_ACLR_A, WRADDRESS_ACLR_B, and LPM_HINT
--           parameters to altqpram.
-- 07/29/00: Added default value to CASCADE_CHAIN and DEVICE_FAMILY
--           parameters of altqpram.
-- 07/28/00: INCLOCK_PERIOD defaulted to "10000". See SPR73860.
-- 07/26/00: Update altdpram to use "lpm_file" and "lpm_hint".
--           Latest altlvds_rx and altlvds_tx dfactor changes added.
-- 07/26/00: Updating altclklock for mercury support.
-- 07/24/00: Changing altlvds_rx and altlvds_tx warning msgs to errors.
-- 07/17/00: Changing altclklock *time_delay* parameter to string type to
--           allow for negative values.
-- 06/28/00: Fix altlvds_tx shift out on 3rd posedge.
-- 06/27/00: adding mercury support to altlvds_rx&tx.
-- 06/26/00: adding "operation_mode" param to altlvds_rx&tx.
-- 06/23/00: altclklock will come out of lock if inclock is unchanged.
-- 06/23/00: Added explicit type conversion for vector comparison.
-- 06/23/00: Adding intended_device_parameter to altclklock, altlvds_rx and
--           altlvds_tx. Minor changes to altclklock, altlvds_rx and
--           altlvds_tx to match Quartus sim behavior at startup.
-- 06/21/00: Changed all parameters of scfifo and dcfifo from type positive
--             to type natural.  scfifo and dcfifo are added back.
--           The scfifo implementation is a "placeholder" only.  The dcfifo
--             implementation is working except for some parameters
--             (almost_empty_value, overflow_checking, underflow_checking,
--             allow_rwcycle_when_full).
-- 06/15/00: Significant fixes to altclklock, altlvds_rx and altlvds_rx.
--           SPR 71839, 72411 and 72570. Corrected bit-ordering, deskew
--           usage, issues with "Duty Cycle violation" etc.
-- 06/13/00: SPR 72514
--           Fixed inclock edge detection problem, which caused output
--             clock to shift 180 degree for some designs.
-- 06/02/00: Added "jumbo" fix for altcam. SPR 70735, 71239, 71240, 71614
--           71617, 71940. Passes all regtest. NB: The state of the outputs
--           during a MULTIPLE mode write cycle is not modelled. (trying to
--           determine what correct behavior is; this behavior is of no 
--           importance to users.)
-- 05/22/00: Added 4 other modes of altqpram.
-- 05/11/00: Added implementation of altqpram.
--           Commented out scfifo and dcfifo.
-- 04/24/00: Added implementation of scfifo and dcfifo.  Currently they
--             require LPM library.
-- 04/20/00: Added implementation of altdpram.
----------------------------------------------------------------------------
-- Version 1.00 Date: 04/17/00
--
-- 04/17/00: SPR 70506
--           Corrected the match_mode parameter of altcam.
-- 04/17/00: SPR 70385
--           Fixed PLL output clocks to resume correctly after input clock
--             is disabled and then re-enabled.
-- 04/05/00: SPR 69196
--           Add a "placeholder" implementation for altdpram.
-- 03/08/00: SPR 69684
--           Fixed PLL duty cycle violation checking when clock does NOT
--             start at time=0.
--           SPR 69745, 69816
--           Fixed PLL output clock of 180 degree shifted problem.
--           SPR 68829
--           Added use_eab parameter to altcam
-- 11/15/99: Made altclklock locked output sensetive to clock variations.
-- 11/15/99: Fixed the lvds deskew according to the new spec.
-- 10/27/99: SPR 65010
--           Fixed lvds reciever and transmitter number of cycles to output.
--           Converted integer and positive types to natural to work with
--             Quartus Wizard. 
--           Changed altclklock outclock_phase_shift type from real to
--             natural which defaults to ps.
-- 10/22/99: SPR 65544
--           Corrected SINGLE mode operation when outputreg is clocked on
--             either "INCLOCK" or "OUTCLOCK".
-- 10/19/99: SPR 65264
--           Corrected CAM initialization from *.hex; i.e. fix to function
--             hex_to_stdlogicarray (s: string).
-- 10/05/99: Changed inclock_frequency to inclock_period for ALTCLKLOCK,
--             ALTLVDS_RX, and ALTLVDS_TX
-- 09/28/99: [lvds receiver]
--             Changed input_frequency to inclock_frequency 
--             Changed rx_clkin to rx_inclock
--             Changed rx_outclk to rx_outclock
--             Corrected the x7 loop index problem
--           [lvds transmitter]
--             Changed input_frequency to inclock_frequency 
--             Changed tx_clkin to tx_inclock
--             Changed tx_outclk to tx_outclock
--             Changed synch_clock_in  to sync_inclock
-- 09/20/99: Changed ALTPLL name to ALTCLKLOCK
--           Changed mult_factor_clock0 to clock0_boost
--           Changed mult_factor_clock1 to clock1_boost
--           Changed divide_factor_clock0 to clock0_divide
--           Changed divide_factor_clock1 to clock1_divide
--           Added lvds_rx and lvds_tx megafuntions
----------------------------------------------------------------------------


----------------------------------------------------------------------------
-- altcam 
----------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use std.textio.all;


entity altcam is
    generic
      ( width:            natural := 1;
        widthad:          natural := 1;
        numwords:         natural := 1;
        lpm_file:         string := "UNUSED";
        lpm_filex:        string := "UNUSED";
        match_mode:       string := "MULTIPLE";
        output_reg:       string := "UNREGISTERED";
        output_aclr:      string := "OFF";
        pattern_reg:      string := "INCLOCK";
        pattern_aclr:     string := "ON";
        wraddress_aclr:   string := "ON";
        wrx_reg:          string := "UNUSED";
        wrx_aclr:         string := "UNUSED";
        wrcontrol_aclr:   string := "OFF";
        use_eab:          string := "ON" );
    port
      ( pattern:    in std_logic_vector(width-1 downto 0);
        wrx:        in std_logic_vector(width-1 downto 0) := (others => '0');
        wrxused:    in std_logic := '1';
        wrdelete:   in std_logic := '0';
        wraddress:  in std_logic_vector(widthad-1 downto 0);
        wren:       in std_logic;
        inclock:    in std_logic;
        inclocken:  in std_logic := '1';
        inaclr:     in std_logic := '0';
        mstart:     in std_logic := 'X';
        mnext:      in std_logic := '0';
        outclock:   in std_logic := '0';
        outclocken: in std_logic := '1';
        outaclr:    in std_logic := '0';
        maddress:   out std_logic_vector(widthad-1 downto 0);
        mbits:      out std_logic_vector(numwords-1 downto 0);
        mfound:     out std_logic;
        mcount:     out std_logic_vector(widthad-1 downto 0);
        rdbusy:     out std_logic;
        wrbusy:     out std_logic );

    type lpm_memory is array
        (numwords-1 downto 0) of std_logic_vector(width-1 downto 0);

    function int_to_str( value : integer ) return string is
    variable ivalue,index : integer;
    variable digit : integer;
    variable line_no: string(8 downto 1) := "        ";  
    begin
        ivalue := value;
        index := 1;   
        while (ivalue > 0) loop
            digit := ivalue MOD 10;
            ivalue := ivalue/10;
            case digit is
                when 0 =>
                    line_no(index) := '0';
                when 1 =>
                    line_no(index) := '1';
                when 2 =>
                    line_no(index) := '2';
                when 3 =>
                    line_no(index) := '3';
                when 4 =>
                    line_no(index) := '4';
                when 5 =>
                    line_no(index) := '5';
                when 6 =>
                    line_no(index) := '6';
                when 7 =>
                    line_no(index) := '7';
                when 8 =>
                    line_no(index) := '8';
                when 9 =>
                    line_no(index) := '9';
                when others =>
                    ASSERT FALSE
                    REPORT "Illegal number!"
                    SEVERITY ERROR;
            end case;
            index := index + 1;
        end loop;
        return line_no;
    end;

    function hex_str_to_int( str : string ) return integer is
    variable len : integer := str'length;
    variable ivalue : integer := 0;
    variable digit : integer;
    begin
        for i in len downto 1 loop
            case str(i) is
                when '0' =>
                    digit := 0;
                when '1' =>
                    digit := 1;
                when '2' =>
                    digit := 2;
                when '3' =>
                    digit := 3;
                when '4' =>
                    digit := 4;
                when '5' =>
                    digit := 5;
                when '6' =>
                    digit := 6;
                when '7' =>
                    digit := 7;
                when '8' =>
                    digit := 8;
                when '9' =>
                    digit := 9;
                when 'A' =>
                    digit := 10;
                when 'a' =>
                    digit := 10;
                when 'B' =>
                    digit := 11;
                when 'b' =>
                    digit := 11;
                when 'C' =>
                    digit := 12;
                when 'c' =>
                    digit := 12;
                when 'D' =>
                    digit := 13;
                when 'd' =>
                    digit := 13;
                when 'E' =>
                    digit := 14;
                when 'e' =>
                    digit := 14;
                when 'F' =>
                    digit := 15;
                when 'f' =>
                    digit := 15;
                when others =>
                    ASSERT FALSE
                    REPORT "Illegal character "&  str(i) & "in Intel Hex File! "
                    SEVERITY ERROR;
            end case;
            ivalue := ivalue * 16 + digit;
        end loop;
        return ivalue;
    end;

    procedure Shrink_line(L : inout LINE; pos : in integer) is
    subtype nstring is string(1 to pos);
    variable stmp : nstring;
    begin
        if pos >= 1 then
            read(l,stmp);
        end if;
    end;

    function hex_to_stdlogicarray (s: string) return lpm_memory is
    variable mem_data : lpm_memory;
    variable mem_data_word : std_logic_vector(width-1 downto 0);
    variable i,j,k,n,m,lineno: integer := 0;
    variable buf: line;
    variable booval: boolean;
    --FILE mem_data_file: TEXT IS IN s;
    FILE mem_data_file: TEXT OPEN READ_MODE IS s;
    variable base, byte, rec_type, datain, checksum: string(2 downto 1);
    variable startadd: string(4 downto 1);
    variable ibase: integer := 0;
    variable ibyte: integer := 0;
    variable istartadd: integer := 0;
    variable check_sum_vec, check_sum_vec_tmp: std_logic_vector(7 downto 0);
    begin
        WHILE NOT ENDFILE(mem_data_file) loop
            booval := true;
            READLINE(mem_data_file, buf);
            lineno := lineno + 1;
            check_sum_vec := (OTHERS => '0');
            if (buf(buf'LOW) = ':') then  -- check for start of record char ':'
                i := 1;
                shrink_line(buf, i);
                READ(L=>buf, VALUE=>byte, good=>booval); -- read length of record (2 hex chars)
                if not (booval) then
                    ASSERT FALSE
                    REPORT "[Line "& int_to_str(lineno) & "]:Illegal Intel Hex Format!"
                    SEVERITY ERROR;
                end if;
                ibyte := hex_str_to_int(byte);
                check_sum_vec := unsigned(check_sum_vec) + unsigned(CONV_STD_LOGIC_VECTOR(ibyte, 8));
                READ(L=>buf, VALUE=>startadd, good=>booval); -- read start/load address (4 hex chars)
                if not (booval) then 
                    ASSERT FALSE
                    REPORT "[Line "& int_to_str(lineno) & "]:Illegal Intel Hex Format! "
                    SEVERITY ERROR;
                end if;
                istartadd := hex_str_to_int(startadd);
                byte(2) := startadd(4);
                byte(1) := startadd(3);
                check_sum_vec := unsigned(check_sum_vec) + unsigned(CONV_STD_LOGIC_VECTOR(hex_str_to_int(byte), 8));
                byte(2) := startadd(2);
                byte(1) := startadd(1);
                check_sum_vec := unsigned(check_sum_vec) + unsigned(CONV_STD_LOGIC_VECTOR(hex_str_to_int(byte), 8));
                READ(L=>buf, VALUE=>rec_type, good=>booval);  -- read record type (2 hex chars)
                if not (booval) then
                    ASSERT FALSE
                    REPORT "[Line "& int_to_str(lineno) & "]:Illegal Intel Hex Format! "
                    SEVERITY ERROR;
                end if;
                check_sum_vec := unsigned(check_sum_vec) + unsigned(CONV_STD_LOGIC_VECTOR(hex_str_to_int(rec_type), 8));
            else
                ASSERT FALSE
                REPORT "[Line "& int_to_str(lineno) & "]:Illegal Intel Hex Format! "
                SEVERITY ERROR;
            end if;

            case rec_type is
                when "00"=>  -- Data record
                    i := 0;
                    k := width/8;
                    if ( (width MOD 8) /= 0 ) then
                        k := k + 1; 
                    end if;
                    -- k = no. of bytes per CAM entry.
                    while( i < ibyte ) loop
                       mem_data_word := (others => '0');
                       n := (k - 1)*8;
                       m := width - 1;
                       for j in 1 to k loop
                            READ(L=>buf, VALUE=>datain,good=>booval); -- read in data a byte (2 hex chars) at a time.
                            if not (booval) then
                                ASSERT FALSE
                                REPORT "[Line "& int_to_str(lineno) & "]:Illegal Intel Hex Format! "
                                SEVERITY ERROR;
                            end if;
                            check_sum_vec := unsigned(check_sum_vec) + unsigned(CONV_STD_LOGIC_VECTOR(hex_str_to_int(datain), 8));
                            mem_data_word(m downto n) := CONV_STD_LOGIC_VECTOR(hex_str_to_int(datain), m-n+1);
                            m := n - 1;
                            n := n - 8;
                        end loop;
                        i := i + k;
                        mem_data(ibase + istartadd) := mem_data_word;
                        istartadd := istartadd + 1;
                    end loop;
                when "01"=>
                    exit;
                when "02"=>
                    ibase := 0;
                    if (ibyte /= 2) then
                        ASSERT FALSE
                        REPORT "[Line "& int_to_str(lineno) & "]:Illegal Intel Hex Format for record type 02! "
                        SEVERITY ERROR;
                    end if;
                    for i in 0 to (ibyte-1) loop
                        READ(L=>buf, VALUE=>base,good=>booval);
                        if not (booval) then
                            ASSERT FALSE
                            REPORT "[Line "& int_to_str(lineno) & "]:Illegal Intel Hex Format! "
                            SEVERITY ERROR;
                        end if;
                        ibase := ibase * 256 + hex_str_to_int(base);
                        check_sum_vec := unsigned(check_sum_vec) + unsigned(CONV_STD_LOGIC_VECTOR(hex_str_to_int(base), 8));
                    end loop;
                    ibase := ibase * 16;  -- because std load addr is 2 bytes.
                when OTHERS =>
                    ASSERT FALSE
                    REPORT "[Line "& int_to_str(lineno) & "]:Illegal record type in Intel Hex File! "
                    SEVERITY ERROR;
            end case;
            READ(L=>buf, VALUE=>checksum,good=>booval);
            if not (booval) then
                ASSERT FALSE
                REPORT "[Line "& int_to_str(lineno) & "]:Checksum is missing! "
                SEVERITY ERROR;
            end if;

            check_sum_vec := unsigned(not (check_sum_vec)) + 1;
            check_sum_vec_tmp := CONV_STD_LOGIC_VECTOR(hex_str_to_int(checksum),8);

            if (check_sum_vec /= check_sum_vec_tmp) then
                ASSERT FALSE
                REPORT "[Line "& int_to_str(lineno) & "]:Incorrect checksum!"
                SEVERITY ERROR;
            end if;
         end loop;
         return mem_data;
    end;

end altcam;    

architecture behave of altcam is

signal pattern_rgd: std_logic_vector(width-1 downto 0) := (others => '0'); -- registered input
signal pattern_int: std_logic_vector(width-1 downto 0) := (others => '0'); -- internal input
signal wrx_rgd: std_logic_vector(width-1 downto 0) := (others => '0'); -- registered input
signal wrx_int: std_logic_vector(width-1 downto 0) := (others => '0'); -- internal input
signal wrxused_rgd : std_logic := '0'; -- registered input
signal wrxused_int : std_logic := '0'; -- internal input
signal wraddress_rgd: std_logic_vector(widthad-1 downto 0) := (others => '0'); -- registered input
signal wren_rgd: std_logic := '0'; -- registered input
signal wrdelete_rgd: std_logic := '0'; -- internally registered input
signal maddress_rgd: std_logic_vector(widthad-1 downto 0) := (others => '0'); -- registered output
signal maddress_int: std_logic_vector(widthad-1 downto 0) := (others => '0'); -- internal output
signal mbits_rgd: std_logic_vector(numwords-1 downto 0) := (others => '0'); -- registered output
signal mbits_int: std_logic_vector(numwords-1 downto 0) := (others => '0'); -- internal output
signal mfound_rgd: std_logic := '0'; -- registered output
signal mfound_int: std_logic := '0'; -- internal output
signal mcount_rgd: std_logic_vector(widthad-1 downto 0) := (others => '0'); -- registered output
signal mcount_int: std_logic_vector(widthad-1 downto 0) := (others => '0'); -- internal output
signal wrbusy_int : std_logic := '0'; -- internal output
signal rdbusy_int : std_logic := '0'; -- internal output
signal outclock_int: std_logic;  -- internal outclock
signal outaclr_int: std_logic;   -- internal outaclr

-- CAM signals
signal cam_init: integer := 1;
signal cam_array: lpm_memory;
signal x_array: lpm_memory;

-- Read control signals
signal get_first_match : boolean := false;
signal get_next_match : boolean := true;
signal first_read_clock : boolean := false;
signal mstart_rgd1: std_logic := '0';
signal mstart_rgd2: std_logic := '0';
signal rdbusy_delayed : std_logic;
signal first_read_in_write: boolean := false;
signal mstart_used: boolean := false;

-- Write control signals
signal write_start : std_logic;
signal write_start_rgd: std_logic := '0';
signal write_start_1: std_logic := '0';
signal write0: boolean := true;
signal write1: boolean := false;
signal writex: boolean := false;
signal write0_done: boolean := false;
signal write1_done: boolean := false;
signal writex_done: boolean := false;
signal write_incomplete: std_logic := '0';

begin

    msg: process (wraddress_rgd, wrdelete_rgd, wren_rgd, 
                  wrxused_int, pattern_int, mstart_rgd1)
    begin
        if wren_rgd'event and wren_rgd = '0' and write_incomplete = '1' then
            ASSERT FALSE
            REPORT "Insufficient write cycle time, write maybe invalid! "
            SEVERITY WARNING;
        elsif pattern_int'event and write_incomplete = '1' then
            ASSERT FALSE
            REPORT "Insufficient pattern hold time, write maybe invalid! "
            SEVERITY WARNING;
        elsif wraddress_rgd'event and write_incomplete = '1' then
            ASSERT FALSE
            REPORT "Insufficient address hold time, write maybe invalid! "
            SEVERITY WARNING;
        elsif wrdelete_rgd'event and wrdelete_rgd = '0' and 
            write_incomplete = '1' then
            ASSERT FALSE
            REPORT "Insufficient delete cycle time, delete failed! "
            SEVERITY WARNING;
        elsif wrdelete_rgd'event and wrdelete_rgd = '1' and 
            write_incomplete = '1' then
            ASSERT FALSE
            REPORT "Insufficient write cycle time, write maybe invalid! "
            SEVERITY WARNING;
        elsif wrxused_int'event and write_incomplete = '1' and 
            wrdelete_rgd = '0' then
            ASSERT FALSE
            REPORT "wrxused signal changed during write! "
            SEVERITY WARNING;
        elsif mstart_rgd1'event and write_incomplete = '1' and 
            mstart_rgd1 = '1' then
            ASSERT FALSE
            REPORT "Incorrect read attempt during write! "
            SEVERITY WARNING;
        end if;
        if pattern_int'event then
            if rdbusy_delayed = '1' then
                ASSERT FALSE
                REPORT "Insufficient read time, read failed! "
                SEVERITY WARNING;
            elsif rdbusy_delayed = '0' and mnext = '1' then
                ASSERT FALSE
                REPORT "Illegal pattern change during read, read failed! "
                SEVERITY WARNING;
            end if;
        end if;      
    end process msg;

    -- Evaluate parameters
    pattern_int <= pattern when pattern_reg = "UNREGISTERED" else pattern_rgd;
    wrx_int     <= wrx when wrx_reg = "UNREGISTERED" else wrx_rgd;
    wrxused_int <= wrxused when wrx_reg = "UNREGISTERED" else wrxused_rgd;
    maddress    <= maddress_int when output_reg = "UNREGISTERED" else maddress_rgd;
    mbits       <= mbits_int when output_reg = "UNREGISTERED" else mbits_rgd;
    mcount      <= mcount_int when output_reg = "UNREGISTERED" else mcount_rgd;
    mfound      <= mfound_int when output_reg = "UNREGISTERED" else mfound_rgd;
    wrbusy      <= wrbusy_int;
    rdbusy      <= rdbusy_int;
    outclock_int <= outclock when output_reg = "OUTCLOCK" else inclock;
    outaclr_int  <= outaclr when output_aclr = "ON" else inaclr;

    rdbusy_delayed <= rdbusy_int after 2 ns;

    mstart_update: process (mstart)
    begin
        mstart_used <= true;
    end process mstart_update;


    -----------------------------------------
    -- Evaluate ALTCAM reading and writing --
    -----------------------------------------
    read_write: process (inaclr, inclock, pattern)
    variable count : natural := 0;
    variable index : natural :=0;
    variable addr : natural :=0;
    variable next_search : natural := 0;
    variable restart_read : boolean := false;
    variable reset_read : boolean := true;
    variable ipattern: std_logic_vector(width-1 downto 0); 
    variable iwraddress: std_logic_vector(widthad-1 downto 0);
    variable iwrx: std_logic_vector(width-1 downto 0); 
    variable iwren: std_logic;
    variable iwrxused : std_logic;
    variable mbits_tmp: std_logic_vector(numwords-1 downto 0);
    variable cam_array_tmp: lpm_memory;
    variable x_array_tmp: lpm_memory;
    begin
        -- Initialise the CAM at startup.
        if cam_init = 1  then 
            if lpm_file = "UNUSED" then
                for i in 0 to numwords-1 loop
                    cam_array(i) <= (others => '1');
                    x_array(i) <= (others => '1');
                end loop;
            elsif lpm_filex = "UNUSED" then
                cam_array <= hex_to_stdlogicarray(lpm_file);
                for i in 0 to numwords-1 loop
                    x_array(i) <= (others => '0');
                end loop;
            else
                cam_array <= hex_to_stdlogicarray(lpm_file);
                x_array <= hex_to_stdlogicarray(lpm_filex);
            end if;
            if match_mode /= "SINGLE" then
                maddress_int <= (others => '1');
            end if;
        end if;
        cam_init <= 0;

        ipattern := pattern;
        iwrx := wrx;
        iwraddress := wraddress;
        iwren := wren;
        if (wrx_reg = "UNUSED") or (wrx_aclr = "UNUSED") then
            iwrxused := '0';  -- must be unconnected
        else
            iwrxused := wrxused;
        end if;
        
        if inaclr = '1' then
            if (mstart_used = true) then
                reset_read := true;
            end if;
            first_read_clock <= false;
            get_first_match <= false;
            if pattern_aclr = "ON" then 
                pattern_rgd <= (others => '0'); 
                ipattern := (others => '0'); 
            end if;
            if wrx_aclr = "ON" then 
                wrx_rgd <= (others => '0'); 
                iwrx := (others => '0'); 
                wrxused_rgd <= '0';
                iwrxused := '0';
            end if;
            if wraddress_aclr = "ON" then 
                wraddress_rgd <= (others => '0'); 
                iwraddress := (others => '0'); 
            end if;
            if wrcontrol_aclr = "ON" then 
                wren_rgd <= '0'; 
                iwren := '0'; 
                write0_done <= false;
                write1_done <= false;
                writex_done <= false;
            end if;
            if (pattern_aclr = "ON") then 
                mbits_int <= (others => '0');
                mcount_int <= (others => '0');
                mfound_int <= '0';
                if (match_mode = "SINGLE") then
                    maddress_int <= (others => '0');
                else 
                    maddress_int <= (others => '1');
                end if;
            end if;
        end if;

        if inclock'event and (inclocken = '1') then
          if inclock = '1' then
            pattern_rgd <= ipattern;
            wrx_rgd <= iwrx;
            wrxused_rgd <= iwrxused;
            wraddress_rgd <= iwraddress;
            wren_rgd <= iwren;

            write_start_rgd <= write_start;
            write_incomplete <= wrbusy_int;
            mstart_rgd1 <= mstart;
            mstart_rgd2 <= mstart_rgd1;
            wrdelete_rgd <= wrdelete;

            if iwren = '0' then
                write0_done <= false;
                write1_done <= false;
                writex_done <= false;
                --------------------
                -- CAM READ MODES --
                --------------------
                -- If pattern changed or mstart asserted again then restart
                -- read cycle.
                if (match_mode = "FAST_MULTIPLE" and mstart_used = false and reset_read = true) 
                    or (mstart = '1' and mstart_rgd1 = '0')
                    or ((ipattern /= pattern_rgd) and (reset_read=false)) then
                    restart_read := true;
                    reset_read := false;
                    get_first_match <= false;
                else
                    restart_read := false;
                end if;
                -------------------------
                -- FAST MULTIPLE: READ --
                -------------------------
                if match_mode = "FAST_MULTIPLE" then
                    if get_first_match = true and restart_read = false then
                        if get_next_match = true then -- start of next read cycle
                            index := next_search; 
                            maddr_fm0: for i in index to numwords-1 loop
                                mword_fm0: for j in 0 to width-1 loop
                                    if (((x_array(i)(j) = '0') and (cam_array(i)(j) = ipattern(j))) or 
                                        ((x_array(i)(j) = '1') and (cam_array(i)(j) = '0'))) then
                                        if j = width-1 then 
                                            next_search := i+1;
                                            exit maddr_fm0;
                                        end if;
                                    else 
                                        exit mword_fm0;
                                    end if;
                                end loop mword_fm0;
                            end loop maddr_fm0;
                            if index = next_search then -- no more matches
                                mfound_int <= '0';
                                maddress_int <= (others => '1');
                            else
                                maddress_int <= ieee.std_logic_arith.conv_std_logic_vector(next_search-1, widthad);
                            end if;
                        end if;
                    else -- start of new read cycle
                        count := 0;
                        mbits_tmp := (others => '0');
                        maddr_fm1: for i in 0 to numwords -1 loop
                            mword_fm1: for j in 0 to width-1 loop
                                if (((x_array(i)(j) = '0') and (cam_array(i)(j) = ipattern(j))) or 
                                    ((x_array(i)(j) = '1') and (cam_array(i)(j) = '0'))) then
                                    if j = width-1 then 
                                        if (count = 0) and (reset_read = false) and (restart_read = true) then
                                            mfound_int <= '1';
                                            maddress_int <= ieee.std_logic_arith.conv_std_logic_vector(i, widthad);
                                            get_first_match <= true;
                                            next_search := i+1;
                                        end if;
                                        mbits_tmp(i) := '1';
                                        count := count + 1;
                                    end if;
                                else 
                                    exit mword_fm1;
                                end if;
                            end loop mword_fm1;
                        end loop maddr_fm1;
                        mcount_int <= ieee.std_logic_arith.conv_std_logic_vector(count, widthad);
                        mbits_int <= mbits_tmp;
                        if (count = 0) or (reset_read = true) then -- no matches
                            mfound_int <= '0';
                            maddress_int <= (others => '1');
                        end if;
                    end if; -- end of initial read cycle
                end if; -- end of FAST MULTIPLE

                --------------------
                -- MULTIPLE: READ --
                --------------------
                if match_mode = "MULTIPLE" then
                    if get_first_match = true and restart_read = false then
                        if get_next_match = true then -- start of next read cycle
                            index := next_search; 
                            maddr_mm0: for i in index to numwords-1 loop
                                mword_mm0: for j in 0 to width-1 loop
                                    if (((x_array(i)(j) = '0') and (cam_array(i)(j) = ipattern(j))) or 
                                        ((x_array(i)(j) = '1') and (cam_array(i)(j) = '0'))) then
                                        if j = width-1 then 
                                            next_search := i+1;
                                            exit maddr_mm0;
                                        end if;
                                    else 
                                        exit mword_mm0;
                                    end if;
                                end loop mword_mm0;
                            end loop maddr_mm0;
                            if index = next_search then
                                mfound_int <= '0';
                                maddress_int <= (others => '1');
                            else
                                maddress_int <= ieee.std_logic_arith.conv_std_logic_vector(next_search-1, widthad);
                            end if;
                        end if;
                    else -- start of 1st match 
                      count := 0;
                      if reset_read=false then -- Not in reset state
                        if first_read_clock = false then 
                            -- 1st cycle: match with even and write to even
                            first_read_clock <= true;
                            maddress_int <= (others => '1');
                            mfound_int <= '0';
                            mbits_tmp := mbits_int;
                            maddr_mm1: for i in 0 to numwords-1 loop
                                if (i mod 2)=0 then
                                    if mbits_int(i) = '1' then
                                        count := count + 1;
                                    end if;
                                    mword_mm1: for j in 0 to width-1 loop
                                        if (((x_array(i)(j) = '0') and (cam_array(i)(j) = ipattern(j))) or 
                                            ((x_array(i)(j) = '1') and (cam_array(i)(j) = '0'))) then
                                            if j = width-1 then 
                                                mbits_tmp(i+1) := '1';
                                                count := count + 1;
                                            end if;
                                        else 
                                            mbits_tmp(i+1) := '0';
                                            exit mword_mm1;
                                        end if;
                                    end loop mword_mm1;
                                end if;
                            end loop maddr_mm1;
                        else -- 2nd read cycle
                            -- 2nd cycle: do match 
                            first_read_clock <= false;
                            mbits_tmp := (others => '0');
                            maddr_mm2: for i in 0 to numwords-1 loop
                                mword_mm2: for j in 0 to width-1 loop
                                    if (((x_array(i)(j) = '0') and (cam_array(i)(j) = ipattern(j))) or 
                                        ((x_array(i)(j) = '1') and (cam_array(i)(j) = '0'))) then
                                        if j = width-1 then 
                                            if count = 0 then
                                                next_search := i+1;
                                            end if;
                                            mbits_tmp(i) := '1';
                                            count := count + 1;
                                        end if;
                                    else 
                                        exit mword_mm2;
                                    end if;
                                end loop mword_mm2;
                            end loop maddr_mm2;
                            if count = 0 then -- no matches
                                maddress_int <= (others => '1');
                                mfound_int <= '0';
                            else
                                get_first_match <= true;
                                mfound_int <= '1';
                                maddress_int <= ieee.std_logic_arith.conv_std_logic_vector(next_search-1, widthad);
                            end if;
                        end if;
                      else -- In reset state
                           -- Match with even but write to odd
                        maddress_int <= (others => '1');
                        mfound_int <= '0';
                        mbits_tmp := (others => '0');
                        maddr_mm3: for i in 0 to numwords-1 loop
                            if (i mod 2)=0 then
                                mword_mm3: for j in 0 to width-1 loop
                                    if (((x_array(i)(j) = '0') and (cam_array(i)(j) = ipattern(j))) or 
                                        ((x_array(i)(j) = '1') and (cam_array(i)(j) = '0'))) then
                                        if j = width-1 then 
                                            mbits_tmp(i+1) := '1';
                                            count := count + 1;
                                        end if;
                                    else 
                                        exit mword_mm3;
                                    end if;
                                end loop mword_mm3;
                            end if;
                        end loop maddr_mm3;
                      end if; -- end of 1st match
                      mcount_int <= ieee.std_logic_arith.conv_std_logic_vector(count, widthad);
                      mbits_int <= mbits_tmp;
                    end if; -- end of initial read cycle
                end if; -- end of MULTIPLE

                ------------------
                -- SINGLE: READ --
                ------------------
                if match_mode = "SINGLE" then
                    mbits_tmp := (others => '0');
                    index := 0;
                    count := 0;
                    maddr_sm0: for i in 0 to numwords-1 loop
                        mword_sm0: for j in 0 to width-1 loop
                               if (((x_array(i)(j) = '0') and (cam_array(i)(j) = ipattern(j))) or 
                                   ((x_array(i)(j) = '1') and (cam_array(i)(j) = '0'))) then
                                   if j = width-1 then 
                                       mbits_tmp(i) := '1';
                                       index := i;
                                       count := 1;
                                       exit maddr_sm0;
                                    end if;
                                else
                                    exit mword_sm0;
                                end if;
                        end loop mword_sm0;
                    end loop maddr_sm0;
                    mcount_int <= ieee.std_logic_arith.conv_std_logic_vector(count, widthad);
                    mbits_int <= mbits_tmp;
                    if count = 0 then
                        maddress_int <= (others => '0');
                        mfound_int <= '0';
                    else
                        mfound_int <= '1';
                        maddress_int <= ieee.std_logic_arith.conv_std_logic_vector(index, widthad);
                    end if;
                end if; -- end of SINGLE
            else -- if wren = '1' 
                ----------------------
                -- READ AFTER WRITE --
                ----------------------
                -- Writing to CAM so reset read cycle.
                get_first_match <= false;
                first_read_clock <= false;
                restart_read := false;
                if (mstart_used = true) then
                    reset_read := true;
                end if;
                -------------------------------------
                -- FAST MULTIPLE: READ AFTER WRITE --
                -------------------------------------
                if match_mode = "FAST_MULTIPLE" then
                    mfound_int <= '0';
                    maddress_int <= (others => '1');
                    count := 0;
                    mbits_tmp := (others => '0');
                    if (writex = true) and (iwrxused = '1') then
                        waddr_fm0: for i in 0 to numwords-1 loop
                            wword_fm0: for j in 0 to width-1 loop
                                if (((x_array(i)(j) = '0') and (cam_array(i)(j) = (ipattern(j) xor iwrx(j)))) or 
                                    ((x_array(i)(j) = '1') and (cam_array(i)(j) = '0'))) then
                                    if j = width-1 then 
                                        mbits_tmp(i) := '1';
                                        count := count + 1;
                                    end if;
                                else 
                                    exit wword_fm0;
                                end if;
                            end loop wword_fm0;
                        end loop waddr_fm0;
                    else
                        waddr_fm1: for i in 0 to numwords-1 loop
                            wword_fm1: for j in 0 to width-1 loop
                                if (((x_array(i)(j) = '0') and (cam_array(i)(j) = ipattern(j))) or 
                                    ((x_array(i)(j) = '1') and (cam_array(i)(j) = '0'))) then
                                    if j = width-1 then 
                                        mbits_tmp(i) := '1';
                                        count := count + 1;
                                    end if;
                                else 
                                    exit wword_fm1;
                                end if;
                            end loop wword_fm1;
                        end loop waddr_fm1;
                    end if;
                    mcount_int <= ieee.std_logic_arith.conv_std_logic_vector(count, widthad);
                    mbits_int <= mbits_tmp;
                end if; -- end of FAST MULTIPLE

                --------------------------------
                -- MULTIPLE: READ AFTER WRITE --
                --------------------------------
                -- THIS IMPLEMENTATION IS INACCURATE
                if match_mode = "MULTIPLE" then
                    mfound_int <= '0';
                    maddress_int <= (others => '1');
                    mbits_tmp := (others => '0');
                    if (writex = true) and (iwrxused = '1') then
                        mcount_int <= (others => '0');
                    else
                        if first_read_in_write = false then
                        -- Read even addresses but they appear on the odd locations
                        -- of mbits.
                        count := 0;
                        waddr_mm0: for i in 0 to numwords-1 loop
                            if ( (i mod 2) = 0 ) then
                                if (mbits_int(i) = '1') then -- counting previous even address matches
                                    count := count + 1;
                                end if;
                            wword_mm0: for j in 0 to width-1 loop
                                if (((x_array(i)(j) = '0') and (cam_array(i)(j) = ipattern(j))) or 
                                    ((x_array(i)(j) = '1') and (cam_array(i)(j) = '0'))) then
                                    if j = width-1 then 
                                        mbits_tmp(i+1) := '1';
                                        count := count + 1;
                                    end if;
                                else 
                                    exit wword_mm0;
                                end if;
                            end loop wword_mm0;
                            end if;
                        end loop waddr_mm0;
                        else
                        --  Read odd addresses. 
                        count := 0;
                        waddr_mm1: for i in numwords-1 downto 0 loop
                                if (i mod 2) = 1 then
                                mbits_tmp(i-1) := mbits_tmp(i);
                            else
                            wword_mm1: for j in 0 to width-1 loop
                                if (((x_array(i)(j) = '0') and (cam_array(i)(j) = ipattern(j))) or 
                                    ((x_array(i)(j) = '1') and (cam_array(i)(j) = '0'))) then
                                    if j = width-1 then 
                                        mbits_tmp(i) := '1';
                                        count := count + 1;
                                    end if;
                                else 
                                    exit wword_mm1;
                                end if;
                            end loop wword_mm1;
                            end if;
                        end loop waddr_mm1;
                        end if;
                        mcount_int <= ieee.std_logic_arith.conv_std_logic_vector(count, widthad);
                        mbits_int <= mbits_tmp;
                    end if;
                end if; -- end of MULTIPLE

                ------------------------------
                -- SINGLE: READ AFTER WRITE --
                ------------------------------
                if match_mode = "SINGLE" then
                    mbits_tmp := (others => '0');
                    index := 0;
                    count := 0;
                    if (writex = true) and (iwrxused = '1') then
                        waddr_sm0: for i in 0 to numwords-1 loop
                            wword_sm0: for j in 0 to width-1 loop
                                if (((x_array(i)(j) = '0') and (cam_array(i)(j) = (ipattern(j) xor iwrx(j)))) or 
                                    ((x_array(i)(j) = '1') and (cam_array(i)(j) = '0'))) then
                                    if j = width-1 then 
                                        mbits_tmp(i) := '1';
                                        index := i;
                                        count := 1;
                                        exit waddr_sm0;
                                    end if;
                                else 
                                    exit wword_sm0;
                                end if;
                            end loop wword_sm0;
                        end loop waddr_sm0;
                    else
                        waddr_sm1: for i in 0 to numwords-1 loop
                            wword_sm1: for j in 0 to width-1 loop
                                if (((x_array(i)(j) = '0') and (cam_array(i)(j) = ipattern(j))) or 
                                    ((x_array(i)(j) = '1') and (cam_array(i)(j) = '0'))) then
                                    if j = width-1 then 
                                        mbits_tmp(i) := '1';
                                        index := i;
                                        count := 1;
                                        exit waddr_sm1;
                                    end if;
                                else 
                                    exit wword_sm1;
                                end if;
                            end loop wword_sm1;
                        end loop waddr_sm1;
                    end if;
                    mcount_int <= ieee.std_logic_arith.conv_std_logic_vector(count, widthad);
                    mbits_int <= mbits_tmp;
                    if count = 0 then
                        mfound_int <= '0';
                        maddress_int <= (others => '0');
                    else
                        mfound_int <= '1';
                        maddress_int <= ieee.std_logic_arith.conv_std_logic_vector(index, widthad);
                    end if;
                end if; -- end of SINGLE
            end if; 
          else -- if inclock negedge
            -- We write to the CAM on the low cycle of inclock 
            -- when wren_rgd='1'.
            if (wren_rgd='1') and (inclock='0') then
                if pattern_reg = "UNREGISTERED" then
                    ipattern := pattern;
                else
                    ipattern := pattern_rgd;
                end if;
                addr := ieee.std_logic_unsigned.conv_integer(wraddress_rgd);
                cam_array_tmp := cam_array;
                x_array_tmp := x_array;
                ---------------------
                -- CAM WRITE MODES --
                ---------------------
                if wrdelete_rgd = '0' then
                    if (wrxused_int = '1') and (wrx_reg /= "UNUSED") and (wrx_aclr /= "UNUSED") then
                        ------------------- 
                        -- 3 CYCLE WRITE -- 
                        ------------------- 
                        ----------------- 
                        -- WRITE_ZEROS -- 
                        ----------------- 
                        if write0 = true then
                        for i in 0 to width-1 loop
                            if ipattern(i) = '0' then
                                -- "0" => "0"
                                if (cam_array(addr)(i)='0' and x_array(addr)(i)='0') then -- "0"
                                    cam_array_tmp(addr)(i) := '0';
                                    x_array_tmp(addr)(i) := '0';
                                -- "1" => "X"
                                elsif (cam_array(addr)(i)='1' and x_array(addr)(i)='0') then -- "1"
                                    cam_array_tmp(addr)(i) := '0';
                                    x_array_tmp(addr)(i) := '1';
                                -- "X" => "X"
                                elsif (cam_array(addr)(i)='0' and x_array(addr)(i)='1') then -- "X"
                                    cam_array_tmp(addr)(i) := '0';
                                    x_array_tmp(addr)(i) := '1';
                                -- "U" => "0"
                                elsif (cam_array(addr)(i)='1' and x_array(addr)(i)='1') then -- "U"
                                    cam_array_tmp(addr)(i) := '0';
                                    x_array_tmp(addr)(i) := '0';
                                end if;
                            elsif ipattern(i) = '1' then
                                -- "0" => "X"
                                if (cam_array(addr)(i)='0' and x_array(addr)(i)='0') then -- "0"
                                    cam_array_tmp(addr)(i) := '0';
                                    x_array_tmp(addr)(i) := '1';
                                -- "1" => "1"
                                elsif (cam_array(addr)(i)='1' and x_array(addr)(i)='0') then -- "1"
                                    cam_array_tmp(addr)(i) := '1';
                                    x_array_tmp(addr)(i) := '0';
                                -- "X" => "X"
                                elsif (cam_array(addr)(i)='0' and x_array(addr)(i)='1') then -- "X"
                                    cam_array_tmp(addr)(i) := '0';
                                    x_array_tmp(addr)(i) := '1';
                                -- "U" => "1"
                                elsif (cam_array(addr)(i)='1' and x_array(addr)(i)='1') then -- "U"
                                    cam_array_tmp(addr)(i) := '1';
                                    x_array_tmp(addr)(i) := '0';
                                end if;
                            end if;
                        end loop;
                        write0_done <= true;
                        write1_done <= false;
                        writex_done <= false;
                        end if;
                        ----------------
                        -- WRITE_ONES --
                        ----------------
                        if write1 = true then
                        for i in 0 to width-1 loop
                            if ipattern(i) = '0' then
                                -- "0" => "0"
                                if (cam_array(addr)(i)='0' and x_array(addr)(i)='0') then -- "0"
                                    cam_array_tmp(addr)(i) := '0';
                                    x_array_tmp(addr)(i) := '0';
                                -- "1" => "U"
                                elsif (cam_array(addr)(i)='1' and x_array(addr)(i)='0') then -- "1"
                                    cam_array_tmp(addr)(i) := '1';
                                    x_array_tmp(addr)(i) := '1';
                                -- "X" => "0"
                                elsif (cam_array(addr)(i)='0' and x_array(addr)(i)='1') then -- "X"
                                    cam_array_tmp(addr)(i) := '0';
                                    x_array_tmp(addr)(i) := '0';
                                -- "U" => "U"
                                elsif (cam_array(addr)(i)='1' and x_array(addr)(i)='1') then -- "U"
                                    cam_array_tmp(addr)(i) := '1';
                                    x_array_tmp(addr)(i) := '1';
                                end if;
                            elsif ipattern(i) = '1' then
                                -- "0" => "U"
                                if (cam_array(addr)(i)='0' and x_array(addr)(i)='0') then -- "0"
                                    cam_array_tmp(addr)(i) := '1';
                                    x_array_tmp(addr)(i) := '1';
                                -- "1" => "1"
                                elsif (cam_array(addr)(i)='1' and x_array(addr)(i)='0') then -- "1"
                                    cam_array_tmp(addr)(i) := '1';
                                    x_array_tmp(addr)(i) := '0';
                                -- "X" => "1"
                                elsif (cam_array(addr)(i)='0' and x_array(addr)(i)='1') then -- "X"
                                    cam_array_tmp(addr)(i) := '1';
                                    x_array_tmp(addr)(i) := '0';
                                -- "U" => "U"
                                elsif (cam_array(addr)(i)='1' and x_array(addr)(i)='1') then -- "U"
                                    cam_array_tmp(addr)(i) := '1';
                                    x_array_tmp(addr)(i) := '1';
                                end if;
                            end if;
                        end loop;
                        write0_done <= false;
                        write1_done <= true;
                        writex_done <= false;
                        end if;
                        -------------
                        -- WRITE_X --
                        -------------
                        if writex = true then
                            for i in 0 to width-1 loop
                            if (ipattern(i) xor wrx_int(i)) = '0' then
                                -- "0" => "0"
                                if (cam_array(addr)(i)='0' and x_array(addr)(i)='0') then -- "0"
                                    cam_array_tmp(addr)(i) := '0';
                                    x_array_tmp(addr)(i) := '0';
                                -- "1" => "X"
                                elsif (cam_array(addr)(i)='1' and x_array(addr)(i)='0') then -- "1"
                                    cam_array_tmp(addr)(i) := '0';
                                    x_array_tmp(addr)(i) := '1';
                                -- "X" => "X"
                                elsif (cam_array(addr)(i)='0' and x_array(addr)(i)='1') then -- "X"
                                    cam_array_tmp(addr)(i) := '0';
                                    x_array_tmp(addr)(i) := '1';
                                -- "U" => "0"
                                elsif (cam_array(addr)(i)='1' and x_array(addr)(i)='1') then -- "U"
                                    cam_array_tmp(addr)(i) := '0';
                                    x_array_tmp(addr)(i) := '0';
                                end if;
                            elsif (ipattern(i) xor wrx_int(i)) = '1' then
                                -- "0" => "X"
                                if (cam_array(addr)(i)='0' and x_array(addr)(i)='0') then -- "0"
                                    cam_array_tmp(addr)(i) := '0';
                                    x_array_tmp(addr)(i) := '1';
                                -- "1" => "1"
                                elsif (cam_array(addr)(i)='1' and x_array(addr)(i)='0') then -- "1"
                                    cam_array_tmp(addr)(i) := '1';
                                    x_array_tmp(addr)(i) := '0';
                                -- "X" => "X"
                                elsif (cam_array(addr)(i)='0' and x_array(addr)(i)='1') then -- "X"
                                    cam_array_tmp(addr)(i) := '0';
                                    x_array_tmp(addr)(i) := '1';
                                -- "U" => "1"
                                elsif (cam_array(addr)(i)='1' and x_array(addr)(i)='1') then -- "U"
                                    cam_array_tmp(addr)(i) := '1';
                                    x_array_tmp(addr)(i) := '0';
                                end if;
                            end if;
                        end loop;
                        writex_done <= true;
                        write0_done <= false;
                        write1_done <= false;
                        end if;

                        if wrbusy_int = '1' then
                            write_start_1 <= '1';
                            write_start <= write_start_1;
                        else
                            write_start_1 <= '0';
                            write_start <= '0';
                        end if;
                    else -- 2 Cycle write
                        ------------------- 
                        -- 2 CYCLE WRITE -- 
                        ------------------- 
                        ----------------- 
                        -- WRITE_ZEROS -- 
                        ----------------- 
                        if write0 = true then
                        for i in 0 to width-1 loop
                            if ipattern(i) = '0' then
                                -- "0" => "0"
                                if (cam_array(addr)(i)='0' and x_array(addr)(i)='0') then -- "0"
                                    cam_array_tmp(addr)(i) := '0';
                                    x_array_tmp(addr)(i) := '0';
                                -- "1" => "X"
                                elsif (cam_array(addr)(i)='1' and x_array(addr)(i)='0') then -- "1"
                                    cam_array_tmp(addr)(i) := '0';
                                    x_array_tmp(addr)(i) := '1';
                                -- "X" => "X"
                                elsif (cam_array(addr)(i)='0' and x_array(addr)(i)='1') then -- "X"
                                    cam_array_tmp(addr)(i) := '0';
                                    x_array_tmp(addr)(i) := '1';
                                -- "U" => "0"
                                elsif (cam_array(addr)(i)='1' and x_array(addr)(i)='1') then -- "U"
                                    cam_array_tmp(addr)(i) := '0';
                                    x_array_tmp(addr)(i) := '0';
                                end if;
                            elsif ipattern(i) = '1' then
                                -- "0" => "X"
                                if (cam_array(addr)(i)='0' and x_array(addr)(i)='0') then -- "0"
                                    cam_array_tmp(addr)(i) := '0';
                                    x_array_tmp(addr)(i) := '1';
                                -- "1" => "1"
                                elsif (cam_array(addr)(i)='1' and x_array(addr)(i)='0') then -- "1"
                                    cam_array_tmp(addr)(i) := '1';
                                    x_array_tmp(addr)(i) := '0';
                                -- "X" => "X"
                                elsif (cam_array(addr)(i)='0' and x_array(addr)(i)='1') then -- "X"
                                    cam_array_tmp(addr)(i) := '0';
                                    x_array_tmp(addr)(i) := '1';
                                -- "U" => "1"
                                elsif (cam_array(addr)(i)='1' and x_array(addr)(i)='1') then -- "U"
                                    cam_array_tmp(addr)(i) := '1';
                                    x_array_tmp(addr)(i) := '0';
                                end if;
                            end if;
                        end loop;
                        write0_done <= true;
                        write1_done <= false;
                        writex_done <= false;
                        end if;
                        ----------------
                        -- WRITE_ONES --
                        ----------------
                        if write1 = true then
                        for i in 0 to width-1 loop
                            if ipattern(i) = '0' then
                                -- "0" => "0"
                                if (cam_array(addr)(i)='0' and x_array(addr)(i)='0') then -- "0"
                                    cam_array_tmp(addr)(i) := '0';
                                    x_array_tmp(addr)(i) := '0';
                                -- "1" => "U"
                                elsif (cam_array(addr)(i)='1' and x_array(addr)(i)='0') then -- "1"
                                    cam_array_tmp(addr)(i) := '1';
                                    x_array_tmp(addr)(i) := '1';
                                -- "X" => "0"
                                elsif (cam_array(addr)(i)='0' and x_array(addr)(i)='1') then -- "X"
                                    cam_array_tmp(addr)(i) := '0';
                                    x_array_tmp(addr)(i) := '0';
                                -- "U" => "U"
                                elsif (cam_array(addr)(i)='1' and x_array(addr)(i)='1') then -- "U"
                                    cam_array_tmp(addr)(i) := '1';
                                    x_array_tmp(addr)(i) := '1';
                                end if;
                            elsif ipattern(i) = '1' then
                                -- "0" => "U"
                                if (cam_array(addr)(i)='0' and x_array(addr)(i)='0') then -- "0"
                                    cam_array_tmp(addr)(i) := '1';
                                    x_array_tmp(addr)(i) := '1';
                                -- "1" => "1"
                                elsif (cam_array(addr)(i)='1' and x_array(addr)(i)='0') then -- "1"
                                    cam_array_tmp(addr)(i) := '1';
                                    x_array_tmp(addr)(i) := '0';
                                -- "X" => "1"
                                elsif (cam_array(addr)(i)='0' and x_array(addr)(i)='1') then -- "X"
                                    cam_array_tmp(addr)(i) := '1';
                                    x_array_tmp(addr)(i) := '0';
                                -- "U" => "U"
                                elsif (cam_array(addr)(i)='1' and x_array(addr)(i)='1') then -- "U"
                                    cam_array_tmp(addr)(i) := '1';
                                    x_array_tmp(addr)(i) := '1';
                                end if;
                            end if;
                        end loop;
                        write0_done <= false;
                        write1_done <= true;
                        writex_done <= false;
                        end if;

                        if wrbusy_int = '1' then
                            write_start <= '1';
                        else
                            write_start <= '0';
                        end if;
                    end if; -- wrxused_int
                else -- if wrdelete = '1' then
                    -------------------- 
                    -- 2 CYCLE DELETE -- 
                    -------------------- 
                    -- Delete is a 2-cycle write
                    ----------------
                    -- WRITE_ONES --
                    ----------------
                    if write0 = true then
                        for i in 0 to width-1 loop
                            cam_array_tmp(addr)(i) := '1';
                        end loop;
                        write0_done <= true;
                        write1_done <= false;
                        writex_done <= false;
                    end if;
                    -------------
                    -- WRITE_X --
                    -------------
                    if write1 = true then
                        for i in 0 to width-1 loop
                            x_array_tmp(addr)(i) := '1';
                        end loop;
                        write1_done <= true;
                        write0_done <= false;
                        writex_done <= false;
                    end if;

                    if wrbusy_int = '1' then
                        write_start <= '1';
                    else
                        write_start <= '0';
                    end if;
                end if; -- wrdelete

                cam_array <= cam_array_tmp;
                x_array <= x_array_tmp;

                --------------------------------------
                -- FAST MULTIPLE: READ DURING WRITE --
                --------------------------------------
                -- Now we need to update mbits, mcount during the write.
                if match_mode = "FAST_MULTIPLE" then
                    mfound_int <= '0';
                    maddress_int <= (others => '1');
                    count := 0;
                    mbits_tmp := (others => '0');
                    if (writex = true) and (wrxused_int = '1') then
                        waddr_fm2: for i in 0 to numwords-1 loop
                            wword_fm2: for j in 0 to width-1 loop
                                if (((x_array_tmp(i)(j) = '0') and (cam_array_tmp(i)(j) = (ipattern(j) xor wrx_int(j)))) or 
                                    ((x_array_tmp(i)(j) = '1') and (cam_array_tmp(i)(j) = '0'))) then
                                    if j = width-1 then 
                                        mbits_tmp(i) := '1';
                                        count := count + 1;
                                    end if;
                                else 
                                    exit wword_fm2;
                                end if;
                            end loop wword_fm2;
                        end loop waddr_fm2;
                    else
                        waddr_fm3: for i in 0 to numwords-1 loop
                            wword_fm3: for j in 0 to width-1 loop
                                if (((x_array_tmp(i)(j) = '0') and (cam_array_tmp(i)(j) = ipattern(j))) or 
                                    ((x_array_tmp(i)(j) = '1') and (cam_array_tmp(i)(j) = '0'))) then
                                    if j = width-1 then 
                                        mbits_tmp(i) := '1';
                                        count := count + 1;
                                    end if;
                                else 
                                    exit wword_fm3;
                                end if;
                            end loop wword_fm3;
                        end loop waddr_fm3;
                    end if;
                    mcount_int <= ieee.std_logic_arith.conv_std_logic_vector(count, widthad);
                    mbits_int <= mbits_tmp;
                end if; -- end of FAST MULTIPLE

                ---------------------------------
                -- MULTIPLE: READ DURING WRITE --
                ---------------------------------
                -- THIS IMPLEMENTATION IS INACCURATE
                if match_mode = "MULTIPLE" then
                    mfound_int <= '0';
                    maddress_int <= (others => '1');
                    mbits_tmp := (others => '0');
                    if (writex = true) and (wrxused_int = '1') then
                        mcount_int <= (others => '0');
                        first_read_in_write <= false;
                    else
                        if first_read_in_write = false then
                        first_read_in_write <= true;
                        -- Read even addresses but they appear on the odd locations
                        -- of mbits.
                        count := 0;
                        waddr_mm2: for i in 0 to numwords-1 loop
                            if ( (i mod 2) = 0 ) then
                                if (mbits_int(i) = '1') then -- counting previous even address matches
                                    count := count + 1;
                                end if;
                            wword_mm2: for j in 0 to width-1 loop
                                if (((x_array_tmp(i)(j) = '0') and (cam_array_tmp(i)(j) = ipattern(j))) or 
                                    ((x_array_tmp(i)(j) = '1') and (cam_array_tmp(i)(j) = '0'))) then
                                    if j = width-1 then 
                                        mbits_tmp(i+1) := '1';
                                        count := count + 1;
                                    end if;
                                else 
                                    exit wword_mm2;
                                end if;
                            end loop wword_mm2;
                            end if;
                        end loop waddr_mm2;
                        else
                        first_read_in_write <= false;
                        --  Read odd addresses. 
                        count := 0;
                        waddr_mm3: for i in numwords-1 downto 0 loop
                                if (i mod 2) = 1 then
                                mbits_tmp(i-1) := mbits_tmp(i);
                            else
                            wword_mm3: for j in 0 to width-1 loop
                                if (((x_array_tmp(i)(j) = '0') and (cam_array_tmp(i)(j) = ipattern(j))) or 
                                    ((x_array_tmp(i)(j) = '1') and (cam_array_tmp(i)(j) = '0'))) then
                                    if j = width-1 then 
                                        mbits_tmp(i) := '1';
                                        count := count + 1;
                                    end if;
                                else 
                                    exit wword_mm3;
                                end if;
                            end loop wword_mm3;
                            end if;
                        end loop waddr_mm3;
                        end if;
                        mcount_int <= ieee.std_logic_arith.conv_std_logic_vector(count, widthad);
                        mbits_int <= mbits_tmp;
                    end if;
                end if; -- end of MULTIPLE
            
                ------------------------------
                -- SINGLE: READ DURING WRITE --
                ------------------------------
                if match_mode = "SINGLE" then
                    mbits_tmp := (others => '0');
                    index := 0;
                    count := 0;
                    if (writex = true) and (wrxused_int = '1') then
                        waddr_sm2: for i in 0 to numwords-1 loop
                            wword_sm2: for j in 0 to width-1 loop
                                if (((x_array_tmp(i)(j) = '0') and (cam_array_tmp(i)(j) = (ipattern(j) xor wrx_int(j)))) or 
                                    ((x_array_tmp(i)(j) = '1') and (cam_array_tmp(i)(j) = '0'))) then
                                    if j = width-1 then 
                                        mbits_tmp(i) := '1';
                                        index := i;
                                        count := 1;
                                        exit waddr_sm2;
                                    end if;
                                else 
                                    exit wword_sm2;
                                end if;
                            end loop wword_sm2;
                        end loop waddr_sm2;
                    else
                        waddr_sm3: for i in 0 to numwords-1 loop
                            wword_sm3: for j in 0 to width-1 loop
                                if (((x_array_tmp(i)(j) = '0') and (cam_array_tmp(i)(j) = ipattern(j))) or 
                                    ((x_array_tmp(i)(j) = '1') and (cam_array_tmp(i)(j) = '0'))) then
                                    if j = width-1 then 
                                        mbits_tmp(i) := '1';
                                        index := i;
                                        count := 1;
                                        exit waddr_sm3;
                                    end if;
                                else 
                                    exit wword_sm3;
                                end if;
                            end loop wword_sm3;
                        end loop waddr_sm3;
                    end if;
                    mcount_int <= ieee.std_logic_arith.conv_std_logic_vector(count, widthad);
                    mbits_int <= mbits_tmp;
                    if count = 0 then
                        mfound_int <= '0';
                        maddress_int <= (others => '0');
                    else
                        mfound_int <= '1';
                        maddress_int <= ieee.std_logic_arith.conv_std_logic_vector(index, widthad);
                    end if;
                end if; -- end of SINGLE
            end if; -- end of Write
          end if; -- end of inclock edges
        else -- if the pattern changes
            -- Only updating mbits, mcount, mfound and maddress if
            -- the pattern input in unregistered, wren_rgd='0' and the pattern
            -- patterb changes.
            if pattern'event and (pattern_reg="UNREGISTERED") then
            if wren_rgd='0' then
                ----------------------------------------
                -- FAST MULTIPLE: READ ON NEW PATTERN --
                ----------------------------------------
                if match_mode = "FAST_MULTIPLE" then
                    count := 0;
                    mbits_tmp := (others => '0');
                    maddr_fm2: for i in 0 to numwords-1 loop
                        mword_fm2: for j in 0 to width-1 loop
                            if (((x_array(i)(j) = '0') and (cam_array(i)(j) = pattern(j))) or 
                                ((x_array(i)(j) = '1') and (cam_array(i)(j) = '0'))) then
                                if j = width-1 then 
                                    if (count = 0) and (reset_read = false) then
                                        mfound_int <= '1';
                                        maddress_int <= ieee.std_logic_arith.conv_std_logic_vector(i, widthad);
                                    end if;
                                    mbits_tmp(i) := '1';
                                    count := count + 1;
                                end if;
                            else 
                                exit mword_fm2;
                            end if;
                        end loop mword_fm2;
                    end loop maddr_fm2;
                    mcount_int <= ieee.std_logic_arith.conv_std_logic_vector(count, widthad);
                    mbits_int <= mbits_tmp;
                    if (count = 0) or (reset_read = true) then
                        mfound_int <= '0';
                        maddress_int <= (others => '1');
                    end if;
                end if; -- end of FAST MULTIPLE

                -----------------------------------
                -- MULTIPLE: READ ON NEW PATTERN --
                -----------------------------------
                if match_mode = "MULTIPLE" then
                     count := 0;
                     mbits_tmp := mbits_int;
                     if reset_read = true then
                        maddr_mm4: for i in 0 to numwords-1 loop
                            if (i mod 2)=0 then
                                mword_mm4: for j in 0 to width-1 loop
                                    if (((x_array(i)(j) = '0') and (cam_array(i)(j) = pattern(j))) or 
                                        ((x_array(i)(j) = '1') and (cam_array(i)(j) = '0'))) then
                                        if j = width-1 then 
                                            mbits_tmp(i+1) := '1';
                                            count := count + 1;
                                        end if;
                                    else 
                                        mbits_tmp(i+1) := '0';
                                        exit mword_mm4;
                                    end if;
                                end loop mword_mm4;
                            end if;
                        end loop maddr_mm4;
                    else
                        -- Match odd addresses and write to odd
                        maddr_mm5: for i in 0 to numwords-1 loop
                            if (i mod 2)=1 then
                                mword_mm5: for j in 0 to width-1 loop
                                    if (((x_array(i)(j) = '0') and (cam_array(i)(j) = pattern(j))) or 
                                        ((x_array(i)(j) = '1') and (cam_array(i)(j) = '0'))) then
                                        if j = width-1 then 
                                            mbits_tmp(i) := '1';
                                            if (count = 0) then
                                                maddress_int <= ieee.std_logic_arith.conv_std_logic_vector(i, widthad);
                                            end if;
                                            count := count + 1;
                                        end if;
                                    else 
                                        mbits_tmp(i) := '0';
                                        exit mword_mm5;
                                    end if;
                                end loop mword_mm5;
                            else
                                if mbits_tmp(i) = '1' then
                                    if (count = 0) then
                                        maddress_int <= ieee.std_logic_arith.conv_std_logic_vector(i, widthad);
                                    end if;
                                    count := count + 1;
                                end if;
                            end if;
                        end loop maddr_mm5;
                        if count > 0 then
                            mfound_int <= '1';
                        else
                            mfound_int <= '0';
                            maddress_int <= (others => '1');
                        end if;
                    end if;
                    mcount_int <= ieee.std_logic_arith.conv_std_logic_vector(count, widthad);
                    mbits_int <= mbits_tmp;
                end if;

                ---------------------------------
                -- SINGLE: READ ON NEW PATTERN --
                ---------------------------------
                if match_mode = "SINGLE" then
                    mbits_tmp := (others => '0');
                    index := 0;
                    count := 0;
                    maddr_sm1: for i in 0 to numwords-1 loop
                        mword_sm1: for j in 0 to width-1 loop
                           if (((x_array(i)(j) = '0') and (cam_array(i)(j) = pattern(j))) or 
                               ((x_array(i)(j) = '1') and (cam_array(i)(j) = '0'))) then
                               if j = width-1 then 
                                   mbits_tmp(i) := '1';
                                   count := 1;
                                   index := i;
                                   exit maddr_sm1;
                                end if;
                            else
                                exit mword_sm1;
                            end if;
                        end loop mword_sm1;
                    end loop maddr_sm1;
                    mcount_int <= ieee.std_logic_arith.conv_std_logic_vector(count, widthad);
                    mbits_int <= mbits_tmp;
                    if count = 0 then
                        maddress_int <= (others => '0');
                        mfound_int <= '0';
                    else
                        mfound_int <= '1';
                        maddress_int <= ieee.std_logic_arith.conv_std_logic_vector(index, widthad);
                    end if;
                end if; -- end of SINGLE
            else
            -- We write to the CAM on the low cycle of inclock 
            -- when wren_rgd='1' and pattern changes.
            if (wren_rgd='1') and (inclock='0') then
                addr := ieee.std_logic_unsigned.conv_integer(wraddress_rgd);
                cam_array_tmp := cam_array;
                x_array_tmp := x_array;
                ---------------------
                -- CAM WRITE MODES --
                ---------------------
                if wrdelete_rgd = '0' then
                    if (wrxused_int = '1') and (wrx_reg /= "UNUSED") and (wrx_aclr /= "UNUSED") then
                        ------------------- 
                        -- 3 CYCLE WRITE -- 
                        ------------------- 
                        ----------------- 
                        -- WRITE_ZEROS -- 
                        ----------------- 
                        if write0_done = true then
                        for i in 0 to width-1 loop
                            if pattern(i) = '0' then
                                -- "0" => "0"
                                if (cam_array(addr)(i)='0' and x_array(addr)(i)='0') then -- "0"
                                    cam_array_tmp(addr)(i) := '0';
                                    x_array_tmp(addr)(i) := '0';
                                -- "1" => "X"
                                elsif (cam_array(addr)(i)='1' and x_array(addr)(i)='0') then -- "1"
                                    cam_array_tmp(addr)(i) := '0';
                                    x_array_tmp(addr)(i) := '1';
                                -- "X" => "X"
                                elsif (cam_array(addr)(i)='0' and x_array(addr)(i)='1') then -- "X"
                                    cam_array_tmp(addr)(i) := '0';
                                    x_array_tmp(addr)(i) := '1';
                                -- "U" => "0"
                                elsif (cam_array(addr)(i)='1' and x_array(addr)(i)='1') then -- "U"
                                    cam_array_tmp(addr)(i) := '0';
                                    x_array_tmp(addr)(i) := '0';
                                end if;
                            elsif pattern(i) = '1' then
                                -- "0" => "X"
                                if (cam_array(addr)(i)='0' and x_array(addr)(i)='0') then -- "0"
                                    cam_array_tmp(addr)(i) := '0';
                                    x_array_tmp(addr)(i) := '1';
                                -- "1" => "1"
                                elsif (cam_array(addr)(i)='1' and x_array(addr)(i)='0') then -- "1"
                                    cam_array_tmp(addr)(i) := '1';
                                    x_array_tmp(addr)(i) := '0';
                                -- "X" => "X"
                                elsif (cam_array(addr)(i)='0' and x_array(addr)(i)='1') then -- "X"
                                    cam_array_tmp(addr)(i) := '0';
                                    x_array_tmp(addr)(i) := '1';
                                -- "U" => "1"
                                elsif (cam_array(addr)(i)='1' and x_array(addr)(i)='1') then -- "U"
                                    cam_array_tmp(addr)(i) := '1';
                                    x_array_tmp(addr)(i) := '0';
                                end if;
                            end if;
                        end loop;
                        end if;
                        ----------------
                        -- WRITE_ONES --
                        ----------------
                        if write1_done = true then
                        for i in 0 to width-1 loop
                            if pattern(i) = '0' then
                                -- "0" => "0"
                                if (cam_array(addr)(i)='0' and x_array(addr)(i)='0') then -- "0"
                                    cam_array_tmp(addr)(i) := '0';
                                    x_array_tmp(addr)(i) := '0';
                                -- "1" => "U"
                                elsif (cam_array(addr)(i)='1' and x_array(addr)(i)='0') then -- "1"
                                    cam_array_tmp(addr)(i) := '1';
                                    x_array_tmp(addr)(i) := '1';
                                -- "X" => "0"
                                elsif (cam_array(addr)(i)='0' and x_array(addr)(i)='1') then -- "X"
                                    cam_array_tmp(addr)(i) := '0';
                                    x_array_tmp(addr)(i) := '0';
                                -- "U" => "U"
                                elsif (cam_array(addr)(i)='1' and x_array(addr)(i)='1') then -- "U"
                                    cam_array_tmp(addr)(i) := '1';
                                    x_array_tmp(addr)(i) := '1';
                                end if;
                            elsif pattern(i) = '1' then
                                -- "0" => "U"
                                if (cam_array(addr)(i)='0' and x_array(addr)(i)='0') then -- "0"
                                    cam_array_tmp(addr)(i) := '1';
                                    x_array_tmp(addr)(i) := '1';
                                -- "1" => "1"
                                elsif (cam_array(addr)(i)='1' and x_array(addr)(i)='0') then -- "1"
                                    cam_array_tmp(addr)(i) := '1';
                                    x_array_tmp(addr)(i) := '0';
                                -- "X" => "1"
                                elsif (cam_array(addr)(i)='0' and x_array(addr)(i)='1') then -- "X"
                                    cam_array_tmp(addr)(i) := '1';
                                    x_array_tmp(addr)(i) := '0';
                                -- "U" => "U"
                                elsif (cam_array(addr)(i)='1' and x_array(addr)(i)='1') then -- "U"
                                    cam_array_tmp(addr)(i) := '1';
                                    x_array_tmp(addr)(i) := '1';
                                end if;
                            end if;
                        end loop;
                        end if;
                        -------------
                        -- WRITE_X --
                        -------------
                        if writex_done = true then
                            for i in 0 to width-1 loop
                            if (pattern(i) xor wrx_int(i)) = '0' then
                                -- "0" => "0"
                                if (cam_array(addr)(i)='0' and x_array(addr)(i)='0') then -- "0"
                                    cam_array_tmp(addr)(i) := '0';
                                    x_array_tmp(addr)(i) := '0';
                                -- "1" => "X"
                                elsif (cam_array(addr)(i)='1' and x_array(addr)(i)='0') then -- "1"
                                    cam_array_tmp(addr)(i) := '0';
                                    x_array_tmp(addr)(i) := '1';
                                -- "X" => "X"
                                elsif (cam_array(addr)(i)='0' and x_array(addr)(i)='1') then -- "X"
                                    cam_array_tmp(addr)(i) := '0';
                                    x_array_tmp(addr)(i) := '1';
                                -- "U" => "0"
                                elsif (cam_array(addr)(i)='1' and x_array(addr)(i)='1') then -- "U"
                                    cam_array_tmp(addr)(i) := '0';
                                    x_array_tmp(addr)(i) := '0';
                                end if;
                            elsif (pattern(i) xor wrx_int(i)) = '1' then
                                -- "0" => "X"
                                if (cam_array(addr)(i)='0' and x_array(addr)(i)='0') then -- "0"
                                    cam_array_tmp(addr)(i) := '0';
                                    x_array_tmp(addr)(i) := '1';
                                -- "1" => "1"
                                elsif (cam_array(addr)(i)='1' and x_array(addr)(i)='0') then -- "1"
                                    cam_array_tmp(addr)(i) := '1';
                                    x_array_tmp(addr)(i) := '0';
                                -- "X" => "X"
                                elsif (cam_array(addr)(i)='0' and x_array(addr)(i)='1') then -- "X"
                                    cam_array_tmp(addr)(i) := '0';
                                    x_array_tmp(addr)(i) := '1';
                                -- "U" => "1"
                                elsif (cam_array(addr)(i)='1' and x_array(addr)(i)='1') then -- "U"
                                    cam_array_tmp(addr)(i) := '1';
                                    x_array_tmp(addr)(i) := '0';
                                end if;
                            end if;
                        end loop;
                        end if;
                    else -- 2 Cycle write
                        ------------------- 
                        -- 2 CYCLE WRITE -- 
                        ------------------- 
                        ----------------- 
                        -- WRITE_ZEROS -- 
                        ----------------- 
                        if write0_done = true then
                        for i in 0 to width-1 loop
                            if pattern(i) = '0' then
                                -- "0" => "0"
                                if (cam_array(addr)(i)='0' and x_array(addr)(i)='0') then -- "0"
                                    cam_array_tmp(addr)(i) := '0';
                                    x_array_tmp(addr)(i) := '0';
                                -- "1" => "X"
                                elsif (cam_array(addr)(i)='1' and x_array(addr)(i)='0') then -- "1"
                                    cam_array_tmp(addr)(i) := '0';
                                    x_array_tmp(addr)(i) := '1';
                                -- "X" => "X"
                                elsif (cam_array(addr)(i)='0' and x_array(addr)(i)='1') then -- "X"
                                    cam_array_tmp(addr)(i) := '0';
                                    x_array_tmp(addr)(i) := '1';
                                -- "U" => "0"
                                elsif (cam_array(addr)(i)='1' and x_array(addr)(i)='1') then -- "U"
                                    cam_array_tmp(addr)(i) := '0';
                                    x_array_tmp(addr)(i) := '0';
                                end if;
                            elsif pattern(i) = '1' then
                                -- "0" => "X"
                                if (cam_array(addr)(i)='0' and x_array(addr)(i)='0') then -- "0"
                                    cam_array_tmp(addr)(i) := '0';
                                    x_array_tmp(addr)(i) := '1';
                                -- "1" => "1"
                                elsif (cam_array(addr)(i)='1' and x_array(addr)(i)='0') then -- "1"
                                    cam_array_tmp(addr)(i) := '1';
                                    x_array_tmp(addr)(i) := '0';
                                -- "X" => "X"
                                elsif (cam_array(addr)(i)='0' and x_array(addr)(i)='1') then -- "X"
                                    cam_array_tmp(addr)(i) := '0';
                                    x_array_tmp(addr)(i) := '1';
                                -- "U" => "1"
                                elsif (cam_array(addr)(i)='1' and x_array(addr)(i)='1') then -- "U"
                                    cam_array_tmp(addr)(i) := '1';
                                    x_array_tmp(addr)(i) := '0';
                                end if;
                            end if;
                        end loop;
                        end if;
                        ----------------
                        -- WRITE_ONES --
                        ----------------
                        if write1_done = true then
                        for i in 0 to width-1 loop
                            if pattern(i) = '0' then
                                -- "0" => "0"
                                if (cam_array(addr)(i)='0' and x_array(addr)(i)='0') then -- "0"
                                    cam_array_tmp(addr)(i) := '0';
                                    x_array_tmp(addr)(i) := '0';
                                -- "1" => "U"
                                elsif (cam_array(addr)(i)='1' and x_array(addr)(i)='0') then -- "1"
                                    cam_array_tmp(addr)(i) := '1';
                                    x_array_tmp(addr)(i) := '1';
                                -- "X" => "0"
                                elsif (cam_array(addr)(i)='0' and x_array(addr)(i)='1') then -- "X"
                                    cam_array_tmp(addr)(i) := '0';
                                    x_array_tmp(addr)(i) := '0';
                                -- "U" => "U"
                                elsif (cam_array(addr)(i)='1' and x_array(addr)(i)='1') then -- "U"
                                    cam_array_tmp(addr)(i) := '1';
                                    x_array_tmp(addr)(i) := '1';
                                end if;
                            elsif pattern(i) = '1' then
                                -- "0" => "U"
                                if (cam_array(addr)(i)='0' and x_array(addr)(i)='0') then -- "0"
                                    cam_array_tmp(addr)(i) := '1';
                                    x_array_tmp(addr)(i) := '1';
                                -- "1" => "1"
                                elsif (cam_array(addr)(i)='1' and x_array(addr)(i)='0') then -- "1"
                                    cam_array_tmp(addr)(i) := '1';
                                    x_array_tmp(addr)(i) := '0';
                                -- "X" => "1"
                                elsif (cam_array(addr)(i)='0' and x_array(addr)(i)='1') then -- "X"
                                    cam_array_tmp(addr)(i) := '1';
                                    x_array_tmp(addr)(i) := '0';
                                -- "U" => "U"
                                elsif (cam_array(addr)(i)='1' and x_array(addr)(i)='1') then -- "U"
                                    cam_array_tmp(addr)(i) := '1';
                                    x_array_tmp(addr)(i) := '1';
                                end if;
                            end if;
                        end loop;
                        end if;
                    end if; -- wrxused_int
                else -- if wrdelete = '1' then
                    -------------------- 
                    -- 2 CYCLE DELETE -- 
                    -------------------- 
                    -- Delete is a 2-cycle write
                    ----------------
                    -- WRITE_ONES --
                    ----------------
                    if write0_done = true then
                        for i in 0 to width-1 loop
                            cam_array_tmp(addr)(i) := '1';
                        end loop;
                    end if;
                    -------------
                    -- WRITE_X --
                    -------------
                    if write1_done = true then
                        for i in 0 to width-1 loop
                            x_array_tmp(addr)(i) := '1';
                        end loop;
                    end if;
                end if; -- wrdelete

                cam_array <= cam_array_tmp;
                x_array <= x_array_tmp;

                --------------------------------------
                -- FAST MULTIPLE: READ DURING WRITE --
                --------------------------------------
                -- Now we need to update mbits, mcount during the write.
                if match_mode = "FAST_MULTIPLE" then
                    mfound_int <= '0';
                    maddress_int <= (others => '1');
                    count := 0;
                    mbits_tmp := (others => '0');
                    if (writex_done = true) and (wrxused_int = '1') then
                        waddr_fm_2: for i in 0 to numwords-1 loop
                            wword_fm_2: for j in 0 to width-1 loop
                                if (((x_array_tmp(i)(j) = '0') and (cam_array_tmp(i)(j) = (pattern(j) xor wrx_int(j)))) or 
                                    ((x_array_tmp(i)(j) = '1') and (cam_array_tmp(i)(j) = '0'))) then
                                    if j = width-1 then 
                                        mbits_tmp(i) := '1';
                                        count := count + 1;
                                    end if;
                                else 
                                    exit wword_fm_2;
                                end if;
                            end loop wword_fm_2;
                        end loop waddr_fm_2;
                    else
                        waddr_fm_3: for i in 0 to numwords-1 loop
                            wword_fm_3: for j in 0 to width-1 loop
                                if (((x_array_tmp(i)(j) = '0') and (cam_array_tmp(i)(j) = pattern(j))) or 
                                    ((x_array_tmp(i)(j) = '1') and (cam_array_tmp(i)(j) = '0'))) then
                                    if j = width-1 then 
                                        mbits_tmp(i) := '1';
                                        count := count + 1;
                                    end if;
                                else 
                                    exit wword_fm_3;
                                end if;
                            end loop wword_fm_3;
                        end loop waddr_fm_3;
                    end if;
                    mcount_int <= ieee.std_logic_arith.conv_std_logic_vector(count, widthad);
                    mbits_int <= mbits_tmp;
                end if; -- end of FAST MULTIPLE

                ---------------------------------
                -- MULTIPLE: READ DURING WRITE --
                ---------------------------------
                -- THIS IMPLEMENTATION IS INACCURATE
                if match_mode = "MULTIPLE" then
                    mfound_int <= '0';
                    maddress_int <= (others => '1');
                    mbits_tmp := (others => '0');
                    if (writex_done = true) and (wrxused_int = '1') then
                        mcount_int <= (others => '0');
                        first_read_in_write <= false;
                    else
                        if first_read_in_write = false then
                        first_read_in_write <= true;
                        -- Read even addresses but they appear on the odd locations
                        -- of mbits.
                        count := 0;
                        waddr_mm_2: for i in 0 to numwords-1 loop
                            if ( (i mod 2) = 0 ) then
                                if (mbits_int(i) = '1') then -- counting previous even address matches
                                    count := count + 1;
                                end if;
                            wword_mm_2: for j in 0 to width-1 loop
                                if (((x_array_tmp(i)(j) = '0') and (cam_array_tmp(i)(j) = pattern(j))) or 
                                    ((x_array_tmp(i)(j) = '1') and (cam_array_tmp(i)(j) = '0'))) then
                                    if j = width-1 then 
                                        mbits_tmp(i+1) := '1';
                                        count := count + 1;
                                    end if;
                                else 
                                    exit wword_mm_2;
                                end if;
                            end loop wword_mm_2;
                            end if;
                        end loop waddr_mm_2;
                        else
                        first_read_in_write <= false;
                        --  Read odd addresses. 
                        count := 0;
                        waddr_mm_3: for i in numwords-1 downto 0 loop
                                if (i mod 2) = 1 then
                                mbits_tmp(i-1) := mbits_tmp(i);
                            else
                            wword_mm_3: for j in 0 to width-1 loop
                                if (((x_array_tmp(i)(j) = '0') and (cam_array_tmp(i)(j) = pattern(j))) or 
                                    ((x_array_tmp(i)(j) = '1') and (cam_array_tmp(i)(j) = '0'))) then
                                    if j = width-1 then 
                                        mbits_tmp(i) := '1';
                                        count := count + 1;
                                    end if;
                                else 
                                    exit wword_mm_3;
                                end if;
                            end loop wword_mm_3;
                            end if;
                        end loop waddr_mm_3;
                        end if;
                        mcount_int <= ieee.std_logic_arith.conv_std_logic_vector(count, widthad);
                        mbits_int <= mbits_tmp;
                    end if;
                end if; -- end of MULTIPLE
            
                ------------------------------
                -- SINGLE: READ DURING WRITE --
                ------------------------------
                if match_mode = "SINGLE" then
                    mbits_tmp := (others => '0');
                    index := 0;
                    count := 0;
                    if (writex_done = true) and (wrxused_int = '1') then
                        waddr_sm_2: for i in 0 to numwords-1 loop
                            wword_sm_2: for j in 0 to width-1 loop
                                if (((x_array_tmp(i)(j) = '0') and (cam_array_tmp(i)(j) = (pattern(j) xor wrx_int(j)))) or 
                                    ((x_array_tmp(i)(j) = '1') and (cam_array_tmp(i)(j) = '0'))) then
                                    if j = width-1 then 
                                        mbits_tmp(i) := '1';
                                        index := i;
                                        count := 1;
                                        exit waddr_sm_2;
                                    end if;
                                else 
                                    exit wword_sm_2;
                                end if;
                            end loop wword_sm_2;
                        end loop waddr_sm_2;
                    else
                        waddr_sm_3: for i in 0 to numwords-1 loop
                            wword_sm_3: for j in 0 to width-1 loop
                                if (((x_array_tmp(i)(j) = '0') and (cam_array_tmp(i)(j) = pattern(j))) or 
                                    ((x_array_tmp(i)(j) = '1') and (cam_array_tmp(i)(j) = '0'))) then
                                    if j = width-1 then 
                                        mbits_tmp(i) := '1';
                                        index := i;
                                        count := 1;
                                        exit waddr_sm_3;
                                    end if;
                                else 
                                    exit wword_sm_3;
                                end if;
                            end loop wword_sm_3;
                        end loop waddr_sm_3;
                    end if;
                    mcount_int <= ieee.std_logic_arith.conv_std_logic_vector(count, widthad);
                    mbits_int <= mbits_tmp;
                    if count = 0 then
                        mfound_int <= '0';
                        maddress_int <= (others => '0');
                    else
                        mfound_int <= '1';
                        maddress_int <= ieee.std_logic_arith.conv_std_logic_vector(index, widthad);
                    end if;
                end if; -- end of SINGLE
                end if; -- end of Write
            end if; -- end of wren_rgd check
            end if; -- end of pattern change
        end if;  -- end of inclock event
    end process read_write;
    
    outputreg: process (outaclr_int, outclock, inclock)
    begin
        if output_reg = "OUTCLOCK" or output_reg = "INCLOCK" then
            if (outaclr_int = '1') and ((output_aclr = "ON") or (output_aclr = "OFF" and output_reg = "INCLOCK")) then 
                    maddress_rgd <= (others => '0');
                    mbits_rgd <= (others => '0');
                    mfound_rgd <= '0';
                    mcount_rgd <= (others => '0');
            else
                if (outclock'event and outclock = '1' and outclocken = '1' and output_reg = "OUTCLOCK") or
                   (inclock'event and inclock = '1' and inclocken = '1' and output_reg = "INCLOCK") then
                    mbits_rgd <= mbits_int;
                    mcount_rgd <= mcount_int;
                    mfound_rgd <= mfound_int;
                    maddress_rgd <= maddress_int;
                end if;
            end if;
        end if;
    end process outputreg;

    write_busy_control: process(wren_rgd, wraddress_rgd, write_start_rgd) 
    begin
        if wren_rgd'event and wren_rgd = '0' then 
            wrbusy_int <= '0';
        elsif wren_rgd'event and wren_rgd = '1' then 
            wrbusy_int <= '1';
        elsif wraddress_rgd'event and wren_rgd = '1' then
            wrbusy_int <= '1';
        elsif write_start_rgd'event and write_start_rgd = '1' then
            wrbusy_int <= '0';
          elsif write_start_rgd'event and write_start_rgd = '0' and wren_rgd = '1' then
            wrbusy_int <= '1';
        end if;
    end process write_busy_control;

    write_control: process(write0_done, write1_done, writex_done, wrbusy_int)
    begin
        if wrbusy_int'event and wrbusy_int = '0' then 
            write0 <= false;
        end if;
        if wrbusy_int'event and wrbusy_int = '1' then
            write0 <= true;
            write1 <= false;
            writex <= false;
        else
            if write0_done'event and write0_done = true then
                write1 <= true;
                if (wrxused_int = '1') and (wrx_reg /= "UNUSED") and (wrx_aclr /= "UNUSED") then
                    write0 <= false;
                end if;
                else
                if write1_done'event and write1_done = true then
                    if (wrxused_int = '1') and (wrx_reg /= "UNUSED") and (wrx_aclr /= "UNUSED") then
                        writex <= true;
                    else 
                        writex <= false;
                    end if;
                    write1 <= false;
                else
                    if writex_done'event and writex_done = true then
                        write0 <= false;
                        write1 <= false;
                        writex <= false;
                    end if;
                end if;
            end if;
        end if;
    end process write_control;

    read_control: process(mstart_rgd1, mstart_rgd2, mnext, mstart_used)
    begin
        if mstart_used'event then
            -- 1st toggle of mstart
            get_next_match <= false;
        else
            if mstart_rgd1'event and mstart_rgd1 = '1' then
                if match_mode = "SINGLE" or match_mode = "FAST_MULTIPLE" then
                    rdbusy_int <= '0';
                else
                    rdbusy_int <= '1';  -- must be "MULTIPLE" mode
                end if;
            elsif mstart_rgd2'event and mstart_rgd2 = '1' then
                rdbusy_int <= '0'; 
            end if;
            if mnext'event  and mnext = '1' and get_first_match = true then
                get_next_match <= true;
            elsif mnext'event and mnext = '0' then
                get_next_match <= false;
            end if;
        end if;
    end process read_control;
end;



----------------------------------------------------------------------------
-- altclklock
----------------------------------------------------------------------------
--
-- Restrictions: 
--   1. NORMAL operation mode only 
--   2. Zero delay outputs
--
library ieee;
use ieee.std_logic_1164.all;

entity altclklock is
    generic
      ( inclock_period          : natural := 10000;
        inclock_settings        : string := "UNUSED";
        valid_lock_cycles       : natural := 5;
        invalid_lock_cycles     : natural := 5;
        valid_lock_multiplier   : natural := 1;
        invalid_lock_multiplier : natural := 1;
        operation_mode          : string := "NORMAL";
        clock0_boost            : natural := 1;
        clock0_divide           : natural := 1;
        clock1_boost            : natural := 1;
        clock1_divide           : natural := 1;
        clock2_boost            : natural := 1;
        clock2_divide           : natural := 1;
        clock_ext_boost         : natural := 1;
        clock_ext_divide        : natural := 1;
        clock0_settings         : string := "UNUSED";
        clock1_settings         : string := "UNUSED";
        clock2_settings         : string := "UNUSED";
        clock_ext_settings      : string := "UNUSED";
        outclock_phase_shift    : natural := 0;
        clock0_time_delay       : string := "0";
        clock1_time_delay       : string := "0";
        clock2_time_delay       : string := "0";
        clock_ext_time_delay    : string := "0";
        intended_device_family  : string := "APEX20KE" );
    port
      ( inclock   : in std_logic;
        inclocken : in std_logic := '1';
        fbin      : in std_logic := '1';
        clock0    : out std_logic;
        clock1    : out std_logic;
        clock2    : out std_logic;
        clock_ext : out std_logic;
        locked    : out std_logic );

    function time_delay ( s : string ) return TIME is
	variable len : integer := s'length;
	variable outclock_phase_shift_adj : integer := 0;
	variable sign : integer := 1;
	variable digit : integer;
	begin
        for i in 1 to len loop
			case s(i) is
				when '-' =>
                                        if i = 1 then
					    sign := -1;
                                        else
 				       	    ASSERT FALSE
					    REPORT "Illegal Character "&  s(i) & "in string parameter! "
					    SEVERITY ERROR;
                                        end if;
				when '0' =>
					digit := 0;
				when '1' =>
					digit := 1;
				when '2' =>
					digit := 2;
				when '3' =>
					digit := 3;
				when '4' =>
					digit := 4;
				when '5' =>
					digit := 5;
				when '6' =>
					digit := 6;
				when '7' =>
					digit := 7;
				when '8' =>
					digit := 8;
				when '9' =>
					digit := 9;
				when others =>
					ASSERT FALSE
					REPORT "Illegal Character "&  s(i) & "in string parameter! "
					SEVERITY ERROR;
			end case;
			outclock_phase_shift_adj := outclock_phase_shift_adj * 10 + digit;
	end loop;
        outclock_phase_shift_adj := outclock_phase_shift + sign*outclock_phase_shift_adj;
        while (outclock_phase_shift_adj < 0) loop
            outclock_phase_shift_adj := outclock_phase_shift_adj + inclock_period;
        end loop;
        while (outclock_phase_shift_adj >= inclock_period) loop
            outclock_phase_shift_adj := outclock_phase_shift_adj - inclock_period;
        end loop;
	return  outclock_phase_shift_adj * 1 ps;
    end;
end altclklock;

architecture behavior of altclklock is

signal pll0_half_period : TIME;
signal pll1_half_period : TIME;
signal pll2_half_period : TIME;
signal pll_ext_half_period : TIME;
signal new_clock0 : std_logic;
signal new_clock1 : std_logic;
signal new_clock2 : std_logic;
signal new_clock_ext : std_logic;
signal start_new_clock0 : std_logic;
signal start_new_clock1 : std_logic;
signal start_new_clock2 : std_logic;
signal start_new_clock_ext : std_logic;
signal phase_delay0 : TIME;
signal phase_delay1 : TIME;
signal phase_delay2 : TIME;
signal phase_delay_ext : TIME;
signal locked_int : std_logic := '0';
signal highcycle, lowcycle: time := 0 ps;
signal check_lock : std_logic := '0';

begin

    lock: process(inclock, inclocken, check_lock)    
    variable valid_inclock_edge_count       : integer := 0;
    variable invalid_inclock_edge_count     : integer := 0;
    variable inclock_started          : boolean := false;
    variable check_lock_time          : TIME;
    variable clk_per_tolerance : time;
    begin
    
        --clk_per_tolerance := 0.05 * 1 ps * inclock_period; 
        clk_per_tolerance := 1 ps; 
        if locked_int = '0' and inclocken = '1' then
            if inclock'event then
                locked <= '0';
            end if;
            check_lock_time := real(real(inclock_period)/2.0) * 1 ps;

            pll0_half_period <= real(real(real(clock0_divide)* real(inclock_period))/(2.0*real(clock0_boost))) * 1 ps;
            pll1_half_period <= real(real(real(clock1_divide)* real(inclock_period))/(2.0*real(clock1_boost))) * 1 ps;
            pll2_half_period <= real(real(real(clock2_divide)* real(inclock_period))/(2.0*real(clock2_boost))) * 1 ps;
            pll_ext_half_period <= real(real(real(clock_ext_divide)* real(inclock_period))/(2.0*real(clock_ext_boost))) * 1 ps;
      
            phase_delay0 <= time_delay(clock0_time_delay);
            phase_delay1 <= time_delay(clock1_time_delay);
            phase_delay2 <=  time_delay(clock2_time_delay);
            phase_delay_ext <= time_delay(clock_ext_time_delay);
    
            if ((inclock = '0') and ((clock0_boost/clock0_divide) mod 2 = 0) and (clock0_boost>clock0_divide)) then
                start_new_clock0 <= NOT inclock;
            else
                start_new_clock0 <= inclock;
            end if;
            if ((inclock = '0') and ((clock1_boost/clock1_divide) mod 2 = 0) and (clock1_boost>clock1_divide)) then
                start_new_clock1 <= NOT inclock;
            else
                start_new_clock1 <= inclock;
            end if;
            if intended_device_family = "MERCURY" then
                if ((inclock = '0') and ((clock2_boost/clock2_divide) mod 2 = 0) and (clock2_boost>clock2_divide)) then
                    start_new_clock2 <= NOT inclock;
                else
                    start_new_clock2 <= inclock;
                end if;
                if ((inclock = '0') and ((clock_ext_boost/clock_ext_divide) mod 2 = 0) and (clock_ext_boost>clock_ext_divide)) then
                    start_new_clock_ext <= NOT inclock;
                else
                    start_new_clock_ext <= inclock;
                end if;
            end if;
        end if;
          
        if inclocken = '0' then
            valid_inclock_edge_count := 0;
            invalid_inclock_edge_count := 0;
            locked_int <= '0';
            locked <= '0';
            inclock_started := false;
        elsif inclock'event then -- any inclock edge
            if locked_int = '1' then
                check_lock <= NOT check_lock after check_lock_time;
            end if;
            if inclock_started = false then
                if inclock = '1' then
                    highcycle <= NOW;
                    inclock_started := true;
                    valid_inclock_edge_count := 1;
                    invalid_inclock_edge_count := 0;
                elsif inclock = '0' then
                    lowcycle <= NOW;
                    inclock_started := true;
                    valid_inclock_edge_count := 0;
                    invalid_inclock_edge_count := 0;
                end if;
            elsif inclock = '1' then
                if (((NOW - lowcycle)*2 < ((1 ps * inclock_period) - clk_per_tolerance)) or ((NOW - lowcycle)*2 > ((1 ps * inclock_period) + clk_per_tolerance))) then
                    invalid_inclock_edge_count := invalid_inclock_edge_count + 1;
                    valid_inclock_edge_count := 1;
                    ASSERT FALSE
                    REPORT "Duty cycle violation"
                    SEVERITY ERROR; 
                else
                    valid_inclock_edge_count := valid_inclock_edge_count + 1;
                    invalid_inclock_edge_count := 0;
                end if;
                highcycle <= NOW;
            elsif inclock = '0' then
                if (((NOW - highcycle)*2 < ((1 ps * inclock_period) - clk_per_tolerance)) or ((NOW - highcycle)*2 > ((1 ps * inclock_period) + clk_per_tolerance))) then
                    invalid_inclock_edge_count := invalid_inclock_edge_count + 1;
                    valid_inclock_edge_count := 1;
                    ASSERT FALSE
                    REPORT "Duty cycle violation"
                    SEVERITY ERROR; 
                else
                    valid_inclock_edge_count := valid_inclock_edge_count + 1;
                    invalid_inclock_edge_count := 0;
                end if;
                lowcycle <= NOW;
            end if;
            if (valid_inclock_edge_count >= valid_lock_cycles) and (NOW > 0 ps) then
                locked_int <= '1';
                locked <= '1';
                valid_inclock_edge_count := 0;
            else
                if (invalid_inclock_edge_count >= invalid_lock_cycles) and 
                    (invalid_inclock_edge_count > 0) and (NOW > 0 ps) then
                    locked_int <= '0';
                    locked <= '0';
                    invalid_inclock_edge_count := 0;
                end if;
            end if;
        else
            if check_lock'event and (locked_int = '1') then
                invalid_inclock_edge_count := invalid_inclock_edge_count + 1;
                if (invalid_inclock_edge_count >= invalid_lock_cycles) and 
                    (invalid_inclock_edge_count > 0) and (NOW > 0 ps) then
                    locked_int <= '0';
                    locked <= '0';
                    ASSERT FALSE
                    REPORT "altclklock out of lock."
                    SEVERITY WARNING; 
                else
                    check_lock <= NOT check_lock after check_lock_time;
                end if;
            end if;
        end if; -- inclocken
    end process;

    gen_clock0: process(new_clock0,locked_int)
    variable first_cycle: boolean := true;
    begin
        if locked_int = '1' then
            if first_cycle = true then
                clock0 <= start_new_clock0;
                if (clock0_divide /= 1) and (valid_lock_cycles mod 2 = 0) then
                    -- Lock on -ve edge of inclock
                    new_clock0 <= not start_new_clock0 after (phase_delay0 + inclock_period/2* 1 ps);
                else
                    new_clock0 <= not start_new_clock0 after phase_delay0 + pll0_half_period;
                end if;
            else
                clock0 <= new_clock0; 
                new_clock0 <= not new_clock0 after pll0_half_period;
            end if;
            first_cycle := false;
        else
            first_cycle := true;
            new_clock0 <= start_new_clock0;
        end if;
    end process;

    gen_clock1: process(new_clock1,locked_int)
    variable first_cycle: boolean := true;
    begin
        if locked_int = '1' then
            if first_cycle = true then
                clock1 <= start_new_clock1;
                if (clock1_divide /= 1) and (valid_lock_cycles mod 2 = 0) then
                    -- Lock on -ve edge of inclock
                    new_clock1 <= not start_new_clock1 after (phase_delay1 + inclock_period/2* 1 ps);
                else
                    new_clock1 <= not start_new_clock1 after phase_delay1 + pll1_half_period;
                end if;
            else
                clock1 <= new_clock1; 
                new_clock1 <= not new_clock1 after pll1_half_period;
            end if;
            first_cycle := false;
        else
            first_cycle := true;
            new_clock1 <= start_new_clock1;
        end if;
    end process;

    gen_clock2: process(new_clock2,locked_int)
    variable first_cycle: boolean := true;
    begin
        if intended_device_family = "MERCURY" then
            if locked_int = '1' then
                if first_cycle = true then
                    clock2 <= start_new_clock2;
                    if (clock2_divide /= 1) and (valid_lock_cycles mod 2 = 0) then
                        -- Lock on -ve edge of inclock
                        new_clock2 <= not start_new_clock2 after (phase_delay2 + inclock_period/2* 1 ps);
                    else
                        new_clock2 <= not start_new_clock2 after phase_delay2 + pll2_half_period;
                    end if;
                else
                    clock2 <= new_clock2; 
                    new_clock2 <= not new_clock2 after pll2_half_period;
                end if;
                first_cycle := false;
            else
                first_cycle := true;
                new_clock2 <= start_new_clock2;
            end if;
        end if;
    end process;

    gen_clock_ext: process(new_clock_ext,locked_int)
    variable first_cycle: boolean := true;
    begin
        if intended_device_family = "MERCURY" then
            if locked_int = '1' then
                if first_cycle = true then
                    clock_ext <= start_new_clock_ext;
                    if (clock_ext_divide /= 1) and (valid_lock_cycles mod 2 = 0) then
                        -- Lock on -ve edge of inclock
                        new_clock_ext <= not start_new_clock_ext after (phase_delay_ext + inclock_period/2* 1 ps);
                    else
                        new_clock_ext <= not start_new_clock_ext after phase_delay_ext + pll_ext_half_period;
                    end if;
                else
                    clock_ext <= new_clock_ext; 
                    new_clock_ext <= not new_clock_ext after pll_ext_half_period;
                end if;
                first_cycle := false;
            else
                first_cycle := true;
                new_clock_ext <= start_new_clock_ext;
            end if;
        end if;
    end process;

end behavior;

----------------------------------------------------------------------------
-- altdpram megafunction
----------------------------------------------------------------------------
    
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use std.textio.all;

entity altdpram is
    generic
      ( width                  : natural;
        widthad                : natural;
        numwords               : natural := 0;
        lpm_file               : string := "UNUSED";
        lpm_hint               : string := "USE_EAB=ON";
        use_eab                : string := "ON";
        indata_reg             : string := "UNREGISTERED";
        indata_aclr            : string := "OFF";
        wraddress_reg          : string := "UNREGISTERED";
        wraddress_aclr         : string := "OFF";
        wrcontrol_reg          : string := "UNREGISTERED";
        wrcontrol_aclr         : string := "OFF";
        rdaddress_reg          : string := "UNREGISTERED";
        rdaddress_aclr         : string := "OFF";
        rdcontrol_reg          : string := "UNREGISTERED";
        rdcontrol_aclr         : string := "OFF";
        outdata_reg            : string := "UNREGISTERED";
        outdata_aclr           : string := "OFF";
        intended_device_family : string := "APEX20KE");
    port
      ( wren       : in std_logic := '0';
        data       : in std_logic_vector(width-1 downto 0);
        wraddress  : in std_logic_vector(widthad-1 downto 0);
        inclock    : in std_logic := '0';
        inclocken  : in std_logic := '1';
        rden       : in std_logic := '1';
        rdaddress  : in std_logic_vector(widthad-1 downto 0);
        outclock   : in std_logic := '0';
        outclocken : in std_logic := '1';
        aclr       : in std_logic := '0';
        q          : out std_logic_vector(width-1 downto 0) );

	function int_to_str( value : integer ) return string is
	variable ivalue,index : integer;
	variable digit : integer;
    variable line_no: string(8 downto 1) := "        ";  
	begin
		ivalue := value;
		index := 1;
		while (ivalue > 0) loop
			digit := ivalue MOD 10;
			ivalue := ivalue/10;
			case digit is
				when 0 =>
					line_no(index) := '0';
				when 1 =>
					line_no(index) := '1';
				when 2 =>
					line_no(index) := '2';
				when 3 =>
					line_no(index) := '3';
				when 4 =>
					line_no(index) := '4';
				when 5 =>
					line_no(index) := '5';
				when 6 =>
					line_no(index) := '6';
				when 7 =>
					line_no(index) := '7';
				when 8 =>
					line_no(index) := '8';
				when 9 =>
					line_no(index) := '9';
				when others =>
					ASSERT FALSE
					REPORT "Illegal number!"
					SEVERITY ERROR;
			end case;
			index := index + 1;
		end loop;
		return line_no;
	end;

	function hex_str_to_int( str : string ) return integer is
	variable len : integer := str'length;
	variable ivalue : integer := 0;
	variable digit : integer;
	begin
		for i in len downto 1 loop
			case str(i) is
				when '0' =>
					digit := 0;
				when '1' =>
					digit := 1;
				when '2' =>
					digit := 2;
				when '3' =>
					digit := 3;
				when '4' =>
					digit := 4;
				when '5' =>
					digit := 5;
				when '6' =>
					digit := 6;
				when '7' =>
					digit := 7;
				when '8' =>
					digit := 8;
				when '9' =>
					digit := 9;
				when 'A' =>
					digit := 10;
				when 'a' =>
					digit := 10;
				when 'B' =>
					digit := 11;
				when 'b' =>
					digit := 11;
				when 'C' =>
					digit := 12;
				when 'c' =>
					digit := 12;
				when 'D' =>
					digit := 13;
				when 'd' =>
					digit := 13;
				when 'E' =>
					digit := 14;
				when 'e' =>
					digit := 14;
				when 'F' =>
					digit := 15;
				when 'f' =>
					digit := 15;
				when others =>
					ASSERT FALSE
					REPORT "Illegal character "&  str(i) & "in Intel Hex File! "
					SEVERITY ERROR;
			end case;
			ivalue := ivalue * 16 + digit;
		end loop;
		return ivalue;
	end;

	procedure Shrink_line(L : inout LINE; pos : in integer) is
	subtype nstring is string(1 to pos);
	variable stmp : nstring;
	begin
		if pos >= 1 then
			read(l, stmp);
		end if;
	end;

end altdpram;

architecture behavior of altdpram is

type alt_memory is array((2**WIDTHAD)-1 downto 0) of std_logic_vector(WIDTH-1 downto 0);

signal idata_tmp, idata_reg : std_logic_vector(WIDTH-1 downto 0) := (OTHERS => '0');
signal idata_hi, idata_lo : std_logic_vector(WIDTH-1 downto 0) := (OTHERS => '0');
signal iq_tmp, iq_reg : std_logic_vector(WIDTH-1 downto 0) := (OTHERS => '0');
signal irdaddress_tmp, irdaddress_reg : std_logic_vector(WIDTHAD-1 downto 0) := (OTHERS => '0');
signal iwraddress_tmp, iwraddress_reg : std_logic_vector(WIDTHAD-1 downto 0) := (OTHERS => '0');
signal iwraddress_hi, iwraddress_lo : std_logic_vector(WIDTHAD-1 downto 0) := (OTHERS => '0');
signal iwren_tmp, iwren_reg : std_logic := '0';
signal iwren_hi, iwren_lo : std_logic := '0';
signal irden_tmp, irden_reg : std_logic := '0';
signal write_at_low_clock : boolean := false;
signal rden_low_output_0 : boolean := false;

begin
    initial: process (inclock, outclock)
    variable init : boolean := false;
    begin
        if (not init) then
            if (lpm_hint = "USE_EAB=ON") or (use_eab = "ON") then
                if (wrcontrol_reg = "INCLOCK") then
                    write_at_low_clock <= true;
                end if;
                if (intended_device_family = "APEX20K") then
                    rden_low_output_0 <= true;
                end if;
            end if;
            init := true;
        end if;
    end process;

    sync: process(data, idata_reg, rden, irden_reg, rdaddress, irdaddress_reg,
                  wren, iwren_reg, wraddress, iwraddress_reg, iq_tmp, iq_reg,
                  aclr)
	begin
        if (rdaddress_reg = "INCLOCK" or rdaddress_reg = "OUTCLOCK") then
            irdaddress_tmp <= irdaddress_reg;
        elsif (rdaddress_reg = "UNREGISTERED") then
            irdaddress_tmp <= rdaddress;
        else
            ASSERT FALSE
            REPORT "Illegal RDADDRESS_REG property value for ALTDPRAM!"
            SEVERITY ERROR;
		end if;
        if (rdcontrol_reg = "INCLOCK" or rdcontrol_reg = "OUTCLOCK") then
            irden_tmp <= irden_reg;
        elsif (rdcontrol_reg = "UNREGISTERED") then
            irden_tmp <= rden;
        else
            ASSERT FALSE
            REPORT "Illegal RDCONTROL_REG property value for ALTDPRAM!"
            SEVERITY ERROR;
		end if;

        if (wraddress_reg = "INCLOCK") then
            iwraddress_tmp <= iwraddress_reg;
        elsif (wraddress_reg = "UNREGISTERED") then
            iwraddress_tmp <= wraddress;
        else
            ASSERT FALSE
            REPORT "Illegal WRADDRESS_REG property value for ALTDPRAM!"
            SEVERITY ERROR;
		end if;
        if (wrcontrol_reg = "INCLOCK") then
            iwren_tmp <= iwren_reg; 
        elsif (wrcontrol_reg = "UNREGISTERED") then
            iwren_tmp <= wren;
        else
            ASSERT FALSE
            REPORT "Illegal WRCONTROL_REG property value for ALTDPRAM!"
            SEVERITY ERROR;
		end if;

        if (indata_reg = "INCLOCK") then
            idata_tmp <= idata_reg;
        elsif (indata_reg = "UNREGISTERED") then
            idata_tmp <= data;
        else
            ASSERT FALSE
            REPORT "Illegal INDATA_REG property value for ALTDPRAM!"
            SEVERITY ERROR;
		end if;
        if (outdata_reg = "OUTCLOCK") then
            q <= iq_reg;
        elsif (outdata_reg = "UNREGISTERED") then
            q <= iq_tmp;
        else
            ASSERT FALSE
            REPORT "Illegal OUTDATA_REG property value for ALTDPRAM!"
            SEVERITY ERROR;
		end if;

        if (aclr = '1') then
            if(indata_aclr = "ON") then
                idata_tmp <= (OTHERS => '0');
            end if;
            if(wraddress_aclr = "ON") then
                iwraddress_tmp <= (OTHERS => '0');
            end if;
            if(wrcontrol_aclr = "ON") then
                iwren_tmp <= '0';
            end if;
            if(rdaddress_aclr = "ON") then
                irdaddress_tmp <= (OTHERS => '0');
            end if;
            if(rdcontrol_aclr = "ON") then
                irden_tmp <= '0';
            end if;
            if(outdata_aclr = "ON") then
                q <= (OTHERS => '0');
            end if;
        end if;
	end process;

    sync2: process(idata_hi, idata_lo, iwraddress_hi, iwraddress_lo,
                   iwren_hi, iwren_lo)
    begin
        if (write_at_low_clock) then
            idata_reg <= idata_lo;
            iwren_reg <= iwren_lo;
            iwraddress_reg <= iwraddress_lo;
        else
            idata_reg <= idata_hi;
            iwren_reg <= iwren_hi;
            iwraddress_reg <= iwraddress_hi;
        end if;
    end process;

    registers: process (inclock, outclock)
	begin

        -- WRITE REGS --
        if (aclr = '1' and indata_aclr = "ON") then
            idata_hi <= (OTHERS => '0');
            idata_lo <= (OTHERS => '0');
        elsif inclock'event and inclock = '1' and inclocken = '1' then
            idata_hi <= data;
        elsif inclock'event and inclock = '0' then
            idata_lo <= idata_hi;
        end if;
        if (aclr = '1' and wraddress_aclr = "ON") then
            iwraddress_hi <= (OTHERS => '0');
            iwraddress_lo <= (OTHERS => '0');
        elsif inclock'event and inclock = '1' and inclocken = '1' then
            iwraddress_hi <= wraddress;
        elsif inclock'event and inclock = '0' then
            iwraddress_lo <= iwraddress_hi;
        end if;
        if (aclr = '1' and wrcontrol_aclr = "ON") then
            iwren_hi <= '0';
            iwren_lo <= '0';
        elsif inclock'event and inclock = '1' and inclocken = '1' then
            iwren_hi <= wren;
        elsif inclock'event and inclock = '0' then
            iwren_lo <= iwren_hi;
        end if;

        -- READ REGS --
        if (aclr = '1' and outdata_aclr = "ON") then
            iq_reg <= (OTHERS => '0');
        elsif outclock'event and outclock = '1' and outclocken = '1' then
            iq_reg <= iq_tmp;
        end if;
        if (rdaddress_reg = "INCLOCK") then
            if (aclr = '1' and rdaddress_aclr = "ON") then
                irdaddress_reg <= (OTHERS => '0');
            elsif inclock'event and inclock = '1' and inclocken = '1' then
                irdaddress_reg <= rdaddress;
            end if;
        end if;
        if (rdcontrol_reg = "INCLOCK") then
            if (aclr = '1' and rdcontrol_aclr = "ON") then
                irden_reg <= '0';
            elsif inclock'event and inclock = '1' and inclocken = '1' then
                irden_reg <= rden; 
            end if;
        end if;
        if (rdaddress_reg = "OUTCLOCK") then
            if (aclr = '1' and rdaddress_aclr = "ON") then
                irdaddress_reg <= (OTHERS => '0');
            elsif outclock'event and outclock = '1' and outclocken = '1' then
                irdaddress_reg <= rdaddress;
            end if;
        end if;
        if (rdcontrol_reg = "OUTCLOCK") then
            if (aclr = '1' and rdcontrol_aclr = "ON") then
                irden_reg <= '0';
            elsif outclock'event and outclock = '1' and outclocken = '1' then
                irden_reg <= rden; 
            end if;
        end if;
	end process;

    memory: process(idata_tmp, iwren_tmp, irden_tmp, irdaddress_tmp, iwraddress_tmp)
    variable mem_data : alt_memory;
    variable mem_data_word : std_logic_vector(width-1 downto 0);
    variable mem_init : boolean := false;
    variable i, j, k, n, m, lineno : integer := 0;
    variable buf : line;
    variable booval : boolean;
    --FILE unused_file : TEXT IS OUT "UNUSED";
    --FILE mem_data_file : TEXT IS IN lpm_file;
    FILE unused_file : TEXT OPEN WRITE_MODE IS "UNUSED";
    FILE mem_data_file: TEXT OPEN READ_MODE IS lpm_file;
    variable base, byte, rec_type, datain, addr, checksum: string(2 downto 1);
    variable startadd: string(4 downto 1);
    variable ibase : integer := 0;
    variable ibyte : integer := 0;
    variable istartadd : integer := 0;
    variable check_sum_vec, check_sum_vec_tmp : std_logic_vector(7 downto 0);

    --debug
    --variable ln : line;
	begin
		-- INITIALIZE --
		if NOT(mem_init) then
			-- INITIALIZE TO 0 --
			for i in mem_data'LOW to mem_data'HIGH loop
				mem_data(i) := (OTHERS => '0');
			end loop;

            if (lpm_file = "UNUSED") then
				ASSERT FALSE
				REPORT "Initialization file not found!"
				SEVERITY WARNING;
			else
				WHILE NOT ENDFILE(mem_data_file) loop
					booval := true;
					READLINE(mem_data_file, buf);
					lineno := lineno + 1;
					check_sum_vec := (OTHERS => '0');
					if (buf(buf'LOW) = ':') then
						i := 1;
						shrink_line(buf, i);
						READ(L=>buf, VALUE=>byte, good=>booval);
						if not (booval) then
							ASSERT FALSE
							REPORT "[Line "& int_to_str(lineno) & "]:Illegal Intel Hex Format!"
							SEVERITY ERROR;
						end if;
						ibyte := hex_str_to_int(byte);
						check_sum_vec := unsigned(check_sum_vec) + unsigned(CONV_STD_LOGIC_VECTOR(ibyte, 8));
						READ(L=>buf, VALUE=>startadd, good=>booval);
						if not (booval) then
							ASSERT FALSE
							REPORT "[Line "& int_to_str(lineno) & "]:Illegal Intel Hex Format! "
							SEVERITY ERROR;
						end if;
						istartadd := hex_str_to_int(startadd);
						addr(2) := startadd(4);
						addr(1) := startadd(3);
						check_sum_vec := unsigned(check_sum_vec) + unsigned(CONV_STD_LOGIC_VECTOR(hex_str_to_int(addr), 8));
						addr(2) := startadd(2);
						addr(1) := startadd(1);
						check_sum_vec := unsigned(check_sum_vec) + unsigned(CONV_STD_LOGIC_VECTOR(hex_str_to_int(addr), 8));
						READ(L=>buf, VALUE=>rec_type, good=>booval);
						if not (booval) then
							ASSERT FALSE
							REPORT "[Line "& int_to_str(lineno) & "]:Illegal Intel Hex Format! "
							SEVERITY ERROR;
						end if;
						check_sum_vec := unsigned(check_sum_vec) + unsigned(CONV_STD_LOGIC_VECTOR(hex_str_to_int(rec_type), 8));
					else
						ASSERT FALSE
						REPORT "[Line "& int_to_str(lineno) & "]:Illegal Intel Hex Format! "
						SEVERITY ERROR;
					end if;
					case rec_type is
						when "00"=>     -- Data record
							i := 0;
                                                        k := (WIDTH + 7) / 8;  -- # of bytes per entry
							while (i < ibyte) loop
                                                                mem_data_word := (others => '0');
                                                                n := (k - 1)*8;
                                                                m := width - 1;
								for j in 1 to k loop
									READ(L=>buf, VALUE=>datain,good=>booval); -- read in data a byte (2 hex chars) at a time.
									if not (booval) then
										ASSERT FALSE
										REPORT "[Line "& int_to_str(lineno) & "]:Illegal Intel Hex Format! "
										SEVERITY ERROR;
									end if;
									check_sum_vec := unsigned(check_sum_vec) + unsigned(CONV_STD_LOGIC_VECTOR(hex_str_to_int(datain), 8));
                                                                        mem_data_word(m downto n) := CONV_STD_LOGIC_VECTOR(hex_str_to_int(datain), m-n+1);
                                                                        m := n - 1;
                                                                        n := n - 8;
								end loop;
								i := i + k;
                                                                mem_data(ibase + istartadd) := mem_data_word;
								istartadd := istartadd + 1;
							end loop;
						when "01"=>
							exit;
						when "02"=>
							ibase := 0;
							if (ibyte /= 2) then
								ASSERT FALSE
								REPORT "[Line "& int_to_str(lineno) & "]:Illegal Intel Hex Format for record type 02! "
								SEVERITY ERROR;
							end if;
							for i in 0 to (ibyte-1) loop
								READ(L=>buf, VALUE=>base,good=>booval);
								ibase := ibase * 256 + hex_str_to_int(base);
								if not (booval) then
									ASSERT FALSE
									REPORT "[Line "& int_to_str(lineno) & "]:Illegal Intel Hex Format! "
									SEVERITY ERROR;
								end if;
								check_sum_vec := unsigned(check_sum_vec) + unsigned(CONV_STD_LOGIC_VECTOR(hex_str_to_int(base), 8));
							end loop;
							ibase := ibase * 16;
						when OTHERS =>
							ASSERT FALSE
							REPORT "[Line "& int_to_str(lineno) & "]:Illegal record type in Intel Hex File! "
							SEVERITY ERROR;
					end case;
					READ(L=>buf, VALUE=>checksum,good=>booval);
					if not (booval) then
						ASSERT FALSE
						REPORT "[Line "& int_to_str(lineno) & "]:Checksum is missing! "
						SEVERITY ERROR;
					end if;

					check_sum_vec := unsigned(not (check_sum_vec)) + 1 ;
					check_sum_vec_tmp := CONV_STD_LOGIC_VECTOR(hex_str_to_int(checksum),8);

					if (unsigned(check_sum_vec) /= unsigned(check_sum_vec_tmp)) then
						ASSERT FALSE
						REPORT "[Line "& int_to_str(lineno) & "]:Incorrect checksum!"
						SEVERITY ERROR;
					end if;
				end loop;
			end if;
			mem_init := TRUE;

        else -- already initialized

            -- MEMORY FUNCTION --
            if iwren_tmp = '1' then
                mem_data (ieee.std_logic_unsigned.conv_integer(iwraddress_tmp)) := idata_tmp;

            --debug
            --write(ln, (now/1000 ns),right,4);
            --write(ln, string'(" us> write ")); 
            --for i in width-1 downto 0 loop
            --    if (idata_tmp(i) = '0') then write(ln, string'("0"));
            --    elsif (idata_tmp(i) = '1') then write(ln, string'("1"));
            --    else  write(ln, string'("?"));
            --    end if;
            --end loop;
            --write(ln, string'(" @ ")); 
            --for i in widthad-1 downto 0 loop
            --    if (iwraddress_tmp(i) = '0') then write(ln, string'("0"));
            --    elsif (iwraddress_tmp(i) = '1') then write(ln, string'("1"));
            --    else  write(ln, string'("?"));
            --    end if;
            --end loop;
            --writeline(output, ln);
            --end-debug

            end if;

            if irden_tmp = '1' then
                iq_tmp <= mem_data(ieee.std_logic_unsigned.conv_integer(irdaddress_tmp));

                --debug
                --write(ln, (now/1000 ns),right,4);
                --write(ln, string'(" us> read ")); 
                --for i in width-1 downto 0 loop
                --    if (iq_tmp(i) = '0') then write(ln, string'("0"));
                --    elsif (iq_tmp(i) = '1') then write(ln, string'("1"));
                --    else  write(ln, string'("?"));
                --    end if;
                --end loop;
                --write(ln, string'(" @ ")); 
                --for i in widthad-1 downto 0 loop
                --    if (irdaddress_tmp(i) = '0') then write(ln, string'("0"));
                --    elsif (irdaddress_tmp(i) = '1') then write(ln, string'("1"));
                --    else  write(ln, string'("?"));
                --    end if;
                --end loop;
                --writeline(output, ln);
                --end-debug

            elsif rden_low_output_0 then
                iq_tmp <= (OTHERS => '0');
            end if;
        end if; -- if NOT(mem_init)

	end process;

end behavior;




----------------------------------------------------------------------------
-- alt3pram megafunction
----------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use std.textio.all;

entity alt3pram is
    generic
      ( width                  : natural;
        widthad                : natural;
        numwords               : natural := 0;
        lpm_file               : string := "UNUSED";
        lpm_hint               : string := "USE_EAB=ON";
        indata_reg             : string := "UNREGISTERED";
        indata_aclr            : string := "OFF";
        write_reg		: string := "UNREGISTERED";
        write_aclr         	: string := "OFF";
        rdaddress_reg_a          : string := "UNREGISTERED";
        rdaddress_aclr_a         : string := "OFF";
	rdaddress_reg_b          : string := "UNREGISTERED";
        rdaddress_aclr_b         : string := "OFF";
        rdcontrol_reg_a          : string := "UNREGISTERED";
        rdcontrol_aclr_a         : string := "OFF";
	rdcontrol_reg_b          : string := "UNREGISTERED";
        rdcontrol_aclr_b         : string := "OFF";
        outdata_reg_a            : string := "UNREGISTERED";
        outdata_aclr_a           : string := "OFF";
	outdata_reg_b            : string := "UNREGISTERED";
        outdata_aclr_b           : string := "OFF";
        intended_device_family : string := "APEX20KE");
    port
      ( wren       : in std_logic := '0';
        data       : in std_logic_vector(width-1 downto 0);
        wraddress  : in std_logic_vector(widthad-1 downto 0);
        inclock    : in std_logic := '0';
        inclocken  : in std_logic := '1';
        rden_a     : in std_logic := '1';
	rden_b     : in std_logic := '1';
        rdaddress_a	: in std_logic_vector(widthad-1 downto 0);
	rdaddress_b	: in std_logic_vector(widthad-1 downto 0);
        outclock   : in std_logic := '0';
        outclocken : in std_logic := '1';
        aclr       : in std_logic := '0';
	qa        : out std_logic_vector(width-1 downto 0);
        qb        : out std_logic_vector(width-1 downto 0) );

	function int_to_str( value : integer ) return string is
	variable ivalue,index : integer;
	variable digit : integer;
    	variable line_no: string(8 downto 1) := "        ";  
	begin
		ivalue := value;
		index := 1;
		while (ivalue > 0) loop
			digit := ivalue MOD 10;
			ivalue := ivalue/10;
			case digit is
				when 0 =>
					line_no(index) := '0';
				when 1 =>
					line_no(index) := '1';
				when 2 =>
					line_no(index) := '2';
				when 3 =>
					line_no(index) := '3';
				when 4 =>
					line_no(index) := '4';
				when 5 =>
					line_no(index) := '5';
				when 6 =>
					line_no(index) := '6';
				when 7 =>
					line_no(index) := '7';
				when 8 =>
					line_no(index) := '8';
				when 9 =>
					line_no(index) := '9';
				when others =>
					ASSERT FALSE
					REPORT "Illegal number!"
					SEVERITY ERROR;
			end case;
			index := index + 1;
		end loop;
		return line_no;
	end;

	function hex_str_to_int( str : string ) return integer is
	variable len : integer := str'length;
	variable ivalue : integer := 0;
	variable digit : integer;
	begin
		for i in len downto 1 loop
			case str(i) is
				when '0' =>
					digit := 0;
				when '1' =>
					digit := 1;
				when '2' =>
					digit := 2;
				when '3' =>
					digit := 3;
				when '4' =>
					digit := 4;
				when '5' =>
					digit := 5;
				when '6' =>
					digit := 6;
				when '7' =>
					digit := 7;
				when '8' =>
					digit := 8;
				when '9' =>
					digit := 9;
				when 'A' =>
					digit := 10;
				when 'a' =>
					digit := 10;
				when 'B' =>
					digit := 11;
				when 'b' =>
					digit := 11;
				when 'C' =>
					digit := 12;
				when 'c' =>
					digit := 12;
				when 'D' =>
					digit := 13;
				when 'd' =>
					digit := 13;
				when 'E' =>
					digit := 14;
				when 'e' =>
					digit := 14;
				when 'F' =>
					digit := 15;
				when 'f' =>
					digit := 15;
				when others =>
					ASSERT FALSE
					REPORT "Illegal character "&  str(i) & "in Intel Hex File! "
					SEVERITY ERROR;
			end case;
			ivalue := ivalue * 16 + digit;
		end loop;
		return ivalue;
	end;

	procedure Shrink_line(L : inout LINE; pos : in integer) is
	subtype nstring is string(1 to pos);
	variable stmp : nstring;
	begin
		if pos >= 1 then
			read(l, stmp);
		end if;
	end;

end alt3pram;

architecture behavior of alt3pram is

type alt_memory is array((2**WIDTHAD)-1 downto 0) of std_logic_vector(WIDTH-1 downto 0);

signal idata_tmp, idata_reg : std_logic_vector(WIDTH-1 downto 0) := (OTHERS => '0');
signal idata_hi, idata_lo : std_logic_vector(WIDTH-1 downto 0) := (OTHERS => '0');

signal iqa_tmp, iqa_reg : std_logic_vector(WIDTH-1 downto 0) := (OTHERS => '0');
signal iqb_tmp, iqb_reg : std_logic_vector(WIDTH-1 downto 0) := (OTHERS => '0');

signal irdaddress_tmp_a, irdaddress_reg_a : std_logic_vector(WIDTHAD-1 downto 0) := (OTHERS => '0');
signal irdaddress_tmp_b, irdaddress_reg_b : std_logic_vector(WIDTHAD-1 downto 0) := (OTHERS => '0');

signal iwraddress_tmp, iwraddress_reg : std_logic_vector(WIDTHAD-1 downto 0) := (OTHERS => '0');
signal iwraddress_hi, iwraddress_lo : std_logic_vector(WIDTHAD-1 downto 0) := (OTHERS => '0');

signal iwren_tmp, iwren_reg : std_logic := '0';
signal iwren_hi, iwren_lo : std_logic := '0';

signal irden_tmp_a, irden_reg_a : std_logic := '0';
signal irden_tmp_b, irden_reg_b : std_logic := '0';

signal write_at_low_clock : boolean := false;
signal rden_low_output_0 : boolean := false;

begin
    initial: process (inclock, outclock)
    variable init : boolean := false;
    begin
        if (not init) then
            if (lpm_hint = "USE_EAB=ON") then
                if (write_reg = "INCLOCK") then
                    write_at_low_clock <= true;
                end if;
                if (intended_device_family = "APEX20K") then
                    	rden_low_output_0 <= true;
			
                end if;
            end if;
            init := true;
        end if;
    end process;

    sync: process(data, idata_reg, rden_a, rden_b, irden_reg_a, irden_reg_b,
		  rdaddress_a, rdaddress_b, irdaddress_reg_a,irdaddress_reg_b,
                  wren, iwren_reg, wraddress, iwraddress_reg, 
		  iqa_tmp, iqb_tmp, iqa_reg, iqb_reg, aclr)
	begin
        
	if (rdaddress_reg_a = "INCLOCK" or rdaddress_reg_a = "OUTCLOCK") then
            irdaddress_tmp_a <= irdaddress_reg_a;
        elsif (rdaddress_reg_a = "UNREGISTERED") then
            irdaddress_tmp_a <= rdaddress_a;
        else
            ASSERT FALSE
            REPORT "Illegal RDADDRESS_REG_A property value for ALT3PRAM!"
            SEVERITY ERROR;
	end if;
        if (rdcontrol_reg_a = "INCLOCK" or rdcontrol_reg_a = "OUTCLOCK") then
            irden_tmp_a <= irden_reg_a;
        elsif (rdcontrol_reg_a = "UNREGISTERED") then
            irden_tmp_a <= rden_a;
        else
            ASSERT FALSE
            REPORT "Illegal RDCONTROL_REG_A property value for ALT3PRAM!"
            SEVERITY ERROR;
	end if;


	if (rdaddress_reg_b = "INCLOCK" or rdaddress_reg_b = "OUTCLOCK") then
            irdaddress_tmp_b <= irdaddress_reg_b;
        elsif (rdaddress_reg_b = "UNREGISTERED") then
            irdaddress_tmp_b <= rdaddress_b;
        else
            ASSERT FALSE
            REPORT "Illegal RDADDRESS_REG_B property value for ALT3PRAM!"
            SEVERITY ERROR;
	end if;
        if (rdcontrol_reg_b = "INCLOCK" or rdcontrol_reg_b = "OUTCLOCK") then
            irden_tmp_b <= irden_reg_b;
        elsif (rdcontrol_reg_b = "UNREGISTERED") then
            irden_tmp_b <= rden_b;
        else
            ASSERT FALSE
            REPORT "Illegal RDCONTROL_REG_B property value for ALT3PRAM!"
            SEVERITY ERROR;
	end if;

        if (write_reg = "INCLOCK") then
            iwraddress_tmp <= iwraddress_reg;
	    iwren_tmp <= iwren_reg; 
        elsif (write_reg = "UNREGISTERED") then
            iwraddress_tmp <= wraddress;
	    iwren_tmp <= wren;
        else
            ASSERT FALSE
            REPORT "Illegal WRITE_REG property value for ALT3PRAM!"
            SEVERITY ERROR;
	end if;

        if (indata_reg = "INCLOCK") then
            idata_tmp <= idata_reg;
        elsif (indata_reg = "UNREGISTERED") then
            idata_tmp <= data;
        else
            ASSERT FALSE
            REPORT "Illegal INDATA_REG property value for ALT3PRAM!"
            SEVERITY ERROR;
		end if;

        if (outdata_reg_a = "OUTCLOCK") then
            qa <= iqa_reg;
        elsif (outdata_reg_a = "UNREGISTERED") then
            qa <= iqa_tmp;
        else
            ASSERT FALSE
            REPORT "Illegal OUTDATA_REG_A property value for ALT3PRAM!"
            SEVERITY ERROR;
	end if;
	
	if (outdata_reg_b = "OUTCLOCK") then
            qb <= iqb_reg;
        elsif (outdata_reg_b = "UNREGISTERED") then
            qb <= iqb_tmp;
        else
            ASSERT FALSE
            REPORT "Illegal OUTDATA_REG_B property value for ALT3PRAM!"
            SEVERITY ERROR;
	end if;

        if (aclr = '1') then
            if(indata_aclr = "ON") then
                idata_tmp <= (OTHERS => '0');
            end if;
            if(write_aclr = "ON") then
                iwraddress_tmp <= (OTHERS => '0');
            	iwren_tmp <= '0';
	    				end if;
            if(rdaddress_aclr_a = "ON") then
                irdaddress_tmp_a <= (OTHERS => '0');
            end if;
            if(rdcontrol_aclr_a = "ON") then
                irden_tmp_a <= '0';
            end if;
	    if(rdaddress_aclr_b = "ON") then
                irdaddress_tmp_b <= (OTHERS => '0');
            end if;
            if(rdcontrol_aclr_b = "ON") then
                irden_tmp_b <= '0';
            end if;
            if(outdata_aclr_a = "ON") then
                qa <= (OTHERS => '0');
					end if;
	    if(outdata_aclr_b = "ON") then
                qb <= (OTHERS => '0');
            end if;
        end if;
	end process;

    sync2: process(idata_hi, idata_lo, iwraddress_hi, iwraddress_lo,
                   iwren_hi, iwren_lo)
    begin
        if (write_at_low_clock) then
            idata_reg <= idata_lo;
            iwren_reg <= iwren_lo;
            iwraddress_reg <= iwraddress_lo;
        else
            idata_reg <= idata_hi;
            iwren_reg <= iwren_hi;
            iwraddress_reg <= iwraddress_hi;
        end if;
    end process;

    registers: process (inclock, outclock)
    begin

        -- WRITE REGS --
        if (aclr = '1' and indata_aclr = "ON") then
            idata_hi <= (OTHERS => '0');
            idata_lo <= (OTHERS => '0');
        elsif inclock'event and inclock = '1' and inclocken = '1' then
            idata_hi <= data;
        elsif inclock'event and inclock = '0' then
            idata_lo <= idata_hi;
        end if;
        if (aclr = '1' and write_aclr = "ON") then
            iwraddress_hi <= (OTHERS => '0');
            iwraddress_lo <= (OTHERS => '0');
	    iwren_hi <= '0';
            iwren_lo <= '0';
        elsif inclock'event and inclock = '1' and inclocken = '1' then
            iwraddress_hi <= wraddress;
	    iwren_hi <= wren;
        elsif inclock'event and inclock = '0' then
            iwraddress_lo <= iwraddress_hi;
	    iwren_lo <= iwren_hi;
        end if;

        -- READ REGS PORT A--
        if (aclr = '1' and outdata_aclr_a = "ON") then
            iqa_reg <= (OTHERS => '0');
        elsif outclock'event and outclock = '1' and outclocken = '1' then
            iqa_reg <= iqa_tmp;
        end if;
	
        if (rdaddress_reg_a = "INCLOCK") then
            if (aclr = '1' and rdaddress_aclr_a = "ON") then
                irdaddress_reg_a <= (OTHERS => '0');
            elsif inclock'event and inclock = '1' and inclocken = '1' then
                irdaddress_reg_a <= rdaddress_a;
            end if;
        end if;

        if (rdcontrol_reg_a = "INCLOCK") then
            if (aclr = '1' and rdcontrol_aclr_a = "ON") then
                irden_reg_a <= '0';
            elsif inclock'event and inclock = '1' and inclocken = '1' then
                irden_reg_a <= rden_a; 
            end if;
        end if;

        if (rdaddress_reg_a = "OUTCLOCK") then
            if (aclr = '1' and rdaddress_aclr_a = "ON") then
                irdaddress_reg_a <= (OTHERS => '0');
            elsif outclock'event and outclock = '1' and outclocken = '1' then
                irdaddress_reg_a <= rdaddress_a;
            end if;
        end if;

        if (rdcontrol_reg_a = "OUTCLOCK") then
            if (aclr = '1' and rdcontrol_aclr_a = "ON") then
                irden_reg_a <= '0';
            elsif outclock'event and outclock = '1' and outclocken = '1' then
                irden_reg_a <= rden_a; 
            end if;
        end if;

	-- READ REGS PORT B--
		if (aclr = '1' and (outdata_aclr_b = "ON")) then
            iqb_reg <= (OTHERS => '0');
        elsif (outclock'event and outclock = '1' and outclocken = '1') then
            iqb_reg <= iqb_tmp;
        end if;

			if (rdaddress_reg_b = "INCLOCK") then
            if (aclr = '1' and rdaddress_aclr_b = "ON") then
                irdaddress_reg_b <= (OTHERS => '0');
            elsif inclock'event and inclock = '1' and inclocken = '1' then
                irdaddress_reg_b <= rdaddress_b;
            end if;
        end if;

        if (rdcontrol_reg_b = "INCLOCK") then
            if (aclr = '1' and rdcontrol_aclr_b = "ON") then
                irden_reg_b <= '0';
            elsif inclock'event and inclock = '1' and inclocken = '1' then
                irden_reg_b <= rden_b; 
            end if;
        end if;

        if (rdaddress_reg_b = "OUTCLOCK") then
            if (aclr = '1' and rdaddress_aclr_b = "ON") then
                irdaddress_reg_b <= (OTHERS => '0');
            elsif outclock'event and outclock = '1' and outclocken = '1' then
                irdaddress_reg_b <= rdaddress_b;
            end if;
        end if;

        if (rdcontrol_reg_b = "OUTCLOCK") then
            if (aclr = '1' and rdcontrol_aclr_b = "ON") then
                irden_reg_b <= '0';
            elsif outclock'event and outclock = '1' and outclocken = '1' then
                irden_reg_b <= rden_b; 
            end if;
        end if;
    end process;

    memory: process(idata_tmp, iwren_tmp, irden_tmp_a, irden_tmp_b, irdaddress_tmp_a, irdaddress_tmp_b, iwraddress_tmp)
    variable mem_data : alt_memory;
    variable mem_data_word : std_logic_vector(width-1 downto 0);
    variable mem_init : boolean := false;
    variable i, j, k, n, m, lineno : integer := 0;
    variable buf : line;
    variable booval : boolean;
    --FILE unused_file : TEXT IS OUT "UNUSED";
    --FILE mem_data_file : TEXT IS IN lpm_file;
    FILE unused_file : TEXT OPEN WRITE_MODE IS "UNUSED";
    FILE mem_data_file: TEXT OPEN READ_MODE IS lpm_file;
    variable base, byte, rec_type, datain, addr, checksum: string(2 downto 1);
    variable startadd: string(4 downto 1);
    variable ibase : integer := 0;
    variable ibyte : integer := 0;
    variable istartadd : integer := 0;
    variable check_sum_vec, check_sum_vec_tmp : std_logic_vector(7 downto 0);

    --debug
    --variable ln : line;
	begin
		-- INITIALIZE --
		if NOT(mem_init) then
			-- INITIALIZE TO 0 --
			for i in mem_data'LOW to mem_data'HIGH loop
				mem_data(i) := (OTHERS => '0');
			end loop;

            if (lpm_file = "UNUSED") then
				ASSERT FALSE
				REPORT "Initialization file not found!"
				SEVERITY WARNING;
			else
				WHILE NOT ENDFILE(mem_data_file) loop
					booval := true;
					READLINE(mem_data_file, buf);
					lineno := lineno + 1;
					check_sum_vec := (OTHERS => '0');
					if (buf(buf'LOW) = ':') then
						i := 1;
						shrink_line(buf, i);
						READ(L=>buf, VALUE=>byte, good=>booval);
						if not (booval) then
							ASSERT FALSE
							REPORT "[Line "& int_to_str(lineno) & "]:Illegal Intel Hex Format!"
							SEVERITY ERROR;
						end if;
						ibyte := hex_str_to_int(byte);
						check_sum_vec := unsigned(check_sum_vec) + unsigned(CONV_STD_LOGIC_VECTOR(ibyte, 8));
						READ(L=>buf, VALUE=>startadd, good=>booval);
						if not (booval) then
							ASSERT FALSE
							REPORT "[Line "& int_to_str(lineno) & "]:Illegal Intel Hex Format! "
							SEVERITY ERROR;
						end if;
						istartadd := hex_str_to_int(startadd);
						addr(2) := startadd(4);
						addr(1) := startadd(3);
						check_sum_vec := unsigned(check_sum_vec) + unsigned(CONV_STD_LOGIC_VECTOR(hex_str_to_int(addr), 8));
						addr(2) := startadd(2);
						addr(1) := startadd(1);
						check_sum_vec := unsigned(check_sum_vec) + unsigned(CONV_STD_LOGIC_VECTOR(hex_str_to_int(addr), 8));
						READ(L=>buf, VALUE=>rec_type, good=>booval);
						if not (booval) then
							ASSERT FALSE
							REPORT "[Line "& int_to_str(lineno) & "]:Illegal Intel Hex Format! "
							SEVERITY ERROR;
						end if;
						check_sum_vec := unsigned(check_sum_vec) + unsigned(CONV_STD_LOGIC_VECTOR(hex_str_to_int(rec_type), 8));
					else
						ASSERT FALSE
						REPORT "[Line "& int_to_str(lineno) & "]:Illegal Intel Hex Format! "
						SEVERITY ERROR;
					end if;
					case rec_type is
						when "00"=>     -- Data record
							i := 0;
                                                        k := (WIDTH + 7) / 8;  -- # of bytes per entry
							while (i < ibyte) loop
                                                                mem_data_word := (others => '0');
                                                                n := (k - 1)*8;
                                                                m := width - 1;
								for j in 1 to k loop
									READ(L=>buf, VALUE=>datain,good=>booval); -- read in data a byte (2 hex chars) at a time.
									if not (booval) then
										ASSERT FALSE
										REPORT "[Line "& int_to_str(lineno) & "]:Illegal Intel Hex Format! "
										SEVERITY ERROR;
									end if;
									check_sum_vec := unsigned(check_sum_vec) + unsigned(CONV_STD_LOGIC_VECTOR(hex_str_to_int(datain), 8));
                                                                        mem_data_word(m downto n) := CONV_STD_LOGIC_VECTOR(hex_str_to_int(datain), m-n+1);
                                                                        m := n - 1;
                                                                        n := n - 8;
								end loop;
								i := i + k;
                                                                mem_data(ibase + istartadd) := mem_data_word;
								istartadd := istartadd + 1;
							end loop;
						when "01"=>
							exit;
						when "02"=>
							ibase := 0;
							if (ibyte /= 2) then
								ASSERT FALSE
								REPORT "[Line "& int_to_str(lineno) & "]:Illegal Intel Hex Format for record type 02! "
								SEVERITY ERROR;
							end if;
							for i in 0 to (ibyte-1) loop
								READ(L=>buf, VALUE=>base,good=>booval);
								ibase := ibase * 256 + hex_str_to_int(base);
								if not (booval) then
									ASSERT FALSE
									REPORT "[Line "& int_to_str(lineno) & "]:Illegal Intel Hex Format! "
									SEVERITY ERROR;
								end if;
								check_sum_vec := unsigned(check_sum_vec) + unsigned(CONV_STD_LOGIC_VECTOR(hex_str_to_int(base), 8));
							end loop;
							ibase := ibase * 16;
						when OTHERS =>
							ASSERT FALSE
							REPORT "[Line "& int_to_str(lineno) & "]:Illegal record type in Intel Hex File! "
							SEVERITY ERROR;
					end case;
					READ(L=>buf, VALUE=>checksum,good=>booval);
					if not (booval) then
						ASSERT FALSE
						REPORT "[Line "& int_to_str(lineno) & "]:Checksum is missing! "
						SEVERITY ERROR;
					end if;

					check_sum_vec := unsigned(not (check_sum_vec)) + 1 ;
					check_sum_vec_tmp := CONV_STD_LOGIC_VECTOR(hex_str_to_int(checksum),8);

					if (unsigned(check_sum_vec) /= unsigned(check_sum_vec_tmp)) then
						ASSERT FALSE
						REPORT "[Line "& int_to_str(lineno) & "]:Incorrect checksum!"
						SEVERITY ERROR;
					end if;
				end loop;
			end if;
			mem_init := TRUE;

        else -- already initialized

            -- MEMORY FUNCTION --
            if iwren_tmp = '1' then
                mem_data (ieee.std_logic_unsigned.conv_integer(iwraddress_tmp)) := idata_tmp;

            --debug
            --write(ln, (now/1000 ns),right,4);
            --write(ln, string'(" us> write ")); 
            --for i in width-1 downto 0 loop
            --    if (idata_tmp(i) = '0') then write(ln, string'("0"));
            --    elsif (idata_tmp(i) = '1') then write(ln, string'("1"));
            --    else  write(ln, string'("?"));
            --    end if;
            --end loop;
            --write(ln, string'(" @ ")); 
            --for i in widthad-1 downto 0 loop
            --    if (iwraddress_tmp(i) = '0') then write(ln, string'("0"));
            --    elsif (iwraddress_tmp(i) = '1') then write(ln, string'("1"));
            --    else  write(ln, string'("?"));
            --    end if;
            --end loop;
            --writeline(output, ln);
            --end-debug

            end if;

            if irden_tmp_a = '1' then
                iqa_tmp <= mem_data(ieee.std_logic_unsigned.conv_integer(irdaddress_tmp_a));

                --debug
                --write(ln, (now/1000 ns),right,4);
                --write(ln, string'(" us> read ")); 
                --for i in width-1 downto 0 loop
                --    if (iq_tmp(i) = '0') then write(ln, string'("0"));
                --    elsif (iq_tmp(i) = '1') then write(ln, string'("1"));
                --    else  write(ln, string'("?"));
                --    end if;
                --end loop;
                --write(ln, string'(" @ ")); 
                --for i in widthad-1 downto 0 loop
                --    if (irdaddress_tmp(i) = '0') then write(ln, string'("0"));
                --    elsif (irdaddress_tmp(i) = '1') then write(ln, string'("1"));
                --    else  write(ln, string'("?"));
                --    end if;
                --end loop;
                --writeline(output, ln);
                --end-debug

            elsif rden_low_output_0 then
                iqa_tmp <= (OTHERS => '0');
            end if;

	
	    if irden_tmp_b = '1' then
                iqb_tmp <= mem_data(ieee.std_logic_unsigned.conv_integer(irdaddress_tmp_b));

                --debug
                --write(ln, (now/1000 ns),right,4);
                --write(ln, string'(" us> read ")); 
                --for i in width-1 downto 0 loop
                --    if (iq_tmp(i) = '0') then write(ln, string'("0"));
                --    elsif (iq_tmp(i) = '1') then write(ln, string'("1"));
                --    else  write(ln, string'("?"));
                --    end if;
                --end loop;
                --write(ln, string'(" @ ")); 
                --for i in widthad-1 downto 0 loop
                --    if (irdaddress_tmp(i) = '0') then write(ln, string'("0"));
                --    elsif (irdaddress_tmp(i) = '1') then write(ln, string'("1"));
                --    else  write(ln, string'("?"));
                --    end if;
                --end loop;
                --writeline(output, ln);
                --end-debug

            elsif rden_low_output_0 then
                iqb_tmp <= (OTHERS => '0');
            end if;

        end if; -- if NOT(mem_init)

	end process;

end behavior;
--end of alt3pram
----------------------------------------------------------------------------
-- altqpram megafunction
----------------------------------------------------------------------------
    
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use std.textio.all;

entity altqpram is
    generic
      ( operation_mode            : string := "QUAD_PORT";

        width_write_a             : natural := 1;
        widthad_write_a           : natural := 1;
        numwords_write_a          : natural := 0;  -- default = 2^widthad_write_a
        indata_reg_a              : string := "INCLOCK_A";
        indata_aclr_a             : string := "INACLR_A";
        wrcontrol_wraddress_reg_a : string := "INCLOCK_A";
        wrcontrol_aclr_a          : string := "INACLR_A";
        wraddress_aclr_a          : string := "INACLR_A";

        width_write_b             : natural := 1;  -- default = width_write_a
        widthad_write_b           : natural := 1;  -- default = widthad_write_a
        numwords_write_b          : natural := 0;  -- default = 2^widthad_write_b
        indata_reg_b              : string := "INCLOCK_B";
        indata_aclr_b             : string := "INACLR_B";
        wrcontrol_wraddress_reg_b : string := "INCLOCK_B";
        wrcontrol_aclr_b          : string := "INACLR_B";
        wraddress_aclr_b          : string := "INACLR_B";

        width_read_a              : natural := 1;
        widthad_read_a            : natural := 1;
        numwords_read_a           : natural := 0;  -- default = 2^widthad_read_a
        rdcontrol_reg_a           : string := "OUTCLOCK_A";
        rdcontrol_aclr_a          : string := "OUTACLR_A";
        rdaddress_reg_a           : string := "OUTCLOCK_A";
        rdaddress_aclr_a          : string := "OUTACLR_A";
        outdata_reg_a             : string := "UNREGISTERED";
        outdata_aclr_a            : string := "OUTACLR_A";

        width_read_b              : natural := 1;  -- default = width_read_a
        widthad_read_b            : natural := 1;  -- default = widthad_read_a
        numwords_read_b           : natural := 0;  -- default = 2^widthad_read_b
        rdcontrol_reg_b           : string := "OUTCLOCK_B";
        rdcontrol_aclr_b          : string := "OUTACLR_B";
        rdaddress_reg_b           : string := "OUTCLOCK_B";
        rdaddress_aclr_b          : string := "OUTACLR_B";
        outdata_reg_b             : string := "UNREGISTERED";
        outdata_aclr_b            : string := "OUTACLR_B";

        init_file                 : string := "UNUSED";
        lpm_hint                  : string := "UNUSED" );

    port
      ( wren_a       : in std_logic := '0';
        wren_b       : in std_logic := '0';
        data_a       : in std_logic_vector(width_write_a-1 downto 0) := (OTHERS => '0');
        data_b       : in std_logic_vector(width_write_b-1 downto 0) := (OTHERS => '0');
        wraddress_a  : in std_logic_vector(widthad_write_a-1 downto 0) := (OTHERS => '0');
        wraddress_b  : in std_logic_vector(widthad_write_b-1 downto 0) := (OTHERS => '0');
        inclock_a    : in std_logic := '0';
        inclock_b    : in std_logic := '0';
        inclocken_a  : in std_logic := '1';
        inclocken_b  : in std_logic := '1';
        rden_a       : in std_logic := '1';
        rden_b       : in std_logic := '1';
        rdaddress_a  : in std_logic_vector(widthad_read_a-1 downto 0) := (OTHERS => '0');
        rdaddress_b  : in std_logic_vector(widthad_read_b-1 downto 0) := (OTHERS => '0');
        outclock_a   : in std_logic := '0';
        outclock_b   : in std_logic := '0';
        outclocken_a : in std_logic := '1';
        outclocken_b : in std_logic := '1';
        inaclr_a     : in std_logic := '0';
        inaclr_b     : in std_logic := '0';
        outaclr_a    : in std_logic := '0';
        outaclr_b    : in std_logic := '0';
        q_a          : out std_logic_vector(width_read_a-1 downto 0);
        q_b          : out std_logic_vector(width_read_b-1 downto 0) );

	function int_to_str( value : integer ) return string is
	variable ivalue,index : integer;
	variable digit : integer;
	variable line_no: string(8 downto 1) := "        ";  
	begin
		ivalue := value;
		index := 1;
		while (ivalue > 0) loop
			digit := ivalue MOD 10;
			ivalue := ivalue/10;
			case digit is
				when 0 =>
					line_no(index) := '0';
				when 1 =>
					line_no(index) := '1';
				when 2 =>
					line_no(index) := '2';
				when 3 =>
					line_no(index) := '3';
				when 4 =>
					line_no(index) := '4';
				when 5 =>
					line_no(index) := '5';
				when 6 =>
					line_no(index) := '6';
				when 7 =>
					line_no(index) := '7';
				when 8 =>
					line_no(index) := '8';
				when 9 =>
					line_no(index) := '9';
				when others =>
					ASSERT FALSE
					REPORT "Illegal number!"
					SEVERITY ERROR;
			end case;
			index := index + 1;
		end loop;
		return line_no;
	end;

	function hex_str_to_int( str : string ) return integer is
	variable len : integer := str'length;
	variable ivalue : integer := 0;
	variable digit : integer;
	begin
		for i in len downto 1 loop
			case str(i) is
				when '0' =>
					digit := 0;
				when '1' =>
					digit := 1;
				when '2' =>
					digit := 2;
				when '3' =>
					digit := 3;
				when '4' =>
					digit := 4;
				when '5' =>
					digit := 5;
				when '6' =>
					digit := 6;
				when '7' =>
					digit := 7;
				when '8' =>
					digit := 8;
				when '9' =>
					digit := 9;
				when 'A' =>
					digit := 10;
				when 'a' =>
					digit := 10;
				when 'B' =>
					digit := 11;
				when 'b' =>
					digit := 11;
				when 'C' =>
					digit := 12;
				when 'c' =>
					digit := 12;
				when 'D' =>
					digit := 13;
				when 'd' =>
					digit := 13;
				when 'E' =>
					digit := 14;
				when 'e' =>
					digit := 14;
				when 'F' =>
					digit := 15;
				when 'f' =>
					digit := 15;
				when others =>
					ASSERT FALSE
					REPORT "Illegal character "&  str(i) & "in Intel Hex File! "
					SEVERITY ERROR;
			end case;
			ivalue := ivalue * 16 + digit;
		end loop;
		return ivalue;
	end;

	procedure Shrink_line(L : inout LINE; pos : in integer) is
	subtype nstring is string(1 to pos);
	variable stmp : nstring;
	begin
		if pos >= 1 then
			read(l, stmp);
		end if;
    end;

end altqpram;

architecture behavior of altqpram is

type alt_memory is array((2**WIDTHAD_READ_A)-1 downto 0) of std_logic_vector(WIDTH_READ_A-1 downto 0);
type alt_memory_w is array((2**WIDTHAD_WRITE_A)-1 downto 0) of std_logic_vector(WIDTH_WRITE_A-1 downto 0);

signal idata_tmp_a, idata_reg_a : std_logic_vector(WIDTH_WRITE_A-1 downto 0) := (OTHERS => '0');
signal idata_tmp_b, idata_reg_b : std_logic_vector(WIDTH_WRITE_B-1 downto 0) := (OTHERS => '0');
signal idata_hi_a, idata_lo_a : std_logic_vector(WIDTH_WRITE_A-1 downto 0) := (OTHERS => '0');
signal idata_hi_b, idata_lo_b : std_logic_vector(WIDTH_WRITE_B-1 downto 0) := (OTHERS => '0');
signal iq_tmp_a, iq_reg_a : std_logic_vector(WIDTH_READ_A-1 downto 0) := (OTHERS => '0');
signal iq_tmp_b, iq_reg_b : std_logic_vector(WIDTH_READ_B-1 downto 0) := (OTHERS => '0');
signal irdaddress_tmp_a, irdaddress_reg_a : std_logic_vector(WIDTHAD_READ_A-1 downto 0) := (OTHERS => '0');
signal irdaddress_tmp_b, irdaddress_reg_b : std_logic_vector(WIDTHAD_READ_B-1 downto 0) := (OTHERS => '0');
signal iwraddress_tmp_a, iwraddress_reg_a : std_logic_vector(WIDTHAD_WRITE_A-1 downto 0) := (OTHERS => '0');
signal iwraddress_tmp_b, iwraddress_reg_b : std_logic_vector(WIDTHAD_WRITE_B-1 downto 0) := (OTHERS => '0');
signal iwraddress_hi_a, iwraddress_lo_a : std_logic_vector(WIDTHAD_WRITE_A-1 downto 0) := (OTHERS => '0');
signal iwraddress_hi_b, iwraddress_lo_b : std_logic_vector(WIDTHAD_WRITE_B-1 downto 0) := (OTHERS => '0');
signal iwren_tmp_a, iwren_reg_a : std_logic := '0';
signal iwren_tmp_b, iwren_reg_b : std_logic := '0';
signal iwren_hi_a, iwren_lo_a : std_logic := '0';
signal iwren_hi_b, iwren_lo_b : std_logic := '0';
signal irden_tmp_a, irden_reg_a : std_logic := '0';
signal irden_tmp_b, irden_reg_b : std_logic := '0';
signal i_indata_aclr_a, i_outdata_aclr_a : std_logic := '0';
signal i_wrcontrol_aclr_a, i_wraddress_aclr_a : std_logic := '0';
signal i_rdcontrol_aclr_a, i_rdaddress_aclr_a : std_logic := '0';
signal i_indata_aclr_b, i_outdata_aclr_b : std_logic := '0';
signal i_wrcontrol_aclr_b, i_wraddress_aclr_b : std_logic := '0';
signal i_rdcontrol_aclr_b, i_rdaddress_aclr_b : std_logic := '0';
signal inumwords_write_a : natural := numwords_write_a;
signal inumwords_write_b : natural := numwords_write_b;
signal inumwords_read_a : natural := numwords_read_a;
signal inumwords_read_b : natural := numwords_read_b;
signal write_at_low_clock_a, write_at_low_clock_b : boolean := false;
signal om : natural := 0;   -- operation mode
signal mem_init : boolean := false;
signal aclr_updated : std_logic := '0';

begin

    -----------
    -- ACLRs --
    -----------

    aclrs: process(inaclr_a, outaclr_a, inaclr_b, outaclr_b)
    begin

        -- ACLR_A --

        i_indata_aclr_a <= '0';
        i_wrcontrol_aclr_a <= '0';
        i_wraddress_aclr_a <= '0';
        i_outdata_aclr_a <= '0';
        i_rdcontrol_aclr_a <= '0';
        i_rdaddress_aclr_a <= '0';
    
        if (inaclr_a = '1') then
            if (indata_aclr_a = "INACLR_A" and
                indata_reg_a /= "UNREGISTERED")
            then
                i_indata_aclr_a <= '1';
            end if;
            if (wrcontrol_aclr_a = "INACLR_A" and
                wrcontrol_wraddress_reg_a /= "UNREGISTERED")
            then
                i_wrcontrol_aclr_a <= '1';
            end if;
            if (wraddress_aclr_a = "INACLR_A" and
                wrcontrol_wraddress_reg_a /= "UNREGISTERED")
            then
                i_wraddress_aclr_a <= '1';
            end if;
            if (outdata_aclr_a = "INACLR_A" and
                outdata_reg_a /= "UNREGISTERED")
            then
                i_outdata_aclr_a <= '1';
            end if;
            if (rdcontrol_aclr_a = "INACLR_A" and
                rdcontrol_reg_a /= "UNREGISTERED")
            then
                i_rdcontrol_aclr_a <= '1';
            end if;
            if (rdaddress_aclr_a = "INACLR_A" and
                rdaddress_reg_a /= "UNREGISTERED")
            then
                i_rdaddress_aclr_a <= '1';
            end if;
        end if;

        if (outaclr_a = '1') then
            if (outdata_aclr_a = "OUTACLR_A" and
                outdata_reg_a /= "UNREGISTERED")
            then
                i_outdata_aclr_a <= '1';
            end if;
            if (rdcontrol_aclr_a = "OUTACLR_A" and
                rdcontrol_reg_a /= "UNREGISTERED")
            then
                i_rdcontrol_aclr_a <= '1';
            end if;
            if (rdaddress_aclr_a = "OUTACLR_A" and
                rdaddress_reg_a /= "UNREGISTERED")
            then
                i_rdaddress_aclr_a <= '1';
            end if;
        end if;

        -- ACLR_B --

        i_indata_aclr_b <= '0';
        i_wrcontrol_aclr_b <= '0';
        i_wraddress_aclr_b <= '0';
        i_outdata_aclr_b <= '0';
        i_rdcontrol_aclr_b <= '0';
        i_rdaddress_aclr_b <= '0';
    
        if (inaclr_b = '1') then
            if (indata_aclr_b = "INACLR_B" and
                indata_reg_b /= "UNREGISTERED")
            then
                i_indata_aclr_b <= '1';
            end if;
            if (wrcontrol_aclr_b = "INACLR_B" and
                wrcontrol_wraddress_reg_b /= "UNREGISTERED")
            then
                i_wrcontrol_aclr_b <= '1';
            end if;
            if (wraddress_aclr_b = "INACLR_B" and
                wrcontrol_wraddress_reg_b /= "UNREGISTERED")
            then
                i_wraddress_aclr_b <= '1';
            end if;
            if (outdata_aclr_b = "INACLR_B" and
                outdata_reg_b /= "UNREGISTERED")
            then
                i_outdata_aclr_b <= '1';
            end if;
            if (rdcontrol_aclr_b = "INACLR_B" and
                rdcontrol_reg_b /= "UNREGISTERED")
            then
                i_rdcontrol_aclr_b <= '1';
            end if;
            if (rdaddress_aclr_b = "INACLR_B" and
                rdaddress_reg_b /= "UNREGISTERED")
            then
                i_rdaddress_aclr_b <= '1';
            end if;
        end if;

        if (outaclr_b = '1') then
            if (outdata_aclr_b = "OUTACLR_B" and
                outdata_reg_b /= "UNREGISTERED")
            then
                i_outdata_aclr_b <= '1';
            end if;
            if (rdcontrol_aclr_b = "OUTACLR_B" and
                rdcontrol_reg_b /= "UNREGISTERED")
            then
                i_rdcontrol_aclr_b <= '1';
            end if;
            if (rdaddress_aclr_b = "OUTACLR_B" and
                rdaddress_reg_b /= "UNREGISTERED")
            then
                i_rdaddress_aclr_b <= '1';
            end if;
        end if;
        aclr_updated <= not aclr_updated;
    end process;


    ----------
    -- SYNC --
    ----------

    sync: process(data_a, data_b, idata_reg_a, idata_reg_b,
                  rden_a, rden_b, irden_reg_a, irden_reg_b,
                  rdaddress_a, rdaddress_b, irdaddress_reg_a, irdaddress_reg_b,
                  wren_a, wren_b, iwren_reg_a, iwren_reg_b,
                  wraddress_a, wraddress_b, iwraddress_reg_a, iwraddress_reg_b,
                  iq_tmp_a, iq_tmp_b, iq_reg_a, iq_reg_b,
                  aclr_updated)
	begin
        if (not inaclr_a'event and not outaclr_a'event and
            not inaclr_b'event and not outaclr_b'event)
        then
            if (i_rdaddress_aclr_a = '1') then
                irdaddress_tmp_a <= (OTHERS => '0');
            elsif (rdaddress_reg_a = "UNREGISTERED") then
                irdaddress_tmp_a <= rdaddress_a;
            else
                irdaddress_tmp_a <= irdaddress_reg_a;
            end if;
    
            if (i_rdaddress_aclr_b = '1') then
                irdaddress_tmp_b <= (OTHERS => '0');
            elsif (rdaddress_reg_b = "UNREGISTERED") then
                irdaddress_tmp_b <= rdaddress_b;
            else
                irdaddress_tmp_b <= irdaddress_reg_b;
            end if;
    
            if (i_rdcontrol_aclr_a = '1') then
                irden_tmp_a <= '0';
            elsif (rdcontrol_reg_a = "UNREGISTERED") then
                irden_tmp_a <= rden_a;
            else
                irden_tmp_a <= irden_reg_a;
            end if;
    
            if (i_rdcontrol_aclr_b = '1') then
                irden_tmp_b <= '0';
            elsif (rdcontrol_reg_b = "UNREGISTERED") then
                irden_tmp_b <= rden_b;
            else
                irden_tmp_b <= irden_reg_b;
            end if;
    
            if (i_wraddress_aclr_a = '1') then
                iwraddress_tmp_a <= (OTHERS => '0');
            elsif (wrcontrol_wraddress_reg_a = "UNREGISTERED") then
                iwraddress_tmp_a <= wraddress_a;
            else
                iwraddress_tmp_a <= iwraddress_reg_a;
            end if;
    
            if (i_wraddress_aclr_b = '1') then
                iwraddress_tmp_b <= (OTHERS => '0');
            elsif (wrcontrol_wraddress_reg_b = "UNREGISTERED") then
                iwraddress_tmp_b <= wraddress_b;
            else
                iwraddress_tmp_b <= iwraddress_reg_b;
            end if;
    
            if (i_wrcontrol_aclr_a = '1') then
                iwren_tmp_a <= '0';
            elsif (wrcontrol_wraddress_reg_a = "UNREGISTERED") then
                iwren_tmp_a <= wren_a;
            else
                iwren_tmp_a <= iwren_reg_a; 
            end if;
    
            if (i_wrcontrol_aclr_b = '1') then
                iwren_tmp_b <= '0';
            elsif (wrcontrol_wraddress_reg_b = "UNREGISTERED") then
                iwren_tmp_b <= wren_b;
            else
                iwren_tmp_b <= iwren_reg_b; 
            end if;
    
            if (i_indata_aclr_a = '1') then
                idata_tmp_a <= (OTHERS => '0');
            elsif (indata_reg_a = "UNREGISTERED") then
                idata_tmp_a <= data_a;
            else
                idata_tmp_a <= idata_reg_a;
            end if;
    
            if (i_indata_aclr_b = '1') then
                idata_tmp_b <= (OTHERS => '0');
            elsif (indata_reg_b = "UNREGISTERED") then
                idata_tmp_b <= data_b;
            else
                idata_tmp_b <= idata_reg_b;
            end if;
    
            if (om mod 6 = 0 or i_outdata_aclr_a = '1') then
                q_a <= (OTHERS => '0');
            elsif (outdata_reg_a = "UNREGISTERED") then
                q_a <= iq_tmp_a;
            else
                q_a <= iq_reg_a;
            end if;
    
            if (om mod 5 = 0 or i_outdata_aclr_b = '1') then
                q_b <= (OTHERS => '0');
            elsif (outdata_reg_b = "UNREGISTERED") then
                q_b <= iq_tmp_b;
            else
                q_b <= iq_reg_b;
            end if;
        end if;
	end process;

    sync_a: process(idata_hi_a, idata_lo_a, iwraddress_hi_a, iwraddress_lo_a,
                    iwren_hi_a, iwren_lo_a,
                    i_wraddress_aclr_a, i_wrcontrol_aclr_a, i_indata_aclr_a)
    begin
        if (i_indata_aclr_a = '1') then
            idata_reg_a <= (OTHERS => '0');
        elsif (write_at_low_clock_a) then
            idata_reg_a <= idata_lo_a;
        else
            idata_reg_a <= idata_hi_a;
        end if;

        if (i_wrcontrol_aclr_a = '1') then
            iwren_reg_a <= '0';
        elsif (write_at_low_clock_a) then
            iwren_reg_a <= iwren_lo_a;
        else
            iwren_reg_a <= iwren_hi_a;
        end if;

        if (i_wraddress_aclr_a = '1') then
            iwraddress_reg_a <= (OTHERS => '0');
        elsif (write_at_low_clock_a) then
            if operation_mode = "QUAD_PORT" or  operation_mode = "DUAL_PORT" or operation_mode = "BIDIR_DUAL_PORT" then
                iwraddress_reg_a <= iwraddress_lo_a;
            else
                iwraddress_reg_a <= iwraddress_hi_a;
            end if;
        else
            iwraddress_reg_a <= iwraddress_hi_a;
        end if;
    end process;

    sync_b: process(idata_hi_b, idata_lo_b, iwraddress_hi_b, iwraddress_lo_b,
                    iwren_hi_b, iwren_lo_b,
                    i_wraddress_aclr_b, i_wrcontrol_aclr_b, i_indata_aclr_b)
    begin
        if (i_indata_aclr_b = '1') then
            idata_reg_b <= (OTHERS => '0');
        elsif (write_at_low_clock_b) then
            idata_reg_b <= idata_lo_b;
        else
            idata_reg_b <= idata_hi_b;
        end if;

        if (i_wrcontrol_aclr_b = '1') then
            iwren_reg_b <= '0';
        elsif (write_at_low_clock_b) then
            iwren_reg_b <= iwren_lo_b;
        else
            iwren_reg_b <= iwren_hi_b;
        end if;

        if (i_wraddress_aclr_b = '1') then
            iwraddress_reg_b <= (OTHERS => '0');
        elsif (write_at_low_clock_b) then
            if operation_mode = "QUAD_PORT" or  operation_mode = "DUAL_PORT" or operation_mode = "BIDIR_DUAL_PORT" then
                iwraddress_reg_b <= iwraddress_lo_b;
            else
                iwraddress_reg_b <= iwraddress_hi_b;
            end if;
        else
            iwraddress_reg_b <= iwraddress_hi_b;
        end if;
    end process;


    ------------
    -- Clocks --
    ------------

    input_reg_a: process (inclock_a, outclock_a)
	begin
        if (inclock_a'event) then
            if (i_indata_aclr_a = '1') then
                idata_hi_a <= (OTHERS => '0');
                idata_lo_a <= (OTHERS => '0');
            elsif inclock_a = '1' and inclocken_a = '1' then
                idata_hi_a <= data_a;
            elsif inclock_a = '0' then
                idata_lo_a <= idata_hi_a;
            end if;
            if (i_wraddress_aclr_a = '1') then
                iwraddress_hi_a <= (OTHERS => '0');
                iwraddress_lo_a <= (OTHERS => '0');
            elsif inclock_a = '1' and inclocken_a = '1' then
                iwraddress_hi_a <= wraddress_a;
            elsif inclock_a = '0' then
                iwraddress_lo_a <= iwraddress_hi_a;
            end if;
            if (i_wrcontrol_aclr_a = '1') then
                iwren_hi_a <= '0';
                iwren_lo_a <= '0';
            elsif inclock_a = '1' and inclocken_a = '1' then
                iwren_hi_a <= wren_a;
            elsif inclock_a = '0' then
                iwren_lo_a <= iwren_hi_a;
            end if;
    
            if (outdata_reg_a = "INCLOCK_A") then
                if (i_outdata_aclr_a = '1') then
                    iq_reg_a <= (OTHERS => '0');
                elsif inclock_a = '1' and inclocken_a = '1' then
                    iq_reg_a <= iq_tmp_a;
                end if;
            end if;
            if (rdaddress_reg_a = "INCLOCK_A") then
                if (i_rdaddress_aclr_a = '1') then
                    irdaddress_reg_a <= (OTHERS => '0');
                elsif inclock_a = '1' and inclocken_a = '1' then
                    irdaddress_reg_a <= rdaddress_a;
                end if;
            end if;
            if (rdcontrol_reg_a = "INCLOCK_A") then
                if (i_rdcontrol_aclr_a = '1') then
                    irden_reg_a <= '0';
                elsif inclock_a = '1' and inclocken_a = '1' then
                    irden_reg_a <= rden_a;
                end if;
            end if;
        end if;

        if (outclock_a'event) then
            if (outdata_reg_a = "OUTCLOCK_A") then
                if (i_outdata_aclr_a = '1') then
                    iq_reg_a <= (OTHERS => '0');
                elsif outclock_a = '1' and outclocken_a = '1' then
                    iq_reg_a <= iq_tmp_a;
                end if;
            end if;
            if (rdaddress_reg_a = "OUTCLOCK_A") then
                if (i_rdaddress_aclr_a = '1') then
                    irdaddress_reg_a <= (OTHERS => '0');
                elsif outclock_a = '1' and outclocken_a = '1' then
                    irdaddress_reg_a <= rdaddress_a;
                end if;
            end if;
            if (rdcontrol_reg_a = "OUTCLOCK_A") then
                if (i_rdcontrol_aclr_a = '1') then
                    irden_reg_a <= '0';
                elsif outclock_a = '1' and outclocken_a = '1' then
                    irden_reg_a <= rden_a;
                end if;
            end if;
        end if;
	end process;

    input_reg_b: process (inclock_b, outclock_b)
	begin
        if (inclock_b'event) then
            if (i_indata_aclr_b = '1') then
                idata_hi_b <= (OTHERS => '0');
                idata_lo_b <= (OTHERS => '0');
            elsif inclock_b = '1' and inclocken_b = '1' then
                idata_hi_b <= data_b;
            elsif inclock_b = '0' then
                idata_lo_b <= idata_hi_b;
            end if;
            if (i_wraddress_aclr_b = '1') then
                iwraddress_hi_b <= (OTHERS => '0');
                iwraddress_lo_b <= (OTHERS => '0');
            elsif inclock_b = '1' and inclocken_b = '1' then
                iwraddress_hi_b <= wraddress_b;
            elsif inclock_b = '0' then
                iwraddress_lo_b <= iwraddress_hi_b;
            end if;
            if (i_wrcontrol_aclr_b = '1') then
                iwren_hi_b <= '0';
                iwren_lo_b <= '0';
            elsif inclock_b = '1' and inclocken_b = '1' then
                iwren_hi_b <= wren_b;
            elsif inclock_b = '0' then
                iwren_lo_b <= iwren_hi_b;
            end if;
            if (outdata_reg_b = "INCLOCK_B") then
                if (i_outdata_aclr_b = '1') then
                    iq_reg_b <= (OTHERS => '0');
                elsif inclock_b = '1' and inclocken_b = '1' then
                    iq_reg_b <= iq_tmp_b;
                end if;
            end if;
            if (rdaddress_reg_b = "INCLOCK_B") then
                if (i_rdaddress_aclr_b = '1') then
                    irdaddress_reg_b <= (OTHERS => '0');
                elsif inclock_b = '1' and inclocken_b = '1' then
                    irdaddress_reg_b <= rdaddress_b;
                end if;
            end if;
            if (rdcontrol_reg_b = "INCLOCK_B") then
                if (i_rdcontrol_aclr_b = '1') then
                    irden_reg_b <= '0';
                elsif inclock_b = '1' and inclocken_b = '1' then
                    irden_reg_b <= rden_b;
                end if;
            end if;
        end if;

        if (outclock_b'event) then
            if (outdata_reg_b = "OUTCLOCK_B") then
                if (i_outdata_aclr_b = '1') then
                    iq_reg_b <= (OTHERS => '0');
                elsif outclock_b = '1' and outclocken_b = '1' then
                    iq_reg_b <= iq_tmp_b;
                end if;
            end if;
            if (rdaddress_reg_b = "OUTCLOCK_B") then
                if (i_rdaddress_aclr_b = '1') then
                    irdaddress_reg_b <= (OTHERS => '0');
                elsif outclock_b = '1' and outclocken_b = '1' then
                    irdaddress_reg_b <= rdaddress_b;
                end if;
            end if;
            if (rdcontrol_reg_b = "OUTCLOCK_B") then
                if (i_rdcontrol_aclr_b = '1') then
                    irden_reg_b <= '0';
                elsif outclock_b = '1' and outclocken_b = '1' then
                    irden_reg_b <= rden_b;
                end if;
            end if;
        end if;
	end process;


    ------------
    -- Memory --
    ------------

    memory: process(idata_tmp_a, iwren_tmp_a, iwraddress_tmp_a,
                    irden_tmp_a, irdaddress_tmp_a,
                    idata_tmp_b, iwren_tmp_b, iwraddress_tmp_b,
                    irden_tmp_b, irdaddress_tmp_b, mem_init)
    variable mem_data : alt_memory;
    variable mem_data_w : alt_memory_w;
    variable mem_data_word : std_logic_vector(WIDTH_READ_A-1 downto 0);
    variable mem_data_word_w : std_logic_vector(WIDTH_WRITE_A-1 downto 0);
    variable i, j, k, n, m, lineno : integer := 0;
    variable buf : line ;
    variable booval : boolean ;
    --FILE unused_file : TEXT IS OUT "UNUSED";
    --FILE mem_data_file : TEXT IS IN INIT_FILE;
    FILE unused_file : TEXT OPEN WRITE_MODE IS "UNUSED";
    FILE mem_data_file : TEXT OPEN READ_MODE IS INIT_FILE;
	variable base, byte, rec_type, datain, addr, checksum: string(2 downto 1);
	variable startadd: string(4 downto 1);
	variable ibase: integer := 0;
	variable ibyte: integer := 0;
	variable istartadd: integer := 0;
	variable check_sum_vec, check_sum_vec_tmp: std_logic_vector(7 downto 0);
    variable init_numwords_write_a : integer := numwords_write_a;
    variable init_numwords_write_b : integer := numwords_write_b;
    variable init_numwords_read_a : integer := numwords_read_a;
    variable init_numwords_read_b : integer := numwords_read_b;
    variable init_om : integer := 0;

	begin
		-- INITIALIZE --
		if NOT(mem_init) then

            -- CHECK FOR OPERATION_MODE --
            --
            -- This is the table for encoding of om and init_om.
            --       << PORT A >>      |  << PORT B >>
            --      RD  RD  WR  WR     | RD  RD  WR  WR
            --      EN ADDR EN ADDR %6 | EN ADDR EN ADDR %5
            -- QP    o   o   o   o   3 |  o   o   o   o   3
            -- BDP           o   o   2 |          o   o   2
            -- DP    o   o   o   o   3 |                  0
            -- SP            o   o   2 |                  0
            -- ROM       o           1 |                  0
            --
            if operation_mode = "QUAD_PORT" then
                init_om := 3;
            elsif operation_mode = "BIDIR_DUAL_PORT" then
                init_om := 2;
            elsif operation_mode = "DUAL_PORT" then
                init_om := 15;
            elsif operation_mode = "SINGLE_PORT" then
                init_om := 20;
            elsif operation_mode = "ROM" then
                init_om := 25;
            else
                ASSERT FALSE
                REPORT "Illegal OPERATION_MODE property value for ALTQPRAM!"
                SEVERITY ERROR;
            end if;
            om <= init_om;

            -- CHECK PARAMETERS (WIDTH_A) --
            if ((width_write_a <= 0) and
                (init_om mod 6 > 1))
            then
                ASSERT FALSE
                REPORT "Illegal WIDTH_WRITE_A property value for ALTQPRAM!"
                SEVERITY ERROR;
            end if;
            if ((widthad_write_a <= 0) and
                (init_om mod 6 > 1))
            then
                ASSERT FALSE
                REPORT "Illegal WIDTHAD_WRITE_A property value for ALTQPRAM!"
                SEVERITY ERROR;
            end if;
            if ((width_read_a <= 0) and
                (init_om mod 6 > 0) and
                (init_om mod 6 /= 2))
            then
                ASSERT FALSE
                REPORT "Illegal WIDTH_READ_A property value for ALTQPRAM!"
                SEVERITY ERROR;
            end if;
            if ((widthad_read_a <= 0) and
                (init_om mod 6 > 0) and
                (init_om mod 6 /= 2))
            then
                ASSERT FALSE
                REPORT "Illegal WIDTHAD_READ_A property value for ALTQPRAM!"
                SEVERITY ERROR;
            end if;
            if ((width_read_a /= width_write_a) and
                (init_om mod 6 = 2))
            then
                ASSERT FALSE
                REPORT "WIDTH_READ_A and WIDTH_WRITE_A property values for ALTQPRAM must equal!"
                SEVERITY ERROR;
            end if;

            -- CHECK PARAMETERS (WIDTH_B) --
            if ((width_write_b <= 0) and
                (init_om mod 5 > 1))
            then
                ASSERT FALSE
                REPORT "Illegal WIDTH_WRITE_B property value for ALTQPRAM!"
                SEVERITY ERROR;
            end if;
            if ((widthad_write_b <= 0) and
                (init_om mod 5 > 1))
            then
                ASSERT FALSE
                REPORT "Illegal WIDTHAD_WRITE_B property value for ALTQPRAM!"
                SEVERITY ERROR;
            end if;
            if ((width_read_b <= 0) and
                (init_om mod 5 > 2))
            then
                ASSERT FALSE
                REPORT "Illegal WIDTH_READ_B property value for ALTQPRAM!"
                SEVERITY ERROR;
            end if;
            if ((widthad_read_b <= 0) and
                (init_om mod 5 > 2))
            then
                ASSERT FALSE
                REPORT "Illegal WIDTHAD_READ_B property value for ALTQPRAM!"
                SEVERITY ERROR;
            end if;
            if ((width_read_b /= width_write_b) and
                (init_om mod 5 = 2))
            then
                ASSERT FALSE
                REPORT "WIDTH_READ_B and WIDTH_WRITE_B property values for ALTQPRAM must equal!"
                SEVERITY ERROR;
            end if;

            -- CHECK PARAMETERS (REG_A) --
            if ((indata_reg_a /= "INCLOCK_A") and
                (indata_reg_a /= "UNREGISTERED") and
                (init_om mod 6 > 0))
            then
                ASSERT FALSE
                REPORT "Illegal INDATA_REG_A property value for ALTQPRAM!"
                SEVERITY ERROR;
            end if;
            if ((wrcontrol_wraddress_reg_a /= "INCLOCK_A") and
                (wrcontrol_wraddress_reg_a /= "UNREGISTERED") and
                (init_om mod 6 > 0))
            then
                ASSERT FALSE
                REPORT "Illegal WRCONTROL_REG_A property value for ALTQPRAM!"
                SEVERITY ERROR;
            end if;
            if ((outdata_reg_a /= "INCLOCK_A") and
                (outdata_reg_a /= "OUTCLOCK_A") and
                (outdata_reg_a /= "UNREGISTERED") and
                (init_om mod 6 > 0))
            then
                ASSERT FALSE
                REPORT "Illegal OUTDATA_REG_A property value for ALTQPRAM!"
                SEVERITY ERROR;
            end if;
            if ((rdcontrol_reg_a /= "INCLOCK_A") and
                (rdcontrol_reg_a /= "OUTCLOCK_A") and
                (rdcontrol_reg_a /= "UNREGISTERED") and
                (init_om mod 6 > 0))
            then
                ASSERT FALSE
                REPORT "Illegal RDCONTROL_REG_A property value for ALTQPRAM!"
                SEVERITY ERROR;
            end if;
            if ((rdaddress_reg_a /= "INCLOCK_A") and
                (rdaddress_reg_a /= "OUTCLOCK_A") and
                (rdaddress_reg_a /= "UNREGISTERED") and
                (init_om mod 6 > 0))
            then
                ASSERT FALSE
                REPORT "Illegal RDADDRESS_REG_A property value for ALTQPRAM!"
                SEVERITY ERROR;
            end if;

            -- CHECK PARAMETERS (REG_B) --
            if ((indata_reg_b /= "INCLOCK_B") and
                (indata_reg_b /= "UNREGISTERED") and
                (init_om mod 5 > 0))
            then
                ASSERT FALSE
                REPORT "Illegal INDATA_REG_B property value for ALTQPRAM!"
                SEVERITY ERROR;
            end if;
            if ((wrcontrol_wraddress_reg_b /= "INCLOCK_B") and
                (wrcontrol_wraddress_reg_b /= "UNREGISTERED") and
                (init_om mod 5 > 0))
            then
                ASSERT FALSE
                REPORT "Illegal WRCONTROL_REG_B property value for ALTQPRAM!"
                SEVERITY ERROR;
            end if;
            if ((outdata_reg_b /= "INCLOCK_B") and
                (outdata_reg_b /= "OUTCLOCK_B") and
                (outdata_reg_b /= "UNREGISTERED") and
                (init_om mod 5 > 0))
            then
                ASSERT FALSE
                REPORT "Illegal OUTDATA_REG_B property value for ALTQPRAM!"
                SEVERITY ERROR;
            end if;
            if ((rdcontrol_reg_b /= "INCLOCK_B") and
                (rdcontrol_reg_b /= "OUTCLOCK_B") and
                (rdcontrol_reg_b /= "UNREGISTERED") and
                (init_om mod 5 > 0))
            then
                ASSERT FALSE
                REPORT "Illegal RDCONTROL_REG_B property value for ALTQPRAM!"
                SEVERITY ERROR;
            end if;
            if ((rdaddress_reg_b /= "INCLOCK_B") and
                (rdaddress_reg_b /= "OUTCLOCK_B") and
                (rdaddress_reg_b /= "UNREGISTERED") and
                (init_om mod 5 > 0))
            then
                ASSERT FALSE
                REPORT "Illegal RDADDRESS_REG_B property value for ALTQPRAM!"
                SEVERITY ERROR;
            end if;

            -- CHECK PARAMETERS (ACLR_A) --
            if ((indata_aclr_a /= "INACLR_A") and
                (indata_aclr_a /= "NONE"))
            then
                ASSERT FALSE
                REPORT "Illegal INDATA_ACLR_A property value for ALTQPRAM!"
                SEVERITY ERROR;
            end if;
            if ((wrcontrol_aclr_a /= "INACLR_A") and
                (wrcontrol_aclr_a /= "NONE"))
            then
                ASSERT FALSE
                REPORT "Illegal WRCONTROL_ACLR_A property value for ALTQPRAM!"
                SEVERITY ERROR;
            end if;
            if ((wraddress_aclr_a /= "INACLR_A") and
                (wraddress_aclr_a /= "NONE"))
            then
                ASSERT FALSE
                REPORT "Illegal WRADDRESS_ACLR_A property value for ALTQPRAM!"
                SEVERITY ERROR;
            end if;
            if ((outdata_aclr_a /= "INACLR_A") and
                (outdata_aclr_a /= "OUTACLR_A") and
                (outdata_aclr_a /= "NONE"))
            then
                ASSERT FALSE
                REPORT "Illegal OUTDATA_ACLR_A property value for ALTQPRAM!"
                SEVERITY ERROR;
            end if;
            if ((rdcontrol_aclr_a /= "INACLR_A") and
                (rdcontrol_aclr_a /= "OUTACLR_A") and
                (rdcontrol_aclr_a /= "NONE"))
            then
                ASSERT FALSE
                REPORT "Illegal RDCONTROL_ACLR_A property value for ALTQPRAM!"
                SEVERITY ERROR;
            end if;
            if ((rdaddress_aclr_a /= "INACLR_A") and
                (rdaddress_aclr_a /= "OUTACLR_A") and
                (rdaddress_aclr_a /= "NONE"))
            then
                ASSERT FALSE
                REPORT "Illegal RDADDRESS_ACLR_A property value for ALTQPRAM!"
                SEVERITY ERROR;
            end if;

            -- CHECK PARAMETERS (ACLR_B) --
            if ((indata_aclr_b /= "INACLR_B") and
                (indata_aclr_b /= "NONE"))
            then
                ASSERT FALSE
                REPORT "Illegal INDATA_ACLR_B property value for ALTQPRAM!"
                SEVERITY ERROR;
            end if;
            if ((wrcontrol_aclr_b /= "INACLR_B") and
                (wrcontrol_aclr_b /= "NONE"))
            then
                ASSERT FALSE
                REPORT "Illegal WRCONTROL_ACLR_B property value for ALTQPRAM!"
                SEVERITY ERROR;
            end if;
            if ((wraddress_aclr_b /= "INACLR_B") and
                (wraddress_aclr_b /= "NONE"))
            then
                ASSERT FALSE
                REPORT "Illegal WRADDRESS_ACLR_B property value for ALTQPRAM!"
                SEVERITY ERROR;
            end if;
            if ((outdata_aclr_b /= "INACLR_B") and
                (outdata_aclr_b /= "OUTACLR_B") and
                (outdata_aclr_b /= "NONE"))
            then
                ASSERT FALSE
                REPORT "Illegal OUTDATA_ACLR_B property value for ALTQPRAM!"
                SEVERITY ERROR;
            end if;
            if ((rdcontrol_aclr_b /= "INACLR_B") and
                (rdcontrol_aclr_b /= "OUTACLR_B") and
                (rdcontrol_aclr_b /= "NONE"))
            then
                ASSERT FALSE
                REPORT "Illegal RDCONTROL_ACLR_B property value for ALTQPRAM!"
                SEVERITY ERROR;
            end if;
            if ((rdaddress_aclr_b /= "INACLR_B") and
                (rdaddress_aclr_b /= "OUTACLR_B") and
                (rdaddress_aclr_b /= "NONE"))
            then
                ASSERT FALSE
                REPORT "Illegal RDADDRESS_ACLR_B property value for ALTQPRAM!"
                SEVERITY ERROR;
            end if;

            -- SET DEFAULT VALUES OF NUMWORDS'S --
            if (numwords_write_a = 0) then
                inumwords_write_a <= 2**widthad_write_a;
                init_numwords_write_a := 2**widthad_write_a;
            end if;
            if (numwords_write_b = 0) then
                inumwords_write_b <= 2**widthad_write_b;
                init_numwords_write_b := 2**widthad_write_b;
            end if;
            if (numwords_read_a = 0) then
                inumwords_read_a <= 2**widthad_read_a;
                init_numwords_read_a := 2**widthad_read_a;
            end if;
            if (numwords_read_b = 0) then
                inumwords_read_b <= 2**widthad_read_b;
                init_numwords_read_b := 2**widthad_read_b;
            end if;

            -- CHECK RAM PORT SIZE --
            if ((width_read_a*init_numwords_read_a /= width_write_a*init_numwords_write_a) and
                (init_om mod 6 > 0) and (init_om mod 6 /= 2))
            then
                ASSERT FALSE
                REPORT "Inconsistant RAM size for port A of ALTQPRAM!"
                SEVERITY ERROR;
            end if;
            if (init_om mod 5 > 1) then
                if ((width_read_b*init_numwords_read_b /= width_write_b*init_numwords_write_b)) then
                    ASSERT FALSE
                    REPORT "Inconsistant RAM size for port B of ALTQPRAM!"
                    SEVERITY ERROR;
                end if;
                if ((width_read_a*init_numwords_read_a /= width_read_b*init_numwords_read_b)) then
                    ASSERT FALSE
                    REPORT "Inconsistant RAM size between port A and port B of ALTQPRAM!"
                    SEVERITY ERROR;
                end if;
            end if;

            -- CHECK RAM SIZE RATIO --
            i := width_read_b / width_read_a;
            if (i < 1) then
                i := width_read_a / width_read_b;
            end if;
            j := width_write_b / width_write_a;
            if (j < 1) then
                j := width_write_a / width_write_b;
            end if;
            k := width_write_a / width_read_a;
            if (k < 1) then
                k := width_read_a / width_write_a;
            end if;
            if ((i/=1 and i/=2 and i/=4 and i/=8 and i/=16) or
                (j/=1 and j/=2 and j/=4 and j/=8 and j/=16) or
                (k/=1 and k/=2 and k/=4 and k/=8 and k/=16))
            then
                ASSERT FALSE
                REPORT "Invalid RAM size for port A and/or port B of ALTQPRAM!"
                SEVERITY WARNING;
            end if;

            -- SET MEMORY WRITING BEHAVIOUR --
            if (wrcontrol_wraddress_reg_a /= "UNREGISTERED") then
                write_at_low_clock_a <= true;
            end if;
            if (wrcontrol_wraddress_reg_b /= "UNREGISTERED") then
                write_at_low_clock_b <= true;
            end if;

            -- INITIALIZE MEMORY --
            if (INIT_FILE = "UNUSED" or INIT_FILE = "") then
                if (init_om mod 6 = 1) then         -- if ROM mode
                    ASSERT FALSE
                    REPORT "Initialization file not found!"
                    SEVERITY ERROR;
                elsif (init_om mod 6 = 2) then      -- if SP or BDP mode
                    -- INITIALIZE MEM_DATA_W TO ALL 0 --
                    for i in mem_data_w'LOW to mem_data_w'HIGH loop
                        mem_data_w(i) := (OTHERS => '0');
                    end loop;
                else                                -- if QP or DP mode
                    -- INITIALIZE MEM_DATA TO ALL 0 --
                    for i in mem_data'LOW to mem_data'HIGH loop
                        mem_data(i) := (OTHERS => '0');
                    end loop;
                end if;
            else
				WHILE NOT ENDFILE(mem_data_file) loop
					booval := true;
					READLINE(mem_data_file, buf);
					lineno := lineno + 1;
					check_sum_vec := (OTHERS => '0');
					if (buf(buf'LOW) = ':') then
						i := 1;
						shrink_line(buf, i);
						READ(L=>buf, VALUE=>byte, good=>booval);
						if not (booval) then
							ASSERT FALSE
							REPORT "[Line "& int_to_str(lineno) & "]:Illegal Intel Hex Format!"
							SEVERITY ERROR;
						end if;
						ibyte := hex_str_to_int(byte);
						check_sum_vec := unsigned(check_sum_vec) + unsigned(CONV_STD_LOGIC_VECTOR(ibyte, 8));
						READ(L=>buf, VALUE=>startadd, good=>booval);
						if not (booval) then
							ASSERT FALSE
							REPORT "[Line "& int_to_str(lineno) & "]:Illegal Intel Hex Format! "
							SEVERITY ERROR;
						end if;
						istartadd := hex_str_to_int(startadd);
						addr(2) := startadd(4);
						addr(1) := startadd(3);
						check_sum_vec := unsigned(check_sum_vec) + unsigned(CONV_STD_LOGIC_VECTOR(hex_str_to_int(addr), 8));
						addr(2) := startadd(2);
						addr(1) := startadd(1);
						check_sum_vec := unsigned(check_sum_vec) + unsigned(CONV_STD_LOGIC_VECTOR(hex_str_to_int(addr), 8));
						READ(L=>buf, VALUE=>rec_type, good=>booval);
						if not (booval) then
							ASSERT FALSE
							REPORT "[Line "& int_to_str(lineno) & "]:Illegal Intel Hex Format! "
							SEVERITY ERROR;
						end if;
						check_sum_vec := unsigned(check_sum_vec) + unsigned(CONV_STD_LOGIC_VECTOR(hex_str_to_int(rec_type), 8));
					else
						ASSERT FALSE
						REPORT "[Line "& int_to_str(lineno) & "]:Illegal Intel Hex Format! "
						SEVERITY ERROR;
					end if;
					case rec_type is
						when "00"=>     -- Data record
							i := 0;
                                                        if (init_om mod 6 = 2) then -- if SP or BDP mode
                                                            k := (WIDTH_WRITE_A + 7) / 8;  -- # of bytes per entry
                                                        else                        -- if ROM, QP or DP mode
                                                            k := (WIDTH_READ_A + 7) / 8;  -- # of bytes per entry
                                                        end if;
							while (i < ibyte) loop
                                                                n := (k - 1)*8;
                                                                if (init_om mod 6 = 2) then
                                                                    mem_data_word_w := (others => '0');
                                                                    m := WIDTH_WRITE_A - 1;
                                                                else
                                                                    mem_data_word := (others => '0');
                                                                    m := WIDTH_READ_A - 1;
                                                                end if;
								for j in 1 to k loop
									READ(L=>buf, VALUE=>datain,good=>booval); -- read in data a byte (2 hex chars) at a time.
									if not (booval) then
										ASSERT FALSE
										REPORT "[Line "& int_to_str(lineno) & "]:Illegal Intel Hex Format! "
										SEVERITY ERROR;
									end if;
									check_sum_vec := unsigned(check_sum_vec) + unsigned(CONV_STD_LOGIC_VECTOR(hex_str_to_int(datain), 8));
                                                                        if (init_om mod 6 = 2) then -- if SP or BDP mode
                                                                            mem_data_word_w(m downto n) := CONV_STD_LOGIC_VECTOR(hex_str_to_int(datain), m-n+1);
                                                                        else                        -- if ROM, QP or DP mode
                                                                            mem_data_word(m downto n) := CONV_STD_LOGIC_VECTOR(hex_str_to_int(datain), m-n+1);
                                                                        end if;
                                                                        m := n - 1;
                                                                        n := n - 8;
								end loop;
								i := i + k;
                                                                if (init_om mod 6 = 2) then -- if SP or BDP mode
                                                                    mem_data_w(ibase + istartadd) := mem_data_word_w;
                                                                else                        -- if ROM, QP or DP mode
                                                                    mem_data(ibase + istartadd) := mem_data_word;
                                                                end if;
								istartadd := istartadd + 1;
							end loop;
						when "01"=>
							exit;
						when "02"=>
							ibase := 0;
							if (ibyte /= 2) then
								ASSERT FALSE
								REPORT "[Line "& int_to_str(lineno) & "]:Illegal Intel Hex Format for record type 02! "
								SEVERITY ERROR;
							end if;
							for i in 0 to (ibyte-1) loop
								READ(L=>buf, VALUE=>base,good=>booval);
								ibase := ibase * 256 + hex_str_to_int(base);
								if not (booval) then
									ASSERT FALSE
									REPORT "[Line "& int_to_str(lineno) & "]:Illegal Intel Hex Format! "
									SEVERITY ERROR;
								end if;
								check_sum_vec := unsigned(check_sum_vec) + unsigned(CONV_STD_LOGIC_VECTOR(hex_str_to_int(base), 8));
							end loop;
							ibase := ibase * 16;
						when OTHERS =>
							ASSERT FALSE
							REPORT "[Line "& int_to_str(lineno) & "]:Illegal record type in Intel Hex File! "
							SEVERITY ERROR;
					end case;
                    READ(L=>buf, VALUE=>checksum, good=>booval);
					if not (booval) then
						ASSERT FALSE
						REPORT "[Line "& int_to_str(lineno) & "]:Checksum is missing! "
						SEVERITY ERROR;
					end if;

                    check_sum_vec := unsigned(not (check_sum_vec)) + 1;
					check_sum_vec_tmp := CONV_STD_LOGIC_VECTOR(hex_str_to_int(checksum),8);

					if (unsigned(check_sum_vec) /= unsigned(check_sum_vec_tmp)) then
						ASSERT FALSE
						REPORT "[Line "& int_to_str(lineno) & "]:Incorrect checksum!"
						SEVERITY ERROR;
					end if;
				end loop;
			end if;
            mem_init <= TRUE;

        else -- already initialized

            ---------------------
            -- MEMORY FUNCTION --
            ---------------------
            if (iwren_tmp_a = '1' and
                om mod 6 > 1) then                  -- not ROM mode

                if (om mod 6 = 2) then              -- SP or BDP mode
                    j := ieee.std_logic_unsigned.conv_integer(iwraddress_tmp_a);
                    mem_data_w(j) := idata_tmp_a;
                else                                -- QP or DP mode
                    j := ieee.std_logic_unsigned.conv_integer(iwraddress_tmp_a) * width_write_a;
                    for i in 0 to width_write_a-1 loop
                        mem_data((j+i)/width_read_a)((j+i) mod width_read_a) := idata_tmp_a(i);
                    end loop;
                end if;
            end if;

            if (iwren_tmp_b = '1' and
                om mod 5 > 0) then                  -- QP or BDP mode
                j := ieee.std_logic_unsigned.conv_integer(iwraddress_tmp_b) * width_write_b;

                if (om mod 5 = 2) then              -- BDP mode
                    for i in 0 to width_write_b-1 loop
                        mem_data_w((j+i)/width_write_a)((j+i) mod width_write_a) := idata_tmp_b(i);
                    end loop;
                else                                -- QP mode
                    for i in 0 to width_write_b-1 loop
                        mem_data((j+i)/width_read_a)((j+i) mod width_read_a) := idata_tmp_b(i);
                    end loop;
                end if;
            end if;

            if (om mod 6 = 3) then                  -- QP or DP mode
                if irden_tmp_a = '1' then
                    j := ieee.std_logic_unsigned.conv_integer(irdaddress_tmp_a);
                    iq_tmp_a <= mem_data(j);
                end if;
            elsif (om mod 6 = 2) then               -- BDP or SP mode
                j := ieee.std_logic_unsigned.conv_integer(iwraddress_tmp_a);
                iq_tmp_a <= mem_data_w(j);
            elsif (om mod 6 = 1) then               -- ROM mode
                j := ieee.std_logic_unsigned.conv_integer(irdaddress_tmp_a);
                iq_tmp_a <= mem_data(j);
            end if;

            if (om mod 5 = 3) then                  -- QP mode
                if irden_tmp_b = '1' then
                    j := ieee.std_logic_unsigned.conv_integer(irdaddress_tmp_b) * width_read_b;
                    for i in 0 to width_read_b-1 loop
                        iq_tmp_b(i) <= mem_data((j+i)/width_read_a)((j+i) mod width_read_a);
                    end loop;
                end if;
            elsif (om mod 5 = 2) then               -- BDP mode
                j := ieee.std_logic_unsigned.conv_integer(iwraddress_tmp_b) * width_write_b;
                for i in 0 to width_write_b-1 loop
                    iq_tmp_b(i) <= mem_data_w((j+i)/width_write_a)((j+i) mod width_write_a);
                end loop;
            end if;

        end if; -- if NOT(mem_init)
	end process;

end behavior;


----------------------------------------------------------------------------
-- scfifo megafunction
----------------------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;

entity SCFIFO is
    generic
      ( lpm_width               : natural;
        lpm_widthu              : natural;
        lpm_numwords            : natural;
        lpm_showahead           : string := "OFF";
        lpm_hint                : string := "USE_EAB=ON";
        almost_full_value       : natural := 0;
        almost_empty_value      : natural := 0;
        overflow_checking       : string := "ON";
        underflow_checking      : string := "ON";
        allow_rwcycle_when_full : string := "OFF";
        -- parameters that should be stored in lpm_hint
        use_eab                 : string := "ON");
    port
      ( data         : in std_logic_vector(lpm_width-1 downto 0);
        clock        : in std_logic;
        wrreq        : in std_logic;
        rdreq        : in std_logic;
        aclr         : in std_logic := '0';
        sclr         : in std_logic := '0';
        full         : out std_logic;
        almost_full  : out std_logic;
        empty        : out std_logic;
        almost_empty : out std_logic;
        q            : out std_logic_vector(lpm_width-1 downto 0);
        usedw        : out std_logic_vector(lpm_widthu-1 downto 0)
     );
end SCFIFO; 

architecture behavior of SCFIFO is

type lpm_memory is array (lpm_numwords-1 downto 0) of std_logic_vector(lpm_width-1 downto 0);

signal tmp_q : std_logic_vector(lpm_width-1 downto 0) := (OTHERS => '0');
signal read_id, write_id, count_id : integer := 0;
signal empty_flag : std_logic := '1';
signal full_flag : std_logic := '0';
constant ZEROS : std_logic_vector(lpm_width-1 downto 0) := (OTHERS => '0');

begin

	process (clock, aclr)
	variable mem_data : lpm_memory := (OTHERS => ZEROS);
    variable valid_rreq, valid_wreq : boolean;
    variable init : boolean := false;
	begin
        if (not init) then
            if (LPM_SHOWAHEAD /= "ON" and LPM_SHOWAHEAD /= "OFF") then
                ASSERT FALSE
                REPORT "Illegal LPM_SHOWAHEAD property value for SCFIFO!"
                SEVERITY ERROR;
            end if;
            if (UNDERFLOW_CHECKING /= "ON" and UNDERFLOW_CHECKING /= "OFF") then
                ASSERT FALSE
                REPORT "Illegal UNDERFLOW_CHECKING property value for SCFIFO!"
                SEVERITY ERROR;
            end if;
            if (OVERFLOW_CHECKING /= "ON" and OVERFLOW_CHECKING /= "OFF") then
                ASSERT FALSE
                REPORT "Illegal OVERFLOW_CHECKING property value for SCFIFO!"
                SEVERITY ERROR;
            end if;
            if (ALLOW_RWCYCLE_WHEN_FULL /= "ON" and ALLOW_RWCYCLE_WHEN_FULL /= "OFF") then
                ASSERT FALSE
                REPORT "Illegal ALLOW_RWCYCLE_WHEN_FULL property value for SCFIFO!"
                SEVERITY ERROR;
            end if;
            init := true;
        end if; -- not init

		if (aclr = '1') then
			tmp_q <= ZEROS;
			full_flag <= '0';
			empty_flag <= '1';
			read_id <= 0;
			write_id <= 0;
			count_id <= 0;
			if (lpm_showahead = "ON") then
				tmp_q <= mem_data(0);
			end if;
		elsif (clock'event and clock = '1') then
			if (sclr = '1') then
				tmp_q <= mem_data(read_id);
				full_flag <= '0';
				empty_flag <= '1';
				read_id <= 0;
				write_id <= 0;
				count_id <= 0;
				if (lpm_showahead = "ON") then
					tmp_q <= mem_data(0);
				end if;
			else
                valid_rreq := rdreq = '1' and (empty_flag = '0' or underflow_checking = "OFF");
                valid_wreq := wrreq = '1' and (full_flag = '0' or overflow_checking = "OFF" or
                                               (rdreq = '1' and allow_rwcycle_when_full = "ON"));

				----- IF BOTH READ AND WRITE -----
                if (valid_wreq and valid_rreq) then

					mem_data(write_id) := data;
					if (write_id >= lpm_numwords-1) then
						write_id <= 0;
					else
						write_id <= write_id + 1;
					end if;

					tmp_q <= mem_data(read_id);
					if (read_id >= lpm_numwords-1) then
						read_id <= 0;
						if (lpm_showahead = "ON") then
							tmp_q <= mem_data(0);
						end if;
					else
						read_id <= read_id + 1;
						if (lpm_showahead = "ON") then
							tmp_q <= mem_data(read_id+1);
						end if;
					end if;

				----- IF WRITE (ONLY) -----
                elsif (valid_wreq) then

					mem_data(write_id) := data;
					if (lpm_showahead = "ON") then
						tmp_q <= mem_data(read_id);
					end if;
					count_id <= count_id + 1;
					empty_flag <= '0';
					if (count_id >= lpm_numwords-1) then
						full_flag <= '1';
						count_id <= lpm_numwords;
					end if;
					if (write_id >= lpm_numwords-1) then
						write_id <= 0;
					else
						write_id <= write_id + 1;
					end if;

				----- IF READ (ONLY) -----
                elsif (valid_rreq) then

					tmp_q <= mem_data(read_id);
					count_id <= count_id - 1;
					full_flag <= '0';
					if (count_id <= 1) then
						empty_flag <= '1';
						count_id <= 0;
					end if;
					if (read_id >= lpm_numwords-1) then
						read_id <= 0;
						if (lpm_showahead = "ON") then
							tmp_q <= mem_data(0);
						end if;
					else
						read_id <= read_id + 1;
						if (lpm_showahead = "ON") then
							tmp_q <= mem_data(read_id+1);
						end if;
					end if;
				end if;  -- if WRITE and/or READ
			end if;  -- if sclr = '1'
		end if;  -- if aclr = '1'
	end process;

    process (count_id)
    begin
        if (count_id >= almost_full_value) then
            almost_full <= '1';
        else
            almost_full <= '0';
        end if;
        if (count_id < almost_empty_value) then
            almost_empty <= '1';
        else
            almost_empty <= '0';
        end if;
    end process;

	q <= tmp_q;
	full <= full_flag;
	empty <= empty_flag;
	usedw <= conv_std_logic_vector(count_id, lpm_widthu);

end behavior;


----------------------------------------------------------------------------
-- dcfifo_dffpipe (used by dcfifo)
----------------------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;

entity DCFIFO_DFFPIPE is
    generic (LPM_DELAY : natural);
    port (D     : in integer;
          Q     : out integer;
          CLOCK : in std_logic;
          ACLR  : in std_logic := '0');
end DCFIFO_DFFPIPE;

architecture behavior of DCFIFO_DFFPIPE is
type delaypipe is array (LPM_DELAY downto 0) of integer;

begin

    process (clock, aclr, d)
    variable intpipe : delaypipe := (OTHERS => 0);
    variable delay : integer := LPM_DELAY-1;
    variable init : boolean := false;
    begin
        if (LPM_DELAY = 0) then
            if (aclr = '1' or not init) then
                q <= 0;
                init := true;
            else
                q <= d;
            end if;
        else
            if (aclr = '1' or not init) then
                for i in LPM_DELAY downto 0 loop
                    intpipe(i) := 0;
                end loop;
                init := true;
                q <= 0;
            end if;
            if (clock'event and clock = '1' and NOW > 0 ns) then
                if (delay > 0) then
                    for i in delay downto 1 loop
                        intpipe(i) := intpipe(i-1);
                    end loop;
                end if;
                intpipe(0) := d;
                q <= intpipe(delay);
            end if;
        end if;
    end process;

end behavior;

----------------------------------------------------------------------------
-- dcfifo_fefifo (used by dcfifo_sync and dcfifo_async)
----------------------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;

entity DCFIFO_FEFIFO is
    generic (LPM_WIDTHAD  : natural;
             LPM_NUMWORDS : natural;
             UNDERFLOW_CHECKING : string := "ON";
             OVERFLOW_CHECKING : string := "ON";
             LPM_MODE     : string );
    port (USEDW_IN : in std_logic_vector(LPM_WIDTHAD-1 downto 0);
          WREQ     : in std_logic := 'Z';
          RREQ     : in std_logic := 'Z';
          EMPTY    : out std_logic;
          FULL     : out std_logic;
          CLOCK    : in std_logic;
          ACLR     : in std_logic := '0');
end DCFIFO_FEFIFO;

architecture behavior of DCFIFO_FEFIFO is
signal usedw : std_logic_vector(LPM_WIDTHAD-1 downto 0);
signal sm_empty : std_logic_vector(1 downto 0) := "00";
signal lrreq : std_logic := '0'; -- DFF;
signal i_empty : std_logic := '1';
signal i_full : std_logic := '0';
signal valid_rreq : std_logic;

begin

    -- Initialization
    process (clock, aclr)
    variable init : boolean := false;
    begin
        if (not init) then
            if (LPM_MODE /= "READ" and LPM_MODE /= "WRITE") then
                ASSERT FALSE
                REPORT "Illegal LPM_MODE property value for DCFIFO_FEFIFO!"
                SEVERITY ERROR;
            end if;
            if (UNDERFLOW_CHECKING /= "ON" and UNDERFLOW_CHECKING /= "OFF") then
                ASSERT FALSE
                REPORT "Illegal UNDERFLOW_CHECKING property value for DCFIFO_FEFIFO!"
                SEVERITY ERROR;
            end if;
            if (OVERFLOW_CHECKING /= "ON" and OVERFLOW_CHECKING /= "OFF") then
                ASSERT FALSE
                REPORT "Illegal OVERFLOW_CHECKING property value for DCFIFO_FEFIFO!"
                SEVERITY ERROR;
            end if;
            init := true;
        end if; -- not init
    end process;

    valid_rreq <= rreq when underflow_checking = "OFF" else
                  rreq and not i_empty;

    process (clock, aclr)
    begin
        if (aclr = '1') then
            lrreq <= '0';
        elsif (clock'event and clock = '1' and NOW > 0 ns) then
            lrreq <= valid_rreq;
        end if;
    end process;

    process (clock, aclr)
    variable almost_full : integer := 0;
    begin
        if (aclr = '1') then
            i_full <= '0';
        elsif (clock'event and clock = '1' and NOW > 0 ns) then
            if (lpm_numwords >= 3) then
                almost_full := lpm_numwords-3;
            end if;
            if (unsigned(usedw) >= almost_full) then
                i_full <= '1';
            else
                i_full <= '0';
            end if;
        end if;
    end process;

    process (clock, aclr)
    variable local_sm_empty : std_logic_vector(1 downto 0) := "00";
    variable usedw_is_1 : boolean;
    begin
        local_sm_empty := sm_empty;
        if (aclr = '1') then
            local_sm_empty := "00";
        elsif (clock'event and clock = '1' and NOW > 0 ns) then
            if (lpm_mode = "READ") then
                case sm_empty is
                    when "00" =>                    -- state_empty
                        if (usedw /= 0) then
                            local_sm_empty := "01";
                        end if;
                    when "01" =>                    -- state_non_empty
                        usedw_is_1 := (usedw = 1 and lrreq = '0') or (usedw = 2 and lrreq = '1');
                        if (rreq = '1' and usedw_is_1) then
                            local_sm_empty := "10";
                        end if;
                    when "10" =>                    -- state_emptywait
                        if (usedw > 1) then
                            local_sm_empty := "01";
                        else
                            local_sm_empty := "00";
                        end if;
                    when others =>
                        -- INTERNAL ERROR
                end case;
            elsif (lpm_mode = "WRITE") then
                case sm_empty is
                    when "00" =>                    -- state_empty
                        if (wreq = '1') then
                            local_sm_empty := "01";
                        end if;
                    when "01" =>                    -- state_one
                        if (wreq = '0') then
                            local_sm_empty := "11";
                        end if;
                    when "11" =>                    -- state_non_empty
                        if (wreq = '1') then
                            local_sm_empty := "01";
                        elsif (usedw = 0) then
                            local_sm_empty := "00";
                        end if;
                    when others =>
                        -- INTERNAL ERROR
                end case;
            end if;
        end if;
        sm_empty <= local_sm_empty;
        i_empty <= not local_sm_empty(0);
    end process;

    usedw <= usedw_in;
    empty <= i_empty;
    full <= i_full;

end behavior;


----------------------------------------------------------------------------
-- dcfifo_async (used by dcfifo)
----------------------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;

entity DCFIFO_ASYNC is
    generic
      ( lpm_width               : natural;
        lpm_widthu              : natural;
        lpm_numwords            : natural;
        lpm_showahead           : string := "OFF";
        lpm_hint                : string := "USE_EAB=ON";
        overflow_checking       : string := "ON";
        underflow_checking      : string := "ON";
        delay_rdusedw           : natural := 1;
        delay_wrusedw           : natural := 1;
        rdsync_delaypipe        : natural := 3;
        wrsync_delaypipe        : natural := 3;
        -- parameters that should be stored in lpm_hint
        use_eab                 : string := "ON";
        clocks_are_synchronized : string := "FALSE");
    port
      ( data    : in std_logic_vector(lpm_width-1 downto 0);
        rdclk   : in std_logic;
        wrclk   : in std_logic;
        wrreq   : in std_logic;
        rdreq   : in std_logic;
        aclr    : in std_logic := '0';
        rdfull  : out std_logic;
        wrfull  : out std_logic;
        wrempty : out std_logic;
        rdempty : out std_logic;
        q       : out std_logic_vector(lpm_width-1 downto 0);
        rdusedw : out std_logic_vector(lpm_widthu-1 downto 0);
        wrusedw : out std_logic_vector(lpm_widthu-1 downto 0));
end DCFIFO_ASYNC; 

architecture behavior of DCFIFO_ASYNC is

type lpm_memory is array (2**lpm_widthu-1 downto 0) of std_logic_vector(lpm_width-1 downto 0);

signal i_q : std_logic_vector(lpm_width-1 downto 0) := (OTHERS => '0');
signal i_rdptr, i_wrptr, i_rdptrrg, i_wrdelaycycle : integer := 0;
signal i_ws_nbrp, i_rs_nbwp, i_ws_dbrp, i_rs_dbwp : integer := 0;
signal i_wr_udwn, i_rd_udwn, i_wr_dbuw, i_rd_dbuw : integer := 0;
signal i_rdempty, i_wrempty : std_logic := '1';
signal i_rdfull, i_wrfull : std_logic := '0';
signal i_rdusedw, i_wrusedw : integer := 0;
signal i_rden, i_wren, i_rdenclock : std_logic := '0';
signal slv_wr_dbuw, slv_rd_dbuw : std_logic_vector(lpm_widthu-1 downto 0);

signal i_data_tmp, i_data_reg : std_logic_vector(lpm_width-1 downto 0);
signal i_q_tmp, i_q_reg : std_logic_vector(lpm_width-1 downto 0) := (OTHERS => '0');
signal i_rdptr_tmp, i_wrptr_tmp, i_wrptr_reg : integer := 0;
signal i_wren_tmp, i_wren_reg : std_logic := '0';

constant ZEROS : std_logic_vector(lpm_width-1 downto 0) := (OTHERS => '0');
constant GRAY_DELAYPIPE : integer := 1;
constant WRUSEDW_DELAYPIPE : integer := 1;  -- delayed usedw to compute empty/full
constant RDUSEDW_DELAYPIPE : integer := 1;  -- delayed usedw to compute empty/full

component DCFIFO_FEFIFO
    generic (LPM_WIDTHAD : natural;
             LPM_NUMWORDS : natural;
             UNDERFLOW_CHECKING : string := "ON";
             OVERFLOW_CHECKING : string := "ON";
             LPM_MODE : string);
    port (USEDW_IN : in std_logic_vector(LPM_WIDTHAD-1 downto 0);
          WREQ : in std_logic := 'Z';
          RREQ : in std_logic := 'Z';
          EMPTY : out std_logic;
          FULL : out std_logic;
          CLOCK : in std_logic;
          ACLR : in std_logic := '0');
end component;

component DCFIFO_DFFPIPE
    generic (LPM_DELAY : natural);
    port (D : in integer;
          Q : out integer;
          CLOCK : in std_logic;
          ACLR : in std_logic := '0');
end component;

begin

    -- Initialization
    process (wrclk, rdclk, aclr)
    variable init : boolean := false;
    begin
        if (not init) then
            if (LPM_SHOWAHEAD /= "ON" and LPM_SHOWAHEAD /= "OFF") then
                ASSERT FALSE
                REPORT "Illegal LPM_SHOWAHEAD property value for DCFIFO!"
                SEVERITY ERROR;
            end if;
            if (UNDERFLOW_CHECKING /= "ON" and UNDERFLOW_CHECKING /= "OFF") then
                ASSERT FALSE
                REPORT "Illegal UNDERFLOW_CHECKING property value for DCFIFO!"
                SEVERITY ERROR;
            end if;
            if (OVERFLOW_CHECKING /= "ON" and OVERFLOW_CHECKING /= "OFF") then
                ASSERT FALSE
                REPORT "Illegal OVERFLOW_CHECKING property value for DCFIFO!"
                SEVERITY ERROR;
            end if;
            init := true;
        end if; -- not init
    end process;


    ----------
    -- FIFOram
    ----------

    i_rden <= rdreq when underflow_checking = "OFF" else
              rdreq and not i_rdempty;
    i_wren <= wrreq when overflow_checking = "OFF" else
              wrreq and not i_wrfull;

    FIFOram_sync: process (i_data_reg, i_q_tmp, i_q_reg, aclr,
                           i_rdptr, i_wren_reg, i_wrptr_reg)
    begin
        if (aclr = '1') then
            i_wrptr_tmp <= 0;
            i_rdptr_tmp <= 0;
            i_wren_tmp <= '0';
            i_data_tmp <= (OTHERS => '0');
            if (LPM_SHOWAHEAD = "ON") then
                i_q <= i_q_tmp;
            else
                i_q <= (OTHERS => '0');
            end if;
        else
            i_wrptr_tmp <= i_wrptr_reg;
            i_rdptr_tmp <= i_rdptr;
            i_wren_tmp <= i_wren_reg;
            i_data_tmp <= i_data_reg;
            if (LPM_SHOWAHEAD = "ON") then
                i_q <= i_q_tmp;
            else
                i_q <= i_q_reg;
            end if;
        end if;
    end process;

    FIFOram_wrclk: process (wrclk, aclr)
    begin
        if (aclr = '1') then
            i_data_reg <= (OTHERS => '0');
            i_wrptr_reg <= 0;
            i_wren_reg <= '0';
        elsif (wrclk'event and wrclk = '1' and NOW > 0 ns) then
            i_data_reg <= data;
            i_wrptr_reg <= i_wrptr;
            i_wren_reg <= i_wren;
        end if;
    end process;

    FIFOram_rdclk: process (rdclk, aclr)
    begin
        if (aclr = '1') then
            i_q_reg <= (OTHERS => '0');
        elsif (rdclk'event and rdclk = '1' and i_rden = '1' and NOW > 0 ns) then
            i_q_reg <= i_q_tmp;
        end if;
    end process;

    FIFOram_memory: process (i_data_tmp, i_wren_tmp, i_wrptr_tmp, i_rdptr_tmp, wrclk)
    variable mem_data : lpm_memory := (OTHERS => ZEROS);
    variable init : boolean := false;
    begin
        if (not init) then
            for i in lpm_numwords-1 downto 0 loop
                mem_data(i) := ZEROS;
            end loop;
            init := true;
        end if;
        if wrclk'event and i_wren_tmp='1' and
            ((wrclk='1' and USE_EAB="OFF") or
             (wrclk='0' and USE_EAB="ON")) then
            mem_data(i_wrptr_tmp) := i_data_tmp;
        end if;
        i_q_tmp <= mem_data(i_rdptr_tmp);
    end process;


    -----------
    -- Counters
    -----------

    rdptr: process (rdclk, aclr)
    begin
        if (aclr = '1') then
            i_rdptr <= 0;
        elsif (rdclk'event and rdclk = '1' and i_rden = '1' and NOW > 0 ns) then
            if (i_rdptr < 2**lpm_widthu-1) then
                i_rdptr <= i_rdptr + 1;
            else
                i_rdptr <= 0;
            end if;
        end if;
    end process;

    wrptr: process (wrclk, aclr)
    begin
        if (aclr = '1') then
            i_wrptr <= 0;
        elsif (wrclk'event and wrclk = '1' and i_wren = '1' and NOW > 0 ns) then
            if (i_wrptr < 2**lpm_widthu-1) then
                i_wrptr <= i_wrptr + 1;
            else
                i_wrptr <= 0;
            end if;
        end if;
    end process;


    ---------------------
    -- Delays & DFF Pipes
    ---------------------

    process (rdclk)
    begin
        if (rdclk = '0') then
            i_rdenclock <= '0';
        elsif (rdclk = '1' and i_rden = '1') then
            i_rdenclock <= '1';
        end if;
    end process;

    RDPTR_D:    DCFIFO_DFFPIPE
                    generic map (LPM_DELAY => 0)
                    port map (D => i_rdptr, Q => i_rdptrrg,
                              CLOCK => i_rdenclock, ACLR => aclr);

    WRPTR_D:    DCFIFO_DFFPIPE
                    generic map (LPM_DELAY => 1)
                    port map (D => i_wrptr, Q => i_wrdelaycycle,
                              CLOCK => wrclk, ACLR => aclr);

    WS_NBRP:    DCFIFO_DFFPIPE
                    generic map (LPM_DELAY => WRSYNC_DELAYPIPE)
                    port map (D => i_rdptrrg, Q => i_ws_nbrp,
                              CLOCK => wrclk, ACLR => aclr);

    RS_NBWP:    DCFIFO_DFFPIPE
                    generic map (LPM_DELAY => RDSYNC_DELAYPIPE)
                    port map (D => i_wrdelaycycle, Q => i_rs_nbwp,
                              CLOCK => rdclk, ACLR => aclr);

    WS_DBRP:    DCFIFO_DFFPIPE
                    generic map (LPM_DELAY => GRAY_DELAYPIPE)
                    port map (D => i_ws_nbrp, Q => i_ws_dbrp,
                              CLOCK => wrclk, ACLR => aclr);

    RS_DBWP:    DCFIFO_DFFPIPE
                    generic map (LPM_DELAY => GRAY_DELAYPIPE)
                    port map (D => i_rs_nbwp, Q => i_rs_dbwp,
                              CLOCK => rdclk, ACLR => aclr);

    process (i_wrptr, i_ws_dbrp)
    begin
        i_wr_udwn <= i_wrptr - i_ws_dbrp;
    end process;
            
    process (i_rdptr, i_rs_dbwp)
    begin
        i_rd_udwn <= i_rs_dbwp - i_rdptr;
    end process;
            
    WR_USEDW:   DCFIFO_DFFPIPE
                    generic map (LPM_DELAY => DELAY_WRUSEDW)
                    port map (D => i_wr_udwn, Q => i_wrusedw,
                              CLOCK => wrclk, ACLR => aclr);

    RD_USEDW:   DCFIFO_DFFPIPE
                    generic map (LPM_DELAY => DELAY_RDUSEDW)
                    port map (D => i_rd_udwn, Q => i_rdusedw,
                              CLOCK => rdclk, ACLR => aclr);

    WR_DBUW:    DCFIFO_DFFPIPE
                    generic map (LPM_DELAY => WRUSEDW_DELAYPIPE)
                    port map (D => i_wr_udwn, Q => i_wr_dbuw,
                              CLOCK => wrclk, ACLR => aclr);

    RD_DBUW:    DCFIFO_DFFPIPE
                    generic map (LPM_DELAY => RDUSEDW_DELAYPIPE)
                    port map (D => i_rd_udwn, Q => i_rd_dbuw,
                              CLOCK => rdclk, ACLR => aclr);


    -------------
    -- Full/Empty
    -------------

    slv_wr_dbuw <= conv_std_logic_vector(i_wr_dbuw, LPM_WIDTHU);
    slv_rd_dbuw <= conv_std_logic_vector(i_rd_dbuw, LPM_WIDTHU);

    WR_FE:  DCFIFO_FEFIFO
                generic map (LPM_WIDTHAD => LPM_WIDTHU,
                             LPM_NUMWORDS => LPM_NUMWORDS,
                             UNDERFLOW_CHECKING => UNDERFLOW_CHECKING,
                             OVERFLOW_CHECKING => OVERFLOW_CHECKING,
                             LPM_MODE => "WRITE")
                port map (USEDW_IN => slv_wr_dbuw, WREQ => wrreq,
                          CLOCK => wrclk, ACLR => aclr,
                          EMPTY => i_wrempty, FULL => i_wrfull);

    RD_FE:  DCFIFO_FEFIFO
                generic map (LPM_WIDTHAD => LPM_WIDTHU,
                             LPM_NUMWORDS => LPM_NUMWORDS,
                             UNDERFLOW_CHECKING => UNDERFLOW_CHECKING,
                             OVERFLOW_CHECKING => OVERFLOW_CHECKING,
                             LPM_MODE => "READ")
                port map (USEDW_IN => slv_rd_dbuw, RREQ => rdreq,
                          CLOCK => rdclk, ACLR => aclr,
                          EMPTY => i_rdempty, FULL => i_rdfull);

    ----------
    -- Outputs
    ----------

    q <= i_q;
    wrfull <= i_wrfull;
    wrempty <= i_wrempty;
    rdfull <= i_rdfull;
    rdempty <= i_rdempty;
    wrusedw <= conv_std_logic_vector(i_wrusedw, LPM_WIDTHU);
    rdusedw <= conv_std_logic_vector(i_rdusedw, LPM_WIDTHU);

end behavior;


----------------------------------------------------------------------------
-- dcfifo_sync (used by dcfifo)
----------------------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;

entity DCFIFO_SYNC is
    generic
      ( lpm_width               : natural;
        lpm_widthu              : natural;
        lpm_numwords            : natural;
        lpm_showahead           : string := "OFF";
        overflow_checking       : string := "ON";
        underflow_checking      : string := "ON";
        use_eab                 : string := "ON");
    port
      ( data    : in std_logic_vector(lpm_width-1 downto 0);
        rdclk   : in std_logic;
        wrclk   : in std_logic;
        wrreq   : in std_logic;
        rdreq   : in std_logic;
        aclr    : in std_logic := '0';
        rdfull  : out std_logic;
        wrfull  : out std_logic;
        wrempty : out std_logic;
        rdempty : out std_logic;
        q       : out std_logic_vector(lpm_width-1 downto 0);
        rdusedw : out std_logic_vector(lpm_widthu-1 downto 0);
        wrusedw : out std_logic_vector(lpm_widthu-1 downto 0));
end DCFIFO_SYNC;

architecture behavior of DCFIFO_SYNC is

type lpm_memory is array (2**lpm_widthu-1 downto 0) of std_logic_vector(lpm_width-1 downto 0);

signal i_q : std_logic_vector(lpm_width-1 downto 0) := (OTHERS => '0');
signal i_rdptr, i_wrptr : integer := 0;
signal i_rdptr_s, i_wrptr_r, i_wrptr_s : integer := 0;
signal i_rdempty, i_wrempty : std_logic := '1';
signal i_rdfull, i_wrfull : std_logic := '0';
signal i_rdusedw, i_wrusedw : integer := 0;
signal i_rden, i_wren : std_logic := '0';
signal i_cnt_mod : integer;

signal i_data_tmp, i_data_reg : std_logic_vector(lpm_width-1 downto 0);
signal i_q_tmp, i_q_reg : std_logic_vector(lpm_width-1 downto 0) := (OTHERS => '0');
signal i_rdptr_tmp, i_wrptr_tmp, i_wrptr_reg : integer := 0;
signal i_wren_tmp, i_wren_reg : std_logic := '0';

constant ZEROS : std_logic_vector(lpm_width-1 downto 0) := (OTHERS => '0');
constant GRAY_DELAYPIPE : integer := 1;
constant WRUSEDW_DELAYPIPE : integer := 1;  -- delayed usedw to compute empty/full
constant RDUSEDW_DELAYPIPE : integer := 1;  -- delayed usedw to compute empty/full

component DCFIFO_FEFIFO
    generic (LPM_WIDTHAD : natural;
             LPM_NUMWORDS : natural;
             UNDERFLOW_CHECKING : string := "ON";
             OVERFLOW_CHECKING : string := "ON";
             LPM_MODE : string);
    port (USEDW_IN : in std_logic_vector(LPM_WIDTHAD-1 downto 0);
          WREQ : in std_logic := 'Z';
          RREQ : in std_logic := 'Z';
          EMPTY : out std_logic;
          FULL : out std_logic;
          CLOCK : in std_logic;
          ACLR : in std_logic := '0');
end component;

component DCFIFO_DFFPIPE
    generic (LPM_DELAY : natural);
    port (D : in integer;
          Q : out integer;
          CLOCK : in std_logic;
          ACLR : in std_logic := '0');
end component;

begin

    -- Initialization
    process (wrclk, rdclk, aclr)
    variable init : boolean := false;
    begin
        if (not init) then
            if (LPM_SHOWAHEAD /= "ON" and LPM_SHOWAHEAD /= "OFF") then
                ASSERT FALSE
                REPORT "Illegal LPM_SHOWAHEAD property value for DCFIFO!"
                SEVERITY ERROR;
            end if;
            if (UNDERFLOW_CHECKING /= "ON" and UNDERFLOW_CHECKING /= "OFF") then
                ASSERT FALSE
                REPORT "Illegal UNDERFLOW_CHECKING property value for DCFIFO!"
                SEVERITY ERROR;
            end if;
            if (OVERFLOW_CHECKING /= "ON" and OVERFLOW_CHECKING /= "OFF") then
                ASSERT FALSE
                REPORT "Illegal OVERFLOW_CHECKING property value for DCFIFO!"
                SEVERITY ERROR;
            end if;
            if (LPM_NUMWORDS > 2**LPM_WIDTHU) then
                ASSERT FALSE
                REPORT "LPM_NUMWORDS must be less than or equal to 2**LPM_WIDTHU!"
                SEVERITY ERROR;
            end if;

            if lpm_numwords = 2**lpm_widthu then
                i_cnt_mod <= 2**(lpm_widthu+1);
            else
                i_cnt_mod <= 2**lpm_widthu;
            end if;

            init := true;
        end if; -- not init
    end process;


    ----------
    -- FIFOram
    ----------

    i_rden <= rdreq when (underflow_checking = "OFF") else
              rdreq and not i_rdempty;
    i_wren <= wrreq when (overflow_checking = "OFF") else
              wrreq and not i_wrfull;

    FIFOram_sync: process (i_data_reg, i_q_tmp, i_q_reg, aclr,
                           i_rdptr, i_wren_reg, i_wrptr_reg)
    begin
        if (aclr = '1') then
            i_wrptr_tmp <= 0;
            i_rdptr_tmp <= 0;
            i_wren_tmp <= '0';
            i_data_tmp <= (OTHERS => '0');
            if (LPM_SHOWAHEAD = "ON") then
                i_q <= i_q_tmp;
            else
                i_q <= (OTHERS => '0');
            end if;
        else
            i_wrptr_tmp <= i_wrptr_reg;
            i_rdptr_tmp <= i_rdptr;
            i_wren_tmp <= i_wren_reg;
            i_data_tmp <= i_data_reg;
            if (LPM_SHOWAHEAD = "ON") then
                i_q <= i_q_tmp;
            else
                i_q <= i_q_reg;
            end if;
        end if;
    end process;

    FIFOram_wrclk: process (wrclk, aclr)
    begin
        if (aclr = '1') then
            i_data_reg <= (OTHERS => '0');
            i_wrptr_reg <= 0;
            i_wren_reg <= '0';
        elsif (wrclk'event and wrclk = '1' and NOW > 0 ns) then
            i_data_reg <= data;
            i_wrptr_reg <= i_wrptr;
            i_wren_reg <= i_wren;
        end if;
    end process;

    FIFOram_rdclk: process (rdclk, aclr)
    begin
        if (aclr = '1') then
            i_q_reg <= (OTHERS => '0');
        elsif (rdclk'event and rdclk = '1' and i_rden = '1' and NOW > 0 ns) then
            i_q_reg <= i_q_tmp;
        end if;
    end process;

    FIFOram_memory: process (i_data_tmp, i_wren_tmp, i_wrptr_tmp, i_rdptr_tmp, wrclk)
    variable mem_data : lpm_memory := (OTHERS => ZEROS);
    variable init : boolean := false;
    begin
        if (not init) then
            for i in lpm_numwords-1 downto 0 loop
                mem_data(i) := ZEROS;
            end loop;
            init := true;
        end if;
        if wrclk'event and i_wren_tmp='1' and NOW > 0 ns and
            ((wrclk='1' and USE_EAB="OFF") or
             (wrclk='0' and USE_EAB="ON")) then
            mem_data(i_wrptr_tmp mod (2**lpm_widthu)) := i_data_tmp;
        end if;
        i_q_tmp <= mem_data(i_rdptr_tmp mod (2**lpm_widthu));
    end process;


    -----------
    -- Counters
    -----------

    rdptr: process (rdclk, aclr)
    begin
        if (aclr = '1') then
            i_rdptr <= 0;
        elsif (rdclk'event and rdclk = '1' and i_rden = '1' and NOW > 0 ns) then
            if (i_rdptr < i_cnt_mod-1) then
                i_rdptr <= i_rdptr + 1;
            else
                i_rdptr <= 0;
            end if;
        end if;
    end process;

    wrptr: process (wrclk, aclr)
    begin
        if (aclr = '1') then
            i_wrptr <= 0;
        elsif (wrclk'event and wrclk = '1' and i_wren = '1' and NOW > 0 ns) then
            if (i_wrptr < i_cnt_mod-1) then
                i_wrptr <= i_wrptr + 1;
            else
                i_wrptr <= 0;
            end if;
        end if;
    end process;


    ---------
    -- Delays
    ---------

    RDPTR_D:    DCFIFO_DFFPIPE
                    generic map (LPM_DELAY => 1)
                    port map (D => i_rdptr, Q => i_rdptr_s,
                              CLOCK => wrclk, ACLR => aclr);

    WRPTR_D:    DCFIFO_DFFPIPE
                    generic map (LPM_DELAY => 1)
                    port map (D => i_wrptr, Q => i_wrptr_r,
                              CLOCK => wrclk, ACLR => aclr);

    WRPTR_E:    DCFIFO_DFFPIPE
                    generic map (LPM_DELAY => 1)
                    port map (D => i_wrptr_r, Q => i_wrptr_s,
                              CLOCK => rdclk, ACLR => aclr);


    -------------------
    -- Usedw/Full/Empty
    -------------------

    process (i_wrptr, i_rdptr_s)
    begin        
        i_wrusedw <= (i_wrptr - i_rdptr_s) mod i_cnt_mod;
    end process;
            
    process (i_rdptr, i_wrptr_s)
    begin
        i_rdusedw <= (i_wrptr_s - i_rdptr) mod i_cnt_mod;
    end process;

    process (i_wrusedw)
    begin
        i_wrfull <= '0';
        i_wrempty <= '0';

        if ((lpm_numwords = 2**lpm_widthu) and (i_wrusedw >= 2**lpm_widthu)) or
           ((lpm_numwords < 2**lpm_widthu) and (i_wrusedw = lpm_numwords)) then
            i_wrfull <= '1';
        end if;
        if (i_wrusedw = 0) then
            i_wrempty <= '1';
        end if;
    end process;

    process (i_rdusedw)
    begin
        i_rdfull <= '0';
        i_rdempty <= '0';

        if ((lpm_numwords = 2**lpm_widthu) and (i_rdusedw >= 2**lpm_widthu)) or
           ((lpm_numwords < 2**lpm_widthu) and (i_rdusedw = lpm_numwords)) then
            i_rdfull <= '1';
        end if;
        if (i_rdusedw = 0) then
            i_rdempty <= '1';
        end if;
    end process;


    ----------
    -- Outputs
    ----------

    q <= i_q;
    wrfull <= i_wrfull;
    wrempty <= i_wrempty;
    rdfull <= i_rdfull;
    rdempty <= i_rdempty;
    wrusedw <= conv_std_logic_vector(i_wrusedw, LPM_WIDTHU);
    rdusedw <= conv_std_logic_vector(i_rdusedw, LPM_WIDTHU);

end behavior;


----------------------------------------------------------------------------
-- dcfifo megafunction
----------------------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.std_logic_unsigned.all;

entity DCFIFO is
    generic
      ( lpm_width               : natural;
        lpm_widthu              : natural;
        lpm_numwords            : natural;
        lpm_showahead           : string := "OFF";
        lpm_hint                : string := "USE_EAB=ON";
        overflow_checking       : string := "ON";
        underflow_checking      : string := "ON";
        delay_rdusedw           : natural := 1;
        delay_wrusedw           : natural := 1;
        rdsync_delaypipe        : natural := 3;
        wrsync_delaypipe        : natural := 3;
        -- parameters that should be stored in lpm_hint
        use_eab                 : string := "ON";
        clocks_are_synchronized : string := "FALSE");
    port
      ( data    : in std_logic_vector(lpm_width-1 downto 0);
        rdclk   : in std_logic;
        wrclk   : in std_logic;
        wrreq   : in std_logic;
        rdreq   : in std_logic;
        aclr    : in std_logic := '0';
        rdfull  : out std_logic;
        wrfull  : out std_logic;
        wrempty : out std_logic;
        rdempty : out std_logic;
        q       : out std_logic_vector(lpm_width-1 downto 0);
        rdusedw : out std_logic_vector(lpm_widthu-1 downto 0);
        wrusedw : out std_logic_vector(lpm_widthu-1 downto 0));
end DCFIFO; 

architecture behavior of DCFIFO is

signal i_q_s, i_q_a : std_logic_vector(lpm_width-1 downto 0);
signal i_wrusedw_s, i_wrusedw_a : std_logic_vector(lpm_widthu-1 downto 0);
signal i_rdusedw_s, i_rdusedw_a : std_logic_vector(lpm_widthu-1 downto 0);
signal i_wrfull_s, i_wrfull_a, i_rdfull_s, i_rdfull_a : std_logic;
signal i_wrempty_s, i_wrempty_a, i_rdempty_s, i_rdempty_a : std_logic;

component DCFIFO_ASYNC
    generic
      ( lpm_width               : natural;
        lpm_widthu              : natural;
        lpm_numwords            : natural;
        lpm_showahead           : string := "OFF";
        overflow_checking       : string := "ON";
        underflow_checking      : string := "ON";
        delay_rdusedw           : natural := 1;
        delay_wrusedw           : natural := 1;
        rdsync_delaypipe        : natural := 3;
        wrsync_delaypipe        : natural := 3;
        use_eab                 : string := "ON");
    port
      ( data    : in std_logic_vector(lpm_width-1 downto 0);
        rdclk   : in std_logic;
        wrclk   : in std_logic;
        wrreq   : in std_logic;
        rdreq   : in std_logic;
        aclr    : in std_logic := '0';
        rdfull  : out std_logic;
        wrfull  : out std_logic;
        wrempty : out std_logic;
        rdempty : out std_logic;
        q       : out std_logic_vector(lpm_width-1 downto 0);
        rdusedw : out std_logic_vector(lpm_widthu-1 downto 0);
        wrusedw : out std_logic_vector(lpm_widthu-1 downto 0));
end component;

component DCFIFO_SYNC
    generic
      ( lpm_width               : natural;
        lpm_widthu              : natural;
        lpm_numwords            : natural;
        lpm_showahead           : string := "OFF";
        overflow_checking       : string := "ON";
        underflow_checking      : string := "ON";
        use_eab                 : string := "ON");
    port
      ( data    : in std_logic_vector(lpm_width-1 downto 0);
        rdclk   : in std_logic;
        wrclk   : in std_logic;
        wrreq   : in std_logic;
        rdreq   : in std_logic;
        aclr    : in std_logic := '0';
        rdfull  : out std_logic;
        wrfull  : out std_logic;
        wrempty : out std_logic;
        rdempty : out std_logic;
        q       : out std_logic_vector(lpm_width-1 downto 0);
        rdusedw : out std_logic_vector(lpm_widthu-1 downto 0);
        wrusedw : out std_logic_vector(lpm_widthu-1 downto 0));
end component;

begin

    ASYNC:  DCFIFO_ASYNC
                generic map (lpm_width => lpm_width,
                             lpm_widthu => lpm_widthu,
                             lpm_numwords => lpm_numwords,
                             lpm_showahead => lpm_showahead,
                             overflow_checking => overflow_checking,
                             underflow_checking => underflow_checking,
                             delay_rdusedw => delay_rdusedw,
                             delay_wrusedw => delay_wrusedw,
                             rdsync_delaypipe => rdsync_delaypipe,
                             wrsync_delaypipe => wrsync_delaypipe,
                             use_eab => use_eab)
                port map (data => data, rdclk => rdclk, wrclk => wrclk,
                          wrreq => wrreq, rdreq => rdreq, aclr => aclr,
                          rdfull => i_rdfull_a, wrfull => i_wrfull_a,
                          rdempty => i_rdempty_a, wrempty => i_wrempty_a,
                          rdusedw => i_rdusedw_a, wrusedw => i_wrusedw_a,
                          q => i_q_a);

    SYNC:   DCFIFO_SYNC
                generic map (lpm_width => lpm_width,
                             lpm_widthu => lpm_widthu,
                             lpm_numwords => lpm_numwords,
                             lpm_showahead => lpm_showahead,
                             overflow_checking => overflow_checking,
                             underflow_checking => underflow_checking,
                             use_eab => use_eab)
                port map (data => data, rdclk => rdclk, wrclk => wrclk,
                          wrreq => wrreq, rdreq => rdreq, aclr => aclr,
                          rdfull => i_rdfull_s, wrfull => i_wrfull_s,
                          rdempty => i_rdempty_s, wrempty => i_wrempty_s,
                          rdusedw => i_rdusedw_s, wrusedw => i_wrusedw_s,
                          q => i_q_s);

    rdfull <= i_rdfull_a when clocks_are_synchronized = "FALSE" else
              i_rdfull_s;
    wrfull <= i_wrfull_a when clocks_are_synchronized = "FALSE" else
              i_wrfull_s;
    rdempty <= i_rdempty_a when clocks_are_synchronized = "FALSE" else
               i_rdempty_s;
    wrempty <= i_wrempty_a when clocks_are_synchronized = "FALSE" else
               i_wrempty_s;
    rdusedw <= i_rdusedw_a when clocks_are_synchronized = "FALSE" else
               i_rdusedw_s;
    wrusedw <= i_wrusedw_a when clocks_are_synchronized = "FALSE" else
               i_wrusedw_s;
    q <= i_q_a when clocks_are_synchronized = "FALSE" else
         i_q_s;

end behavior;





--
-- Excalibur DPRAM atom.
--

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;
use IEEE.std_logic_textio.all;
use std.textio.all;

entity alt_exc_dpram is
    generic 
      ( width          : integer;
        addrwidth      : integer;
        depth          : integer;
        ramblock       : integer := 65535;
        operation_mode : string := "SINGLE_PORT";
        output_mode    : string := "REG";
        lpm_file       : string := "NONE");

    port
      ( portaclk : in std_logic := '0';
        portaena : in std_logic := '0';
        portawe : in std_logic := '0';
        portaaddr : in std_logic_vector(addrwidth-1 downto 0) := (others =>'0');
        portadatain : in std_logic_vector(width-1 downto 0) := (others =>'0');
        portadataout : out std_logic_vector(width-1 downto 0);
        portbclk : in std_logic := '0';
        portbena : in std_logic := '0';
        portbwe : in std_logic := '0';
        portbaddr : in std_logic_vector(addrwidth-1 downto 0) := (others =>'0');
        portbdatain : in std_logic_vector(width-1 downto 0) := (others =>'0');
        portbdataout : out std_logic_vector(width-1 downto 0));

end alt_exc_dpram;


architecture behaviour of alt_exc_dpram is

	--
	-- Hex Read procedure.
	--
	procedure local_Char2QuadBits(C: Character; 
				RESULT: out Bit_Vector(3 downto 0);
				GOOD: out Boolean;
				ISSUE_ERROR: in Boolean) is
	begin
		case c is
			when '0' => result :=  x"0"; good := TRUE;
			when '1' => result :=  x"1"; good := TRUE;
			when '2' => result :=  x"2"; good := TRUE;
			when '3' => result :=  x"3"; good := TRUE;
			when '4' => result :=  x"4"; good := TRUE;
			when '5' => result :=  x"5"; good := TRUE;
			when '6' => result :=  x"6"; good := TRUE;
			when '7' => result :=  x"7"; good := TRUE;
			when '8' => result :=  x"8"; good := TRUE;
			when '9' => result :=  x"9"; good := TRUE;
			when 'A' => result :=  x"A"; good := TRUE;
			when 'B' => result :=  x"B"; good := TRUE;
			when 'C' => result :=  x"C"; good := TRUE;
			when 'D' => result :=  x"D"; good := TRUE;
			when 'E' => result :=  x"E"; good := TRUE;
			when 'F' => result :=  x"F"; good := TRUE;
 
			when 'a' => result :=  x"A"; good := TRUE;
			when 'b' => result :=  x"B"; good := TRUE;
			when 'c' => result :=  x"C"; good := TRUE;
			when 'd' => result :=  x"D"; good := TRUE;
			when 'e' => result :=  x"E"; good := TRUE;
			when 'f' => result :=  x"F"; good := TRUE;
			when others =>
			   if ISSUE_ERROR then 
				   assert FALSE report
					"HREAD Error: Read a '" & c &
					   "', expected a Hex character (0-F).";
			   end if;
			   good := FALSE;
		end case;
	end;

	procedure HREAD_BV(L:inout LINE; VALUE:out BIT_VECTOR)  is
		variable ok: boolean;
		variable c:  character;
		constant ne: integer := value'length/4;
		variable bv: bit_vector(0 to value'length-1);
		variable s:  string(1 to ne-1);
	begin
		if value'length mod 4 /= 0 then
			assert FALSE report 
				"HREAD_BV Error: Trying to read vector " &
				   "with an odd (non multiple of 4) length";
			return;
		end if;

		loop					-- skip white space
			read(l,c);
			exit when ((c /= ' ') and (c /= CR) and (c /= HT));
		end loop;

		local_Char2QuadBits(c, bv(0 to 3), ok, TRUE);
		if not ok then 
			return;
		end if;

		read(L, s, ok);
		if not ok then
			assert FALSE 
				report "HREAD_bv Error: Failed to read the STRING";
			return;
		end if;

		for i in 1 to ne-1 loop
			local_Char2QuadBits(s(i), bv(4*i to 4*i+3), ok, TRUE);
			if not ok then
				return;
			end if;
		end loop;
		value := bv;
	end HREAD_bv; 

type dpram_memory is array (depth-1 downto 0) of std_logic_vector(width-1 downto 0);


        -------------------------
        function dpram_loadmem return dpram_memory is
            variable result           : dpram_memory := (others=>(others=>'X'));
            --file     memory_file      : TEXT is in lpm_file            ;
            file     memory_file      : TEXT OPEN READ_MODE is lpm_file;
            variable data_line        : LINE                             ;
            variable memory_bitvector : bit_vector (width-1 downto 0)    ; 
            variable line_counter     : INTEGER := 0                     ; 

        begin
            read_memory : while not ENDFILE(memory_file)
            loop
                if ( line_counter <= depth-1 ) 
                then 
                    READLINE (memory_file, data_line) ;
                    if ( data_line'LENGTH = 0 ) then 
                        next ;
                    elsif ( data_line (1) = '/' )  then 
                        next ;
                    end if ;
                    HREAD_bv (data_line , memory_bitvector ) ;
                    result (line_counter) := To_StdLogicVector (memory_bitvector) ;
                    line_counter := line_counter + 1 ; 
                end if ; 
            end loop ; 
            return result;
        end dpram_loadmem;
        -------------------------

        -------------------------
        function dpram_checkmem return dpram_memory is
            variable result : dpram_memory := (others=>(others=>'X'));
        begin

            if (lpm_file /= "NONE" and lpm_file /= "none") 
            then 
                result := dpram_loadmem ;
            end if ;

            return result;

        end dpram_checkmem;
        -------------------------


signal portadataout_reg   : std_logic_vector(width-1 downto 0);
signal portbdataout_reg   : std_logic_vector(width-1 downto 0);
signal portadataout_reg_out : std_logic_vector(width-1 downto 0);
signal portbdataout_reg_out : std_logic_vector(width-1 downto 0);
signal portadataout_unreg : std_logic_vector(width-1 downto 0);
signal portbdataout_unreg : std_logic_vector(width-1 downto 0);

signal portaclk_ipd       : std_logic;
signal portbclk_ipd       : std_logic;
signal portawe_ipd        : std_logic;
signal portbwe_ipd        : std_logic;
signal portaena_ipd       : std_logic;
signal portbena_ipd       : std_logic;
signal portadatain_ipd    : std_logic_vector(width-1   downto 0);
signal portbdatain_ipd    : std_logic_vector(width-1   downto 0);
signal portaaddr_ipd      : std_logic_vector(addrwidth-1 downto 0);
signal portbaddr_ipd      : std_logic_vector(addrwidth-1 downto 0);

signal portadataout_tmp   : std_logic_vector(width-1   downto 0);
signal portbdataout_tmp   : std_logic_vector(width-1   downto 0);



begin

  portaclk_ipd  <= portaclk;
  portawe_ipd   <= portawe;
  portaena_ipd  <= portaena;
  portadatain_ipd  <= portadatain;

  portbclk_ipd  <= portbclk;
  portbwe_ipd   <= portbwe;
  portbena_ipd  <= portbena;

  portadatain_ipd  <= portadatain;
  portbdatain_ipd  <= portbdatain;
    
  portaaddr_ipd  <= portaaddr;
  portbaddr_ipd  <= portbaddr;

  portadataout   <= portadataout_tmp;
  portbdataout   <= portbdataout_tmp;   


CLOCK: process(portaclk_ipd, portbclk_ipd)
  variable dpram_content                    : dpram_memory        := dpram_checkmem ;

  variable portawe_latched : std_ulogic;
  variable portbwe_latched : std_ulogic;

  variable address_A : integer;
  variable address_B : integer;
  variable valid_addr_A    : boolean:=FALSE;
  variable valid_addr_B    : boolean:=FALSE;
  
begin

--  Dual Port Contention  Port A address = Port B address
--
--  +-----------+----------+-------------+-------------+--------------+--------------+---------------------+
--  |  Port A   |  Port B  |  A Data In  |  B Data In  |  A Data Out  |  B Data Out  |     Memory State    |
--  +-----------+----------+-------------+-------------+--------------+--------------+---------------------+
--  |   read    |   read   |     DA      |     DB      |    memory    |    memory    |      no change      |
--  +-----------+----------+-------------+-------------+--------------+--------------+---------------------+
--  |   write   |   read   |     DA      |     DB      |    unknown   |    unknown   |    memory <= DA     |
--  +-----------+----------+-------------+-------------+--------------+--------------+---------------------+
--  |   read    |   write  |     DA      |     DB      |    unknown   |    unknown   |    memory <= DB     |
--  +-----------+----------+-------------+-------------+--------------+--------------+---------------------+
--  |   write   |   write  |     DA      |     DB      |    unknown   |    unknown   |  memory <= unknown  |
--  +-----------+----------+-------------+-------------+--------------+--------------+---------------------+
--
--  Dual Port Contention  Port A address != Port B address
--
--  +-----------+----------+-------------+-------------+--------------+--------------+---------------------+
--  |  Port A   |  Port B  |  A Data In  |  B Data In  |  A Data Out  |  B Data Out  |     Memory State    |
--  +-----------+----------+-------------+-------------+--------------+--------------+---------------------+
--  |   read    |   read   |     DA      |     DB      |  mem[A_addr] |  mem[B_Addr] |      no change      |
--  +-----------+----------+-------------+-------------+--------------+--------------+---------------------+
--  |   write   |   read   |     DA      |     DB      |    unknown   |  mem[B_Addr] |  mem[A_Addr] <= DA  |
--  +-----------+----------+-------------+-------------+--------------+--------------+---------------------+
--  |   read    |   write  |     DA      |     DB      |  mem[A_addr] |    unknown   |  mem[B_Addr] <= DB  |
--  +-----------+----------+-------------+-------------+--------------+--------------+---------------------+
--  |   write   |   write  |     DA      |     DB      |    unknown   |    unknown   |  mem[A_Addr] <= DA  |
--  |           |          |             |             |              |              |  mem[B_Addr] <= DB  |
--  +-----------+----------+-------------+-------------+--------------+--------------+---------------------+
-- 
-- NB: output is always unknown when writing



  if(portaclk_ipd'event and portaclk_ipd='1') then  -- rising edge port a clock
  
    portawe_latched := portawe_ipd;

    valid_addr_A := not Is_X(portaaddr_ipd(addrwidth-1 downto 0));

    if ( valid_addr_A ) then 

      address_A       := conv_integer(portaaddr_ipd);

         if portawe_latched ='0' 
         then
	     -- reading from A
             if valid_addr_B and address_A = address_B and portbwe_latched = '1' 
             then
                 -- B simultaneously writing to same address (effect of B write to memory handled below)
                 portadataout_reg         <= portadataout_unreg;
                 portadataout_unreg       <= (others => 'X')   ;
             else
                 -- B reading from same address, or reading/writing to different address. 
                 portadataout_reg   <= portadataout_unreg      ;
                 portadataout_unreg <= dpram_content(address_A);
             end if;
         else
             -- writing to A
             if valid_addr_B and address_A = address_B and portawe_latched = '1' and portbwe_latched = '1' 
             then
                 -- A and B simultaneously writing to same address
                 portadataout_reg         <= portadataout_unreg;
                 dpram_content(address_A) := (others => 'X')   ;
                 portadataout_unreg       <= (others => 'X')   ;
             else
                 -- B reading from same address or reading/writing to different address
                 portadataout_reg         <= portadataout_unreg;
                 dpram_content(address_A) := portadatain_ipd   ;
                 portadataout_unreg       <= (others => 'X')   ;
             end if;
         end if;

    else 
        portadataout_reg    <= portadataout_unreg;
        address_A           := 0 ; 
        portadataout_unreg  <= (others => 'X')   ;
    end if ; 

  end if ; 
    
  if (portbclk_ipd'event and portbclk_ipd = '1') then  -- rising edge port b clock

    portbwe_latched := portbwe_ipd;

    valid_addr_B := not Is_X(portbaddr_ipd(addrwidth-1 downto 0));

    if ( valid_addr_B ) then 

       address_B := conv_integer(portbaddr_ipd);
 
         if portbwe_latched ='0' 
         then
	     -- reading from B
             if valid_addr_A and address_B = address_A and portawe_latched = '1' 
             then
                 -- A simultaneously writing to same address (effect of A write to memory handled above)
                 portbdataout_reg         <= portbdataout_unreg;
                 portbdataout_unreg       <= (others => 'X')   ;
             else
                 -- A reading from same address, or reading/writing to different address. 
                 portbdataout_reg   <= portbdataout_unreg      ;
                 portbdataout_unreg <= dpram_content(address_B);
             end if;
         else
             -- writing to B
             if valid_addr_A and address_B = address_A and portbwe_latched = '1' and portawe_latched = '1' 
             then
                 -- B and A simultaneously writing to same address
                 portbdataout_reg         <= portbdataout_unreg;
                 dpram_content(address_B) := (others => 'X')   ;
                 portbdataout_unreg       <= (others => 'X')   ;
             else
                 -- A reading from same address or reading/writing to different address
                 portbdataout_reg         <= portbdataout_unreg;
                 dpram_content(address_B) := portbdatain_ipd   ;
                 portbdataout_unreg       <= (others => 'X')   ;
             end if;
         end if;

      else 
        portbdataout_reg   <= portbdataout_unreg;
        address_B          := 0 ; 
        portbdataout_unreg <= (others => 'X')   ;
       end if ; 

      end if;
  end process CLOCK;


  OUTPUT_ENABLE_A : process(portaena_ipd, portadataout_reg)
  begin
      if((output_mode = "REG" or output_mode = "reg") and portaena_ipd = '1') then
          portadataout_reg_out <= portadataout_reg ;  
      end if;
  end process OUTPUT_ENABLE_A;

  OUTPUT_ENABLE_B : process(portbena_ipd, portbdataout_reg)
  begin
      if((output_mode = "REG" or output_mode = "reg") and portbena_ipd = '1') then
          portbdataout_reg_out <= portbdataout_reg ;  
      end if;
  end process OUTPUT_ENABLE_B;


REG_UNREG_SEL_A : process(portadataout_reg_out, portadataout_unreg)
begin
  if(output_mode = "UNREG" or output_mode = "unreg") then
    portadataout_tmp <= portadataout_unreg;
  else
    portadataout_tmp <= portadataout_reg_out;
  end if;
end process REG_UNREG_SEL_A;


REG_UNREG_SEL_B : process(portbdataout_reg_out, portbdataout_unreg)
begin
  if(output_mode = "UNREG" or output_mode = "unreg") then
    portbdataout_tmp <= portbdataout_unreg;
  else
    portbdataout_tmp <= portbdataout_reg_out;
  end if;
end process REG_UNREG_SEL_B;

end behaviour;



--
-- Excalibur UPCORE atom.
--

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;
use IEEE.std_logic_textio.all;
use IEEE.std_logic_unsigned.all;
use std.textio.all;


entity alt_exc_upcore is
    generic
      ( processor      : string := "ARM";
        source         : string := "";
        sdram_width    : integer := 32;
        sdramdqm_width : integer := 4;
        gpio_width     : integer := 4);

    port
      ( npor           : in    std_logic                     := '1';
        clk_ref        : in    std_logic                     := '0';
        nreset         : inout std_logic                     := '1';

        intpld         : in  std_logic_vector(5 downto 0)  := (others => '0');
        intnmi         : in  std_logic                     := '0';
        intuart        : out std_logic;
        inttimer0      : out std_logic;
        inttimer1      : out std_logic;
        intcommtx      : out std_logic;
        intcommrx      : out std_logic;
        intproctimer   : out std_logic;
        intprocbridge  : out std_logic;
        perreset       : out std_logic;

        debugrq      : in  std_logic := '0';
        debugext0    : in  std_logic := '0';
        debugext1    : in  std_logic := '0';
        debugiebrkpt : in  std_logic := '0';
        debugdewpt   : in  std_logic := '0';
        debugextin   : in  std_logic_vector(3 downto 0) := (others => '0');
        debugack     : out std_logic;
        debugrng0    : out std_logic;
        debugrng1    : out std_logic;
        debugextout  : out std_logic_vector(3 downto 0);

        slavehclk      : in  std_logic := '0';
        slavehwrite    : in  std_logic := '0';
        slavehreadyi   : in  std_logic := '0';
        slavehselreg   : in  std_logic := '0';
        slavehsel      : in  std_logic := '0';
        slavehmastlock : in  std_logic := '0';
        slavehaddr     : in  std_logic_vector(31 downto 0) := (others => '0');
        slavehwdata    : in  std_logic_vector(31 downto 0) := (others => '0');
        slavehtrans    : in  std_logic_vector(1 downto 0) := (others => '0');
        slavehsize     : in  std_logic_vector(1 downto 0) := (others => '0');
        slavehburst    : in  std_logic_vector(2 downto 0) := (others => '0');
        slavehreadyo   : out std_logic;
        slavebuserrint : out std_logic;
        slavehrdata    : out std_logic_vector(31 downto 0);
        slavehresp     : out std_logic_vector(1 downto 0);

        masterhclk     : in  std_logic := '0';
        masterhrdata   : in  std_logic_vector(31 downto 0) := (others => '0');
        masterhresp    : in  std_logic_vector(1 downto 0) := (others => '0');
        masterhwrite   : out std_logic;
        masterhlock    : out std_logic;
        masterhbusreq  : out std_logic;
        masterhaddr    : out std_logic_vector(31 downto 0);
        masterhwdata   : out std_logic_vector(31 downto 0);
        masterhtrans   : out std_logic_vector(1 downto 0);
        masterhsize    : out std_logic_vector(1 downto 0);
        masterhready   : in  std_logic := '0';
        masterhburst   : out std_logic_vector(2 downto 0);
        masterhgrant   : in  std_logic := '0';

        lockreqdp0   : in  std_logic := '0';
        lockreqdp1   : in  std_logic := '0';
        lockgrantdp0 : out std_logic;
        lockgrantdp1 : out std_logic;

        ebiack  : in  std_logic := '0';
        ebiwen  : out std_logic;
        ebioen  : out std_logic;
        ebiclk  : out std_logic;
        ebibe   : out std_logic_vector(1 downto 0);
        ebicsn  : out std_logic_vector(3 downto 0);
        ebiaddr : out std_logic_vector(24 downto 0);

        ebidq : inout std_logic_vector(15 downto 0) := (others => '0');

        uarttxd  : out   std_logic;
        uartrtsn : out   std_logic;
        uartdtrn : out   std_logic;
        uartctsn : in    std_logic := '0';
        uartdsrn : in    std_logic := '0';
        uartrxd  : in    std_logic := '0';
        uartdcdn : inout std_logic := '0';
        uartrin  : inout std_logic := '0';

        sdramclk  : out std_logic;
        sdramclkn : out std_logic;
        sdramclke : out std_logic;
        sdramwen  : out std_logic;
        sdramcasn : out std_logic;
        sdramrasn : out std_logic;
        sdramdqm  : out std_logic_vector(sdramdqm_width-1 downto 0);
        sdramaddr : out std_logic_vector(14 downto 0);
        sdramcsn  : out std_logic_vector(1 downto 0);

        sdramdq  : inout std_logic_vector(sdram_width-1 downto 0) := (others => '0');
        sdramdqs : inout std_logic_vector(sdramdqm_width-1 downto 0) := (others => '0');

        intextpin     : in  std_logic := '0';
        traceclk      : out std_logic;
        tracesync     : out std_logic;
        tracepipestat : out std_logic_vector(2 downto 0);
        tracepkt      : out std_logic_vector(15 downto 0);

        gpi           : in  std_logic_vector(gpio_width-1 downto 0) := (others => '0');
        gpo           : out std_logic_vector(gpio_width-1 downto 0));
    

end alt_exc_upcore;


architecture behaviour of alt_exc_upcore is

	--
	-- Hex Read and Write procedures.
	--
	procedure local_Char2QuadBits(C: Character; 
				RESULT: out Bit_Vector(3 downto 0);
				GOOD: out Boolean;
				ISSUE_ERROR: in Boolean) is
	begin
		case c is
			when '0' => result :=  x"0"; good := TRUE;
			when '1' => result :=  x"1"; good := TRUE;
			when '2' => result :=  x"2"; good := TRUE;
			when '3' => result :=  x"3"; good := TRUE;
			when '4' => result :=  x"4"; good := TRUE;
			when '5' => result :=  x"5"; good := TRUE;
			when '6' => result :=  x"6"; good := TRUE;
			when '7' => result :=  x"7"; good := TRUE;
			when '8' => result :=  x"8"; good := TRUE;
			when '9' => result :=  x"9"; good := TRUE;
			when 'A' => result :=  x"A"; good := TRUE;
			when 'B' => result :=  x"B"; good := TRUE;
			when 'C' => result :=  x"C"; good := TRUE;
			when 'D' => result :=  x"D"; good := TRUE;
			when 'E' => result :=  x"E"; good := TRUE;
			when 'F' => result :=  x"F"; good := TRUE;
 
			when 'a' => result :=  x"A"; good := TRUE;
			when 'b' => result :=  x"B"; good := TRUE;
			when 'c' => result :=  x"C"; good := TRUE;
			when 'd' => result :=  x"D"; good := TRUE;
			when 'e' => result :=  x"E"; good := TRUE;
			when 'f' => result :=  x"F"; good := TRUE;
			when others =>
			   if ISSUE_ERROR then 
				   assert FALSE report
					"HREAD Error: Read a '" & c &
					   "', expected a Hex character (0-F).";
			   end if;
			   good := FALSE;
		end case;
	end;

	procedure HREAD_BV(L:inout LINE; VALUE:out BIT_VECTOR)  is
		variable ok: boolean;
		variable c:  character;
		constant ne: integer := value'length/4;
		variable bv: bit_vector(0 to value'length-1);
		variable s:  string(1 to ne-1);
	begin
		if value'length mod 4 /= 0 then
			assert FALSE report 
				"HREAD_BV Error: Trying to read vector " &
				   "with an odd (non multiple of 4) length";
			return;
		end if;

		loop					-- skip white space
			read(l,c);
			exit when ((c /= ' ') and (c /= CR) and (c /= HT));
		end loop;

		local_Char2QuadBits(c, bv(0 to 3), ok, TRUE);
		if not ok then 
			return;
		end if;

		read(L, s, ok);
		if not ok then
			assert FALSE 
				report "HREAD_bv Error: Failed to read the STRING";
			return;
		end if;

		for i in 1 to ne-1 loop
			local_Char2QuadBits(s(i), bv(4*i to 4*i+3), ok, TRUE);
			if not ok then
				return;
			end if;
		end loop;
		value := bv;
	end HREAD_bv; 


     -- functions to match Verilog functions and operators

     -- Bitwise left shift
        function shift_left ( val : std_logic_vector ) return std_logic_vector is
            variable result : std_logic_vector(val'length-1 downto 0);
        begin
            result:=val;
            if (val'length>1) then
                for i in (val'length-1) downto 1 loop
                    result(i):=result(i-1);
                end loop;
            end if;
            result(0) := '0';
            return result;
        end shift_left;
        function shift_left ( val : std_logic_vector; n : integer ) return std_logic_vector is
            variable result : std_logic_vector(val'length-1 downto 0);
        begin
            result:=val;
            for i in 1 to n loop
                result:=shift_left(result);
            end loop;
            return result;
        end shift_left;
	

     -- Unary reduction OR : use ur_OR(foo) as a replacement for Verilog (|foo).
        function ur_OR (val : std_logic_vector ) return std_logic is
            variable result : std_logic ;
        begin
            if (val'length > 0) then
                result := val(val'length-1) ;
                for i in (val'length-2) downto 0 loop
                    result := (result or val(i));
                end loop;
            else
                result := '0';
            end if;
            return result;
        end ur_OR;

     -- Unary reduction XOR : use ur_XOR(foo) as a replacement for Verilog (^foo).
        function ur_XOR (val : std_logic_vector ) return std_logic is
            variable result : std_logic := '0' ;
            variable count  : integer   :=  0  ;
        begin
            if (val'length > 0) then
                for i in (val'length-1) downto 0 loop
                    if (result /= 'X') then
                        if    (val(i) = '1') then count  := count + 1 ;
                        elsif (val(i) = 'X') then result := 'X'  ;
                        end if ; 
                    end if ; 
                end loop;
            else
                result := '0';
            end if;
            if ( result = 'X') then 
                return result ;
            elsif (count > 1 ) then 
                return '0' ;
            else
                return '1' ;
            end if ;
        end ur_XOR;


     -- Components



     -- +---------------+
     -- | AHB Constants |
     -- +---------------+

     -- // number of memory banks   
        constant  NUMBANKS           : integer := 6 ;
	
     -- // respones (HRESP)
        constant  H_OKAY   : std_logic_vector(1 downto 0) := "00" ;
        constant  H_ERROR  : std_logic_vector(1 downto 0) := "01" ;
        constant  H_RETRY  : std_logic_vector(1 downto 0) := "10" ;
        constant  H_SPLIT  : std_logic_vector(1 downto 0) := "11" ;
	
     -- // transaction types (HTRANS)
        constant  H_IDLE   : std_logic_vector(1 downto 0) := "00" ;
        constant  H_BUSY   : std_logic_vector(1 downto 0) := "01" ;
        constant  H_NONSEQ : std_logic_vector(1 downto 0) := "10" ;
        constant  H_SEQ    : std_logic_vector(1 downto 0) := "11" ;
	
     -- // burst mode (HBURST)
        constant  H_SINGLE : std_logic_vector(2 downto 0) := "000";
        constant  H_INCR   : std_logic_vector(2 downto 0) := "001";
        constant  H_WRAP4  : std_logic_vector(2 downto 0) := "010";
        constant  H_INCR4  : std_logic_vector(2 downto 0) := "011";
        constant  H_WRAP8  : std_logic_vector(2 downto 0) := "100";
        constant  H_INCR8  : std_logic_vector(2 downto 0) := "101";
        constant  H_WRAP16 : std_logic_vector(2 downto 0) := "110";
        constant  H_INCR16 : std_logic_vector(2 downto 0) := "111";
	
     -- // transaction sizes (HSIZE 8,16,32 bits -- larger sizes not supported)
        constant  H_BYTE   : std_logic_vector(1 downto 0) := "00" ;
        constant  H_HWORD  : std_logic_vector(1 downto 0) := "01" ;
        constant  H_WORD   : std_logic_vector(1 downto 0) := "10" ;

     -- // maximum number of transmissions   
        constant  NUMTRANS           : integer :=   2048       ; -- was 65535, but may lead to memory allocation problems

     -- // data sub-ranges within transmission
        constant  Trns_b             : integer :=    255       ; 
        --                                           255..254  ; -- spare
        constant  Trns_RESP_b        : integer :=    253       ; -- response
        constant  Trns_RESP_l        : integer :=    252       ;
        constant  Trns_READ_b        : integer :=    251       ; -- read data
        constant  Trns_READ_l        : integer :=    220       ;
        constant  Trns_BUSY          : integer :=    219       ; -- go busy 
        --                                           218..217  ; -- spare
        constant  Trns_NUMSEQBEATS_b : integer :=    216       ; -- number of beats in sequential transaction
        constant  Trns_NUMSEQBEATS_l : integer :=    208       ;
        constant  Trns_STARTADDR_b   : integer :=    207       ; -- start address of transaction
        constant  Trns_STARTADDR_l   : integer :=    176       ; 
        constant  Trns_EXPDATA_b     : integer :=    175       ; -- expected data
        constant  Trns_EXPDATA_l     : integer :=    144       ;
        constant  Trns_TRANSNUM_b    : integer :=    143       ; -- transaction number
        constant  Trns_TRANSNUM_l    : integer :=    128       ; 
        -- ----------------------------------------------------  -- following fields read from command file
        --                                           127..112  ; -- spare
        --                                           111..109  ; -- spare
        constant  Trns_BUSCOMMD      : integer :=    108       ; -- bus command (0 => inactive)
        constant  Trns_ADDR_b        : integer :=    107       ; -- address
        constant  Trns_ADDR_l        : integer :=     76       ;
        --                                            75.. 73  ; -- spare
        constant  Trns_WRITE         : integer :=     72       ; -- write
        constant  Trns_DATA_b        : integer :=     71       ; -- write data / expected read data
        constant  Trns_DATA_l        : integer :=     40       ;
        --                                            39.. 37  ; -- spare
        constant  Trns_LOCK          : integer :=     36       ; -- lock (not implemented)
        --                                            35.. 33  ; -- spare
        constant  Trns_CHKDATA       : integer :=     32       ; -- check expected data (not implemented)
        --                                            31.. 30  ; -- spare
        constant  Trns_TRANSTYPE_b   : integer :=     29       ; -- transaction type
        constant  Trns_TRANSTYPE_l   : integer :=     28       ; 
        --                                            27.. 23  ; -- spare
        constant  Trns_BURSTTYPE_b   : integer :=     22       ; -- burst type
        constant  Trns_BURSTTYPE_l   : integer :=     20       ;
        --                                            19       ; -- spare
        constant  Trns_SIZE_b        : integer :=     18       ; -- size
        constant  Trns_SIZE_l        : integer :=     16       ;
        constant  Trns_RPTCOUNT_b    : integer :=     15       ; -- repeat count
        constant  Trns_RPTCOUNT_l    : integer :=      0       ;

     -- +------------+
     -- | slave port |
     -- +------------+

     -- record of address and control information (latched on address phase)

        signal  startReg  : std_logic_vector(31 downto 0) := ( others => '0') ; --// start address for burst
        signal   addrReg  : std_logic_vector(31 downto 0) := ( others => '0') ;
        signal 	transReg  : std_logic_vector( 1 downto 0) := H_IDLE           ;
        signal   sizeReg  : std_logic_vector( 1 downto 0) := H_WORD           ;
        signal 	writeReg  : std_logic                     := '0'              ;
        signal 	burstReg  : std_logic_vector( 2 downto 0) := H_SINGLE         ;
        signal    selReg  : std_logic                     := '0'              ;
        signal   waitReg  : std_logic_vector( 7 downto 0) := ( others => '0') ;

     -- Implement 6 banks of 256K = (1.5MB of address space)
        
        constant membank_size     : integer := 20480 ; -- full 65535 leads to memory allocation problems

        type upcore_membank       is array (0 to membank_size) of std_logic_vector(31 downto 0);  
        type upcore_bankcfg       is array (0 to 5) of std_logic_vector(79 downto 0);
        type upcore_bankstart_end is array (0 to 5) of std_logic_vector(31 downto 0);
        type upcore_memwait       is array (0 to 5) of std_logic_vector( 7 downto 0);

        constant start_addr_b     : integer := 79   ; -- start address index upper limit
        constant start_addr_l     : integer := 48   ; -- start address index lower limit
        constant end_addr_b       : integer := 47   ; -- end address index upper limit
        constant end_addr_l       : integer := 16   ; -- end address index lower limit
        constant first_acc_wait_b : integer := 15   ; -- wait states on first access index upper limit
        constant first_acc_wait_l : integer :=  8   ; -- wait states on first access index lower limit
        constant cycle_wait_b     : integer :=  7   ; -- wait states per cycle index upper limit
        constant cycle_wait_l     : integer :=  0   ; -- wait states per cycle index lower limit

        -------------------------
        function upcore_loadcfg return upcore_bankcfg is
           -- file     slavememory_cfg       : TEXT is in "slavememory.cfg.dat"          ;
            file     slavememory_cfg       : TEXT OPEN READ_MODE IS "slavememory .cfg.dat"          ;
            variable result                : upcore_bankcfg := (others=>(others=>'0')) ;
            variable memory_cfg_bitvector  : bit_vector (start_addr_b downto 0)        ;
            variable mem_bank_counter      : INTEGER := 0                              ;
            variable cfgline               : LINE                                      ; 
        begin

                read_memory : while (not ENDFILE(slavememory_cfg) and (mem_bank_counter < NUMBANKS))
                loop
                 -- read next data line 
                    READLINE (slavememory_cfg, cfgline) ;
                    if ( cfgline'LENGTH = 0 ) then 
                        next ;
                    elsif ( cfgline (1) = '/' )  then 
                        next ;
                    end if ;
                 -- data is a 20 digit (memory_bitvector'LENGTH/4) HEX string
                    HREAD_bv (cfgline , memory_cfg_bitvector ) ;
                    result   (mem_bank_counter) := To_StdLogicVector ( memory_cfg_bitvector );
                    mem_bank_counter := mem_bank_counter + 1 ;
                end loop; -- read_memory 

            return result;
        end upcore_loadcfg;
        -------------------------

        signal memCfg       : upcore_bankcfg       := upcore_loadcfg                ; -- // slavememory.cfg.dat

        -------------------------
        function upcore_loadbank(bank        : integer        ;
                                 bank_string : string(1 to 17);  
                                 config      : upcore_bankcfg ) return upcore_membank is
            variable result           : upcore_membank := (others=>(others=>'0'));
           -- file     memory_file      : TEXT is in bank_string           ;
            file     memory_file      : TEXT OPEN READ_MODE IS bank_string;
            variable data_line        : LINE                             ;
            variable memory_bitvector : bit_vector (31 downto 0)         ; 
            variable line_counter     : INTEGER := 0                     ; 

        begin
            read_slave_bank : while not ENDFILE(memory_file)
            loop
                READLINE (memory_file, data_line) ;
                if ( data_line'LENGTH = 0 ) then 
                    next ;
                elsif ( data_line (1) = '/' )  then 
                    next ;
                end if ;
                HREAD_bv (data_line , memory_bitvector ) ;
                result (line_counter) := To_StdLogicVector (memory_bitvector) ;
                line_counter := line_counter + 1 ; 
            end loop ; 
            return result;
        end upcore_loadbank;
        -------------------------


        -------------------------
        function upcore_checkbank(bank        : integer         ;
                                  bank_string : string(1 to 17) ) return upcore_membank is
            variable config : upcore_bankcfg  := upcore_loadcfg ;
            variable result : upcore_membank  := (others=>(others=>'0'));
        begin

         -- If end =/= start, read in the memory file (otherwise everything set to zero).

            if (   conv_integer((config(bank)(start_addr_b downto start_addr_l))) /= conv_integer((config(bank)(end_addr_b downto end_addr_l))) ) 
            then 
                result := upcore_loadbank(bank,bank_string,config);
            end if ; -- different start and end address
            return result;

        end upcore_checkbank;
        -------------------------

        -------------------------
        function initialise_memstart return upcore_bankstart_end is
            variable config : upcore_bankcfg := upcore_loadcfg ;
            variable result : upcore_bankstart_end := (others=>(others=>'0'))  ;
        begin 
            result(0) := config(0) (start_addr_b downto start_addr_l);
            result(1) := config(1) (start_addr_b downto start_addr_l);
            result(2) := config(2) (start_addr_b downto start_addr_l);
            result(3) := config(3) (start_addr_b downto start_addr_l);
            result(4) := config(4) (start_addr_b downto start_addr_l);
            result(5) := config(5) (start_addr_b downto start_addr_l);
            return result;
        end initialise_memstart;
        -------------------------

        -------------------------
        function initialise_memend return upcore_bankstart_end is
            variable config : upcore_bankcfg := upcore_loadcfg ;
            variable result : upcore_bankstart_end := (others=>(others=>'0'))  ;
        begin 
            result(0) := config(0) (end_addr_b downto end_addr_l);
            result(1) := config(1) (end_addr_b downto end_addr_l);
            result(2) := config(2) (end_addr_b downto end_addr_l);
            result(3) := config(3) (end_addr_b downto end_addr_l);
            result(4) := config(4) (end_addr_b downto end_addr_l);
            result(5) := config(5) (end_addr_b downto end_addr_l);
            return result;
        end initialise_memend;
        -------------------------
	
        -------------------------
        function initialise_memwaitstart return upcore_memwait is
            variable config : upcore_bankcfg := upcore_loadcfg ;
            variable result : upcore_memwait := (others=>(others=>'0'))  ;
        begin 
            result(0) := config(0) (first_acc_wait_b downto first_acc_wait_l);
            result(1) := config(1) (first_acc_wait_b downto first_acc_wait_l);
            result(2) := config(2) (first_acc_wait_b downto first_acc_wait_l);
            result(3) := config(3) (first_acc_wait_b downto first_acc_wait_l);
            result(4) := config(4) (first_acc_wait_b downto first_acc_wait_l);
            result(5) := config(5) (first_acc_wait_b downto first_acc_wait_l);
            return result;
        end initialise_memwaitstart;
        -------------------------

        -------------------------
        function initialise_memwait return upcore_memwait is
            variable config : upcore_bankcfg := upcore_loadcfg ;
            variable result : upcore_memwait := (others=>(others=>'0'))  ;
        begin 
            result(0) := config(0) (cycle_wait_b downto cycle_wait_l);
            result(1) := config(1) (cycle_wait_b downto cycle_wait_l);
            result(2) := config(2) (cycle_wait_b downto cycle_wait_l);
            result(3) := config(3) (cycle_wait_b downto cycle_wait_l);
            result(4) := config(4) (cycle_wait_b downto cycle_wait_l);
            result(5) := config(5) (cycle_wait_b downto cycle_wait_l);
            return result;
        end initialise_memwait;
        -------------------------

        signal memStart     : upcore_bankstart_end := initialise_memstart                     ;
        signal memEnd       : upcore_bankstart_end := initialise_memend                       ;
        signal memWaitStart : upcore_memwait       := initialise_memwaitstart                 ;
        signal memWait      : upcore_memwait       := initialise_memwait                      ;
	
        signal sel          : std_logic                     := '0'           ;
        signal doWork       : std_logic                     := '0'           ;
        signal doBusyWork   : std_logic                     := '0'           ;
        signal seqTrans     : std_logic                     := '0'           ;

        signal wrapmask     : std_logic_vector(31 downto 0) := (others=>'0') ;
        signal wrapmask_w   : std_logic_vector(31 downto 0) := (others=>'0') ;
        signal seqPlusAddr  : std_logic_vector(31 downto 0) := (others=>'0') ;
        signal seqAddr      : std_logic_vector(31 downto 0) := (others=>'0') ;
        signal startNext    : std_logic_vector(31 downto 0) := (others=>'0') ;
        signal addrNext     : std_logic_vector(31 downto 0) := (others=>'0') ;
        signal transNext    : std_logic_vector( 1 downto 0) := (others=>'0') ;
        signal sizeNext     : std_logic_vector( 1 downto 0) := (others=>'0') ;
        signal burstNext    : std_logic_vector( 2 downto 0) := (others=>'0') ;
        signal writeNext    : std_logic                     := '0'           ;
		
        signal bankA        : std_logic                     := '0'           ;
        signal bankB        : std_logic                     := '0'           ;
        signal bankC        : std_logic                     := '0'           ;
        signal bankD        : std_logic                     := '0'           ;
        signal bankE        : std_logic                     := '0'           ;
        signal bankF        : std_logic                     := '0'           ;
        signal offsetA      : std_logic_vector(31 downto 0) := (others=>'0') ;
        signal offsetB      : std_logic_vector(31 downto 0) := (others=>'0') ;
        signal offsetC      : std_logic_vector(31 downto 0) := (others=>'0') ;
        signal offsetD      : std_logic_vector(31 downto 0) := (others=>'0') ;
        signal offsetE      : std_logic_vector(31 downto 0) := (others=>'0') ;
        signal offsetF      : std_logic_vector(31 downto 0) := (others=>'0') ;
        signal wordA        : std_logic_vector(15 downto 0) := (others=>'0') ;
        signal wordB        : std_logic_vector(15 downto 0) := (others=>'0') ;
        signal wordC        : std_logic_vector(15 downto 0) := (others=>'0') ;
        signal wordD        : std_logic_vector(15 downto 0) := (others=>'0') ;
        signal wordE        : std_logic_vector(15 downto 0) := (others=>'0') ;
        signal wordF        : std_logic_vector(15 downto 0) := (others=>'0') ;
        signal be0          : std_logic                     := '0'           ;
        signal be1          : std_logic                     := '0'           ;
        signal be2          : std_logic                     := '0'           ;
        signal be3          : std_logic                     := '0'           ;
        signal readDataMask : std_logic_vector(31 downto 0) := (others=>'0') ;
        signal currentValSig   : std_logic_vector(31 downto 0) := (others=>'0') ;

     -- +--------------+
     -- | master port  |
     -- +--------------+
	
        type upcore_transStore is array (NUMTRANS downto 1) of std_logic_vector(Trns_TRANSNUM_l-1 downto 0);

        -------------------------
        function upcore_loadtrans return upcore_transStore is
           --file     mastercommands        : TEXT is in "mastercommands.dat"              ;
            file     mastercommands        : TEXT OPEN READ_MODE IS "mastercomma nds.dat"              ;
            variable result                : upcore_transStore := (others=>(others=>'0')) ;
            variable commandsline          : LINE                                         ; 
            variable transaction_bitvector : bit_vector (Trns_TRANSNUM_l-1 downto 0)      ;
            variable trans_num_iter        : INTEGER := 1                                 ; 
        begin

                read_commands : while not ENDFILE(mastercommands) 
                loop
                 -- read next data line 
                    READLINE (mastercommands , commandsline) ;
                    if ( commandsline'LENGTH = 0 ) then 
                        next ;
                    elsif ( commandsline (1) = '/' )  then 
                        next ;
                    end if ;
                    HREAD_bv (commandsline , transaction_bitvector ) ;
                    result   (trans_num_iter) := To_StdLogicVector (transaction_bitvector) ;
                    trans_num_iter := trans_num_iter + 1 ; 
                end loop; -- read_commands 

            return result;
        end upcore_loadtrans;
        -------------------------

     -- +-----------------------------------------------------------------+
     -- |  Transaction records                                            |
     -- |                                                                 |
     -- |  The transactor pipeline consists of 4 stages:                  |
     -- |                                                                 |
     -- |  nextTrans    - the next transaction from the store             |
     -- |  controlTrans - the current control/address stage transaction   |
     -- |  dataTrans    - the data stage transaction                      |
     -- |  reportTrans  - the completed stage for reporting               |
     -- |                                                                 |
     -- |  controlTrans is updated from nextTrans when a new transaction  |
     -- |  begins or from dataTrans in the case of split/retry            |
     -- |                                                                 |
     -- +-----------------------------------------------------------------+

        -- dataTrans    <= idleTrans ;  -- fill pipe with null's
        -- controlTrans <= idleTrans ;
        -- reportTrans  <= idleTrans ;                               
        -- nextTrans    <= idleTrans ;
        -- nextTrans(2) <= '1'       ;  -- repeat 4

        subtype trans_vector is std_logic_vector (Trns_b downto 0) ;

        -------------------------
        function initialise_idle return trans_vector is
            variable result   : trans_vector := (others=>'0') ;
        begin 
         -- set up a null transaction record
            result (Trns_TRANSNUM_b  downto Trns_TRANSNUM_l ) := (OTHERS => '1') ; 
            result (Trns_TRANSTYPE_b downto Trns_TRANSTYPE_l) := H_IDLE          ;
            return result;
        end initialise_idle;
        -------------------------

        -------------------------
        function initialise_next return trans_vector is
            variable result   : trans_vector := (others=>'0') ;
        begin 
         -- set up a null transaction record
            result (Trns_TRANSNUM_b  downto Trns_TRANSNUM_l ) := (OTHERS => '1') ; 
            result (Trns_TRANSTYPE_b downto Trns_TRANSTYPE_l) := H_IDLE          ;
         -- repeat 4
            result (2) := '1' ;  
            return result;
        end initialise_next;
        -------------------------


        signal trans_num       : std_logic_vector(15 downto 0)                := (others=>'0')    ; -- limit is 2^16-1 = NUMTRANS
        signal tmp_transaction : std_logic_vector(Trns_TRANSNUM_l-1 downto 0) := (others=>'0')    ;
        signal currTransNum    : std_logic_vector(15 downto 0)                := (others=>'0')    ;
        signal tmp_beats       : std_logic_vector( 8 downto 0)                := (others=>'0')    ;


        signal idleTrans       : trans_vector      := initialise_idle  ;
        signal controlTrans    : trans_vector      := initialise_idle  ;
        signal dataTrans       : trans_vector      := initialise_idle  ;
        signal reportTrans     : trans_vector      := initialise_idle  ; 
        signal retryTrans      : trans_vector      := initialise_idle  ;
        signal nextTrans       : trans_vector      := initialise_next  ;

     -- +-----------------------------------------+
     -- |  Control signals for master transactor  |
     -- +-----------------------------------------+

        signal reset                : std_logic                     := '1'                       ;
        signal stop                 : std_logic                     := '0'                       ;
        signal continue_after_error : std_logic                     := '1'                       ;
        signal generate_data        : std_logic                     := '0'                       ;
        signal write                : std_logic                                                  ;
        signal start                : std_logic                                                  ;
        signal go_busy              : std_logic                                                  ;
        signal insert_busy          : std_logic_vector( 1 downto 0) := "00"                      ;         
        signal beats                : std_logic_vector( 8 downto 0)                              ;
        signal start_address        : std_logic_vector(31 downto 0)                              ;
        signal data                 : std_logic_vector(31 downto 0)                              ;
        signal burst                : std_logic_vector( 2 downto 0)                              ;
        signal size                 : std_logic_vector( 2 downto 0)                              ;

        signal length               : std_logic_vector( 8 downto 0) := (others=>'0') ;
        signal address_length       : std_logic_vector( 8 downto 0) := (others=>'0') ;

     -- +----------------------------------+
     -- |  Transactor state and responses  |
     -- +----------------------------------+

        signal masterhgrant_in_r           : std_logic                     :=          '0'  ;
        signal break_wrap                  : std_logic                     :=          '0'  ;
        signal original_burst              : std_logic_vector( 2 downto 0) := (others=>'0') ;
        signal busy_states                 : std_logic_vector( 1 downto 0) := (others=>'0') ;
        signal busy_counter                : std_logic_vector( 1 downto 0) := (others=>'0') ;
        signal init_wrap_mask              : std_logic_vector( 9 downto 0) := (others=>'0') ;
        signal wrap_mask                   : std_logic_vector( 9 downto 0) := (others=>'0') ;
        signal masterhaddr_out_r_inc       : std_logic_vector( 7 downto 0) := (others=>'0') ;
        signal init_wrap_boundary_bit      : std_logic_vector( 7 downto 0) := (others=>'0') ;
        signal init_next_masterhaddr_out_r : std_logic_vector(10 downto 0) := (others=>'0') ;
        signal wrap_boundary_bit           : std_logic_vector(10 downto 0) := (others=>'0') ;
        signal next_masterhaddr_out_r      : std_logic_vector( 9 downto 0) := (others=>'0') ;

        signal address_bus_owned           : std_logic                     := '0' ;
        signal data_bus_owned              : std_logic                     := '0' ;
        signal add_go                      : std_logic                     := '0' ;
        signal data_go                     : std_logic                     := '0' ;
        signal reading                     : std_logic                     := '0' ;
        signal writing                     : std_logic                     := '0' ;
        signal first_beat                  : std_logic                     := '0' ;
        signal need_retry                  : std_logic                     := '0' ;
        signal wrap                        : std_logic                     := '0' ;
        signal new_grant                   : std_logic                     := '0' ;
        signal first_masterhtrans_out_r    : std_logic                     := '0' ;
        signal addr_ack                    : std_logic                     := '0' ;
        signal data_ack                    : std_logic                     := '0' ;
        signal add_go_r                    : std_logic                     := '0' ;
        signal replay_wrap                 : std_logic_vector( 2 downto 0) := (others=>'0') ;

     -- output signals

        signal   masterhwrite_out   : std_logic                      :=          '0' ;
        signal   masterhlock_out    : std_logic                      :=          '0' ;
        signal   masterhbusreq_out  : std_logic                      :=          '0' ;
        signal   masterhaddr_out    : std_logic_vector (31 downto 0) := (others=>'0');
        signal   masterhwdata_out   : std_logic_vector (31 downto 0) := (others=>'0');
        signal   masterhtrans_out   : std_logic_vector ( 1 downto 0) := (others=>'0');
        signal   masterhsize_out    : std_logic_vector ( 1 downto 0) := (others=>'0');
        signal   masterhburst_out   : std_logic_vector ( 2 downto 0) := (others=>'0');

        signal   slavehrdata_out    : std_logic_vector (31 downto 0) := (others=>'0');
        signal   slavehreadyo_out   : std_logic                      :=          '0' ;
        signal   slavehresp_out     : std_logic_vector ( 1 downto 0) := (others=>'0');
        signal   slavebuserrint_out : std_logic                      :=          '0' ;

     -- register outputs 
  
        signal   masterhtrans_out_r : std_logic_vector ( 1 downto 0) := (others=>'0');
        signal   masterhaddr_out_r  : std_logic_vector (31 downto 0) := (others=>'0');
        signal   masterhwdata_out_r : std_logic_vector (31 downto 0) := (others=>'0');
        signal   masterhburst_out_r : std_logic_vector ( 2 downto 0) := (others=>'0');
        signal   masterhwrite_out_r : std_logic                      :=          '0' ;
        signal   masterhsize_outl   : std_logic_vector ( 2 downto 0) := (others=>'0'); --  the transactor implements the full 3 bit size field

        signal   slavehreadyo_out_r : std_logic                      :=          '0' ;
        signal   slavehresp_out_r   : std_logic_vector ( 1 downto 0) := (others=>'0');

     -- bus request signals

        signal   single_beat     : std_logic := '0' ;
        signal   last_beat       : std_logic := '0' ;
        signal   retry           : std_logic := '0' ;
        signal   error           : std_logic := '0' ;

     -- bus request states

        constant req_idle        : std_logic_vector(2 downto 0) := "000";
        constant req_first       : std_logic_vector(2 downto 0) := "001";
        constant req_wait        : std_logic_vector(2 downto 0) := "101";
        constant req_masterhold  : std_logic_vector(2 downto 0) := "011";
        constant req_using       : std_logic_vector(2 downto 0) := "010";
        constant req_again       : std_logic_vector(2 downto 0) := "111";

        signal   req_state       : std_logic_vector(2 downto 0) := "000";
        signal   single_beat_r   : std_logic                    := '0'  ;

     -- complete data phase signals 

        signal   trans_end       : std_logic                    := '0'  ;

     -- write data signals 

        signal   addr_offset              : std_logic_vector( 7 downto 0) := (others=>'0');
        signal   masterhwdata_out_r_pipe  : std_logic_vector(31 downto 0) := (others=>'0');
        signal   masterhwdata_out_r_retry : std_logic_vector(31 downto 0) := (others=>'0');

     -- wait state generation

        signal   s_addr_latch  : std_logic                     := '0'          ; --  slave address latched this cycle
        signal   waitStart     : std_logic_vector( 7 downto 0) := (others=>'0');
        signal   waitSeq       : std_logic_vector( 7 downto 0) := (others=>'0');
        signal   waitStartNext : std_logic_vector( 7 downto 0) := (others=>'0');

       -- file     results_file  : TEXT is out "output.dat" ;
        file     results_file  : TEXT OPEN WRITE_MODE IS "output.dat" ;

    begin

     -- Note: in processes where signals are assigned to and used at the same simulation time, 
     -- temporary variables have been created to remove non-determinism as far as possible. 

     -- +==================================================================+
     -- | Initialisation :                                                 |
     -- | This process is executed once at the start of run then suspended |
     -- +==================================================================+

     initialisation : process 

     begin 

     -- Unused signals tied off to ground

	      lockgrantdp0  <= '0';
	      lockgrantdp1  <= '0';
	
	      debugack      <= '0';
	      debugrng0     <= '0';
	      debugrng1     <= '0';
	      debugextout   <= ( others => '0') ;

	      intuart       <= '0';
	      inttimer0     <= '0';
	      inttimer1     <= '0';
	      intcommtx     <= '0';
	      intcommrx     <= '0';
	      intproctimer  <= '0';
	      intprocbridge <= '0';
	      perreset      <= '0';

        slavehresp_out_r <= H_OKAY;

        reset <= '1'   ;
        wait for 10 ns ;
        reset <= '1'   ;
        wait for 20 ns ;
        reset <= '0'   ;
     
        wait ; 

     end process initialisation ;


     -- +---------------------------------------------------------------------------------+
     -- | Concurrent Signal Assignments (these will appear as wires in the Verilog model) |
     -- | also see - 'Slave port implementation' and 'burst mode support'                 |
     -- +---------------------------------------------------------------------------------+

        start            <= nextTrans(Trns_BUSCOMMD                               ) ;
        go_busy          <= nextTrans(Trns_BUSY                                   ) ;
        beats            <= nextTrans(Trns_NUMSEQBEATS_b downto Trns_NUMSEQBEATS_l) ;
        burst            <= nextTrans(Trns_BURSTTYPE_b   downto Trns_BURSTTYPE_l  ) ;
        size             <= nextTrans(Trns_SIZE_b        downto Trns_SIZE_l       ) ;
        write            <= nextTrans(Trns_WRITE                                  ) ;
        start_address    <= nextTrans(Trns_STARTADDR_b   downto Trns_STARTADDR_l  ) ;
        data             <= nextTrans(Trns_DATA_b        downto Trns_DATA_l       ) ;        

     -- +----------------------------+
     -- | Output Signal Assignments  |
     -- +----------------------------+

     -- direct output assignments 

        masterhwrite     <= masterhwrite_out   ;
        masterhlock      <= masterhlock_out    ;
        masterhbusreq    <= masterhbusreq_out  ;
        masterhaddr      <= masterhaddr_out    ;
        masterhwdata     <= masterhwdata_out   ;
        masterhtrans     <= masterhtrans_out   ;
        masterhsize      <= masterhsize_out    ;
        masterhburst     <= masterhburst_out   ;

        slavehreadyo     <= slavehreadyo_out   ;
        slavebuserrint   <= slavebuserrint_out ;
        slavehrdata      <= slavehrdata_out    ;
        slavehresp       <= slavehresp_out     ;

     -- registered output assignments 

        masterhtrans_out <= masterhtrans_out_r ;
        masterhaddr_out  <= masterhaddr_out_r  ;
        masterhwdata_out <= masterhwdata_out_r ;
        masterhburst_out <= masterhburst_out_r ;
        masterhtrans_out <= masterhtrans_out_r ;
        masterhwrite_out <= masterhwrite_out_r ;
        masterhsize_out  <= masterhsize_outl (1 downto 0);  -- upCore only uses 2 bits of the full 3 bit size field

        slavehreadyo_out <= slavehreadyo_out_r ;
        slavehresp_out   <= slavehresp_out_r   ;


     -- +===============================================+
     -- | process NEXTTRANSACTION                       |
     -- |                                               |
     -- |   Sets up the next transaction and waits for  |
     -- |   it to be accepted (rising edge of clk)      |
     -- +===============================================+

        nexttransaction : process 

         -- Some signals are used in checks at current simulation time, t,
         -- rather than at t+delta, so create temporary variables. This should
         -- then give the same functionality as the blocking statements in the 
         -- Verilog version of this model.  

            variable temp_nextTrans       : trans_vector                                 ;
            variable temp_tmp_transaction : std_logic_vector(Trns_TRANSNUM_l-1 downto 0) := (others=>'0') ;
            variable temp_trans_num       : std_logic_vector(15                downto 0) := (others=>'0') ;
            variable temp_tmp_beats       : std_logic_vector( 8                downto 0) := (others=>'0') ;
            variable transactions         : upcore_transStore := upcore_loadtrans ;

        begin

        while ( conv_integer(trans_num) < NUMTRANS )
        loop 

            temp_trans_num := trans_num ;
            temp_tmp_beats := tmp_beats ;
  
            if (conv_integer(nextTrans(Trns_RPTCOUNT_b downto Trns_RPTCOUNT_l)) = 0 ) then
       
             -- get the next record out of those read from the mastercommands file

                temp_trans_num                                           := temp_trans_num + '1' ; 

             -- set temp_tmp_transaction variable now so it can be used straight away in the assignments and checks below.
                    
                temp_tmp_transaction                                     := transactions(conv_integer(temp_trans_num)) ;
                temp_nextTrans                                           := nextTrans ;
                temp_nextTrans(Trns_EXPDATA_b    downto Trns_EXPDATA_l ) := temp_tmp_transaction (Trns_DATA_b downto Trns_DATA_l); -- copy data to expected transaction data
                temp_nextTrans(Trns_TRANSNUM_b   downto Trns_TRANSNUM_l) := temp_trans_num       ;
                temp_nextTrans(Trns_TRANSNUM_l-1 downto               0) := temp_tmp_transaction ;

             -- check for a BUSY
  
                if (UNSIGNED(temp_tmp_transaction(Trns_TRANSTYPE_b downto Trns_TRANSTYPE_l)) = UNSIGNED(H_BUSY)) then
                    temp_nextTrans(Trns_BUSY) := '1';
                else
                    temp_nextTrans(Trns_BUSY) := '0';
                end if ;
    
             -- compute the number of beats in burst

                if (    (UNSIGNED(temp_nextTrans(Trns_BURSTTYPE_b downto Trns_BURSTTYPE_l)) /= UNSIGNED(H_SINGLE))  
                    and (UNSIGNED(temp_nextTrans(Trns_TRANSTYPE_b downto Trns_TRANSTYPE_l))  = UNSIGNED(H_NONSEQ)) ) then
                    temp_tmp_beats := "000000001";
                    temp_nextTrans(Trns_NUMSEQBEATS_b downto Trns_NUMSEQBEATS_l) := "000000001";
                    temp_tmp_transaction := transactions(conv_integer(temp_trans_num) + conv_integer(temp_tmp_beats)) ;
                    while (    (UNSIGNED(temp_tmp_transaction(Trns_TRANSTYPE_b downto Trns_TRANSTYPE_l)) = UNSIGNED(H_SEQ) )
                            or (UNSIGNED(temp_tmp_transaction(Trns_TRANSTYPE_b downto Trns_TRANSTYPE_l)) = UNSIGNED(H_BUSY)) ) 
                    loop 
                        temp_nextTrans(Trns_NUMSEQBEATS_b downto Trns_NUMSEQBEATS_l) := 
                                                     temp_nextTrans(Trns_NUMSEQBEATS_b downto Trns_NUMSEQBEATS_l) + 
                                                     conv_integer(temp_tmp_transaction(Trns_RPTCOUNT_b downto Trns_RPTCOUNT_l)) + 
                                                     1 ;
                        temp_tmp_beats            := temp_tmp_beats + 1 ;
                        temp_tmp_transaction      := transactions(conv_integer(temp_trans_num) + conv_integer(temp_tmp_beats));
                    end loop ;
                    tmp_beats <= temp_tmp_beats ;
                end if ;
    
             -- update the start address

                if (    (UNSIGNED(temp_nextTrans(Trns_TRANSTYPE_b downto Trns_TRANSTYPE_l)) /= UNSIGNED(H_SEQ) )  
                    and (UNSIGNED(temp_nextTrans(Trns_TRANSTYPE_b downto Trns_TRANSTYPE_l)) /= UNSIGNED(H_BUSY)) ) then
                    temp_nextTrans(Trns_STARTADDR_b downto Trns_STARTADDR_l) := temp_nextTrans(Trns_ADDR_b downto Trns_ADDR_l); -- start address
                end if ;

             -- assign the temporary variables we've been using to the signals they represent
                nextTrans       <= temp_nextTrans       ;
                tmp_transaction <= temp_tmp_transaction ;
                trans_num       <= temp_trans_num       ; 

            else
                nextTrans(Trns_RPTCOUNT_b downto Trns_RPTCOUNT_l) <= nextTrans(Trns_RPTCOUNT_b downto Trns_RPTCOUNT_l)- 1 ;
            end if ;

            -- wait for the current transaction to be accepted (rising edge of clk)
            
            wait on masterhclk until ( masterhclk = '1' ) ;
            while ( not ( addr_ack='1' or data_ack='1' ) and nextTrans(Trns_BUSCOMMD)='1') 
            loop
                wait on masterhclk until ( masterhclk = '1' ) ;
            end loop ; 
 
            -- transactions in buffer

        end loop ; 
   
        assert FALSE
            report "Altera BFM stripe-to-PLD transaction list exhausted"
            severity WARNING;

        wait;

     end process nexttransaction ;  

     -- +=============================================================+	
     -- | process COMPBURSTLEN                                        |
     -- |                                                             |
     -- |          Compute burst length                               |
     -- |                                                             |
     -- |          add_go_r prevents a newly loaded length being      |
     -- |          decremented by the last data beat of the previous  |
     -- |          transaction.                                       |       
     -- +=============================================================+	

     compburstlen : process ( masterhclk , reset )
  
        variable report_string_80   : string(1 to 80) := (others=>' ') ;
        variable report_string_20   : string(1 to 20) := (others=>' ') ;
        variable report_string_10   : string(1 to 10) := (others=>' ') ;
        variable display_line       : LINE                             ;

     begin

         if (masterhclk'event) then 
             if (masterhready='1') then 
                 add_go_r <= add_go;
             end if ;
         end if ;

         if (    ( masterhclk'event and masterhclk='1') 
              or ( reset'event      and reset='1'     ) ) then

             if (reset='1') then
                 length <= "000000000";
             elsif (add_go='1') then
                 case burst
                 is 
                     when H_SINGLE => length <= "000000001";
                     when H_INCR   => length <=  beats     ;
                     when H_WRAP4  |
                          H_INCR4  => length <= "000000100";
                     when H_WRAP8  |
                          H_INCR8  => length <= "000001000";
                     when H_WRAP16 |
                          H_INCR16 => length <= "000010000";
                     when others   => report_string_80(1 to 21) := "error in burst signal" ;
                                      std.textio.write(display_line , report_string_80);
                                      std.textio.writeline(results_file, display_line );		

                 end case ;
             elsif (     (reading = '1' or writing = '1') 
                     and masterhready='1'
                     and add_go_r /= '1' 
                     and (UNSIGNED(masterhresp)=UNSIGNED(H_OKAY) or UNSIGNED(masterhresp)=UNSIGNED(H_ERROR)) ) then 
                 length <= length - ur_OR(length);
             end if ;

             if (reset='1') then
                 address_length <= "000000000";
             elsif (add_go='1') then
                 case burst
                 is 
                     when H_SINGLE => address_length <= "000000001";
                     when H_INCR   => address_length <=  beats     ;
                     when H_WRAP4  |
                          H_INCR4  => address_length <= "000000100";
                     when H_WRAP8  |
                          H_INCR8  => address_length <= "000001000";
                     when H_WRAP16 |
                          H_INCR16 => address_length <= "000010000";
                     when others   => report_string_80(1 to 21) := "error in burst signal" ;
                                      std.textio.write(display_line , report_string_80);
                                      std.textio.writeline(results_file, display_line );		
                 end case ;
             elsif (    data_bus_owned        = '1' 
                    and masterhready         /= '1'
                    and (UNSIGNED(masterhresp) = UNSIGNED(H_RETRY) or UNSIGNED(masterhresp) = UNSIGNED(H_SPLIT)) ) then 
                 address_length <= address_length + '1' ;
             elsif (    address_bus_owned     = '1' 
                    and masterhready          = '1' 
                    and ur_OR (busy_states)  /= '1'
                    and UNSIGNED(masterhtrans_out_r) /= UNSIGNED(H_IDLE)) then
                 address_length <= address_length - ur_OR (address_length);
             elsif (    address_bus_owned     = '1'
                    and masterhready          = '1'
                    and ur_OR (busy_states)   = '1'
                    and UNSIGNED(masterhtrans_out_r) = UNSIGNED(H_BUSY)
                    and ur_OR (busy_counter) /= '1'  ) then 
                 address_length <= address_length - ur_OR (address_length);
             end if ; 
         end if ; 

     end process compburstlen ;

     -- +-----------------------------------------------------------+
     -- | Slave port implementation : concurrent signal assignments |
     -- +-----------------------------------------------------------+

     -- select signal

        sel        <= slavehsel and slavehreadyi;
	
     -- determine if the transaction includes an operation / a "busy"

        doWork     <= '1' when ( selReg='1' and 
		               ( transReg=conv_integer(H_NONSEQ) or transReg=conv_integer(H_SEQ)) )
                 else '0';

        doBusyWork <= '1' when ( selReg='1' and transReg=conv_integer(H_BUSY) ) 
                 else '0';
	
     -- +--------------------------------------------------+
     -- | BURST MODE SUPPORT                               |
     -- |                                                  |
     -- | If we are in burst mode we'll compute our own    |
     -- | address and control settings based on the spec.  |
     -- |                                                  |
     -- | compute values SEQuential (burst) transfers      |
     -- +--------------------------------------------------+

         seqTrans <= '1' when  selReg='1' 
                        and ( doWork='1' or doBusyWork='1') 
                        and (slavehtrans=conv_integer(H_SEQ) or slavehtrans=conv_integer(H_BUSY)) 
               else '0';

         with burstReg select
             wrapmask  <= (1 downto 0=>'0', others=>'1') when H_WRAP4 
                        , (2 downto 0=>'0', others=>'1') when H_WRAP8 
                        , (2 downto 0=>'0', others=>'1') when H_WRAP16 
                        , (others=>'0')                  when others ;
					
         with sizeReg select
             wrapmask_w <= shift_left(wrapmask, 2) when H_WORD  
                         , shift_left(wrapmask, 1) when H_HWORD 
                         , wrapmask                when others  ;

         with sizeReg select
             seqPlusAddr<= addrReg + 1 when H_BYTE 
                         , addrReg + 2 when H_HWORD 
                         , addrReg + 4 when H_WORD 
                         , addrReg     when others ;
					
         seqAddr <= addrReg when slavehtrans = conv_integer(H_BUSY) 
                              or burstReg    = conv_integer(H_SINGLE) 
               else (startReg and wrapmask_w) or (seqPlusAddr and (not(wrapmask_w)));
			
         -- if this is a sequential transaction only sample HTRANS

        startNext <= startReg when seqTrans='1' else slavehaddr  ;
         addrNext <= seqAddr  when seqTrans='1' else slavehaddr  ;
        transNext <= slavehtrans;
         sizeNext <= sizeReg  when seqTrans='1' else slavehsize  ;
        burstNext <= burstReg when seqTrans='1' else slavehburst ;
        writeNext <= writeReg when seqTrans='1' else slavehwrite ;
	
     -- +==============================================================+	
     -- |                                                              |
     -- | process CTRLLATCH :                                          |
     -- |                                                              |
     -- |        Latch the control data if we are selected             |
     -- |                                                              |
     -- +==============================================================+	

        ctrllatch: process ( slavehclk )
        begin 
            if ( slavehclk'event and slavehclk='1') then

             -- if read is low another device is wait stating its
             -- data phase and hence extending our address phase

                if ( slavehreadyi = '1' ) then
                    selReg <= sel;
                    if ( sel = '1' ) then		-- latch the control data
                        startReg <= startNext    ;
                         addrReg <=  addrNext    ;
                        transReg <= transNext    ;
                         sizeReg <=  sizeNext    ;
                        writeReg <= writeNext    ;
                        burstReg <= burstNext    ;
                    else
                        startReg <= (others=>'0');
                         addrReg <= (others=>'0');
                        transReg <= H_IDLE       ;
                         sizeReg <= H_WORD       ;
                        writeReg <= '0'          ;
                        burstReg <= H_SINGLE     ;
                    end if;
                end if;
            end if;
        end process ctrllatch;
	

        bankA <= '1' when (           conv_integer(addrReg)     >= memStart(0) 
                            and conv_integer(addrReg)     <= memEnd(0) 
                            and conv_integer(memStart(0)) /= memEnd(0) ) 
                     else '0';

        bankB <= '1' when (           conv_integer(addrReg)     >= memStart(1) 
                            and conv_integer(addrReg)     <= memEnd(1) 
                            and conv_integer(memStart(1)) /= memEnd(1) )  
                     else '0';

        bankC <= '1' when (           conv_integer(addrReg)     >= memStart(2) 
                            and conv_integer(addrReg)     <= memEnd(2) 
                            and conv_integer(memStart(2)) /= memEnd(2) ) 
                     else '0';

        bankD <= '1' when (           conv_integer(addrReg)     >= memStart(3) 
                            and conv_integer(addrReg)     <= memEnd(3) 
                            and conv_integer(memStart(3)) /= memEnd(3) ) 
                     else '0';

        bankE <= '1' when (           conv_integer(addrReg)     >= memStart(4) 
                            and conv_integer(addrReg)     <= memEnd(4) 
                            and conv_integer(memStart(4)) /= memEnd(4) )  
                     else '0';

        bankF <= '1' when (           conv_integer(addrReg)     >= memStart(5) 
                            and conv_integer(addrReg)     <= memEnd(5) 
                            and conv_integer(memStart(5)) /= memEnd(5) )  
                     else '0';

	
      -- byte offset into bank                  -- word offset into bank

         offsetA <= addrReg - memStart(0);         
                                                   wordA <= offsetA(17 downto 2);
         offsetB <= addrReg - memStart(1);         
                                                   wordB <= offsetB(17 downto 2);
         offsetC <= addrReg - memStart(2);         
                                                   wordC <= offsetC(17 downto 2);
         offsetD <= addrReg - memStart(3);         
                                                   wordD <= offsetD(17 downto 2);
         offsetE <= addrReg - memStart(4);         
                                                   wordE <= offsetE(17 downto 2);
         offsetF <= addrReg - memStart(5);         
                                                   wordF <= offsetF(17 downto 2);

         -- byte enables

         be0<= '1' when 	( UNSIGNED(sizeReg)=UNSIGNED(H_WORD)  or
                         ( UNSIGNED(sizeReg)=UNSIGNED(H_HWORD) and addrReg(1)='0' ) or
                         ( UNSIGNED(sizeReg)=UNSIGNED(H_BYTE)  and UNSIGNED(addrReg(1 downto 0))=0) )
                  else '0';
         be1<= '1' when 	( UNSIGNED(sizeReg)=UNSIGNED(H_WORD)  or
                         ( UNSIGNED(sizeReg)=UNSIGNED(H_HWORD) and addrReg(1)='0' ) or
                         ( UNSIGNED(sizeReg)=UNSIGNED(H_BYTE)  and UNSIGNED(addrReg(1 downto 0))=1) )
                  else '0';
         be2<= '1' when 	( UNSIGNED(sizeReg)=UNSIGNED(H_WORD)  or
                         ( UNSIGNED(sizeReg)=UNSIGNED(H_HWORD) and addrReg(1)='1' ) or
                         ( UNSIGNED(sizeReg)=UNSIGNED(H_BYTE)  and UNSIGNED(addrReg(1 downto 0))=2) )
                  else '0';
         be3<= '1' when 	( UNSIGNED(sizeReg)=UNSIGNED(H_WORD)  or
                         ( UNSIGNED(sizeReg)=UNSIGNED(H_HWORD) and addrReg(1)='1' ) or
                         ( UNSIGNED(sizeReg)=UNSIGNED(H_BYTE)  and UNSIGNED(addrReg(1 downto 0))=3) )
                  else '0';

         readDataMask( 7 downto  0) <= "11111111"  when ( be0='1' ) else "00000000" ;
         readDataMask(15 downto  8) <= "11111111"  when ( be1='1' ) else "00000000" ;
         readDataMask(23 downto 16) <= "11111111"  when ( be2='1' ) else "00000000" ;
         readDataMask(31 downto 24) <= "11111111"  when ( be3='1' ) else "00000000" ;
			
     -- +=================================+	
     -- | process waitgen                 |
     -- |                                 |
     -- |     wait state generation       |
     -- +=================================+	

        waitstartgen : process (bankA, bankB, bankC, bankD, bankE, bankF )
        begin
            if    (bankA='1') then waitStart <= memWaitStart(0);
            elsif (bankB='1') then waitStart <= memWaitStart(1);
            elsif (bankC='1') then waitStart <= memWaitStart(2);
            elsif (bankD='1') then waitStart <= memWaitStart(3);
            elsif (bankE='1') then waitStart <= memWaitStart(4);
            elsif (bankF='1') then waitStart <= memWaitStart(5);
            end if ;
        end process waitstartgen ; 

        waitseqgen : process (bankA, bankB, bankC, bankD, bankE, bankF )
        begin
            if    (bankA='1') then waitSeq   <= memWait(0);
            elsif (bankB='1') then waitSeq   <= memWait(1);
            elsif (bankC='1') then waitSeq   <= memWait(2);
            elsif (bankD='1') then waitSeq   <= memWait(3);
            elsif (bankE='1') then waitSeq   <= memWait(4);
            elsif (bankF='1') then waitSeq   <= memWait(5);
            end if ;
        end process waitseqgen ; 

     -- +==================================================================+	
     -- | slave wait processes                                             |
     -- |                                                                  |
     -- | wait if                                                          |
     -- |    first beat and memWaitStart and addr has just been latched    |
     -- | or                                                               |
     -- |    first beat and waitReg (more than 1 wait state)               |
     -- | or                                                               |
     -- |    seq beat and waitReg                                          |
     -- | else ready                                                       |
     -- +==================================================================+	

        slavelatch : process ( slavehclk )
        begin
        if ( slavehclk'event and slavehclk='1') then
            s_addr_latch <= slavehreadyi and slavehsel;
        end if ; 
        end process slavelatch ;

        slavewait : process ( doWork, transReg, waitReg, waitStart, s_addr_latch )
            variable report_string_20 : string(1 to 20) := (others=>' ') ;
            variable display_line     : LINE                             ;
        begin 
            if    (doWork='1' and UNSIGNED(transReg)=UNSIGNED(H_NONSEQ) and UNSIGNED(waitStart)/=0 and s_addr_latch='1' ) then
                slavehreadyo_out_r <= '0' ;
                --report_string_20 := "SLAVE: wait on first" ;
                --std.textio.write (display_line , report_string_20);
                --std.textio.writeline (results_file, display_line );
            elsif (doWork='1' and UNSIGNED(transReg)=UNSIGNED(H_NONSEQ) and UNSIGNED(waitReg)/=0 ) then 
                slavehreadyo_out_r <= '0' ;
                --report_string_20 := "SLAVE: wait         " ;
                --std.textio.write (display_line , report_string_20);
                --std.textio.writeline (results_file, display_line );
            elsif (doWork='1' and UNSIGNED(transReg)=UNSIGNED(H_SEQ) and UNSIGNED(waitReg)/=0) then 
                slavehreadyo_out_r <= '0' ;
                --report_string_20 := "SLAVE: wait         " ;
                --std.textio.write (display_line , report_string_20);
                --std.textio.writeline (results_file, display_line );
            else
                slavehreadyo_out_r <= '1' ;
            end if ; 
        end process slavewait ;

         -- if we are waiting (waitReg>0) and not in a busy decrement the counter
         -- otherwise get the new value from memWait of memWaitStart according to
         -- the transaction type 

         waitStartNext <= waitStart - 1 when ( UNSIGNED(waitStart) > 1 )
                      else "00000000";
  
         slavewaitreg : process ( slavehclk )
         begin
            if ( slavehclk'event and slavehclk='1') then
                if (UNSIGNED(waitReg) /= 0 and doBusyWork/='1') then 
                    waitReg <= waitReg - '1' ;
                elsif (doWork='1' and UNSIGNED(transReg)=UNSIGNED(H_NONSEQ) and UNSIGNED(waitStart)/=0 and s_addr_latch='1' ) then
                    waitReg <= waitStartNext ;
                elsif (seqTrans='1' ) then
                    waitReg <= waitSeq ;
                else 
                    waitReg <= "00000000" ;
                end if ; 
            end if ; 
         end process slavewaitreg ;

         -- read data
      slavehrdata_out <= (readDataMask and currentValSig) when (doWork='1' and writeReg/='1' and slavehreadyo_out_r='1')
                   else (others=>'0');

     -- +===========================================================================+	
     -- |                                                                           |
     -- | process MEMWORK :                                                         |
     -- |                                                                           |
     -- |      +  records reads and writes in memory banks                          |
     -- |      +  writes a text report to the screen                                |
     -- |                                                                           |
     -- +===========================================================================+	

        memwork : process ( slavehclk ,
                 wordA,   wordB,   wordC,   wordD,   wordE,   wordF, 
                 bankA,   bankB,   bankC,   bankD,   bankE,   bankF)

            variable memMapA            : upcore_membank  := upcore_checkbank(0,"slavememory.0.dat") ;
            variable memMapB            : upcore_membank  := upcore_checkbank(1,"slavememory.1.dat") ;
            variable memMapC            : upcore_membank  := upcore_checkbank(2,"slavememory.2.dat") ;
            variable memMapD            : upcore_membank  := upcore_checkbank(3,"slavememory.3.dat") ;
            variable memMapE            : upcore_membank  := upcore_checkbank(4,"slavememory.4.dat") ;
            variable memMapF            : upcore_membank  := upcore_checkbank(5,"slavememory.5.dat") ;

            variable report_string_11   : string(1 to 11) := (others=>' ') ;
            variable report_string_10   : string(1 to 10) := (others=>' ') ;
            variable report_string_9    : string(1 to  9) := (others=>' ') ;
            variable report_string_8    : string(1 to  8) := (others=>' ') ;
            variable report_string_6    : string(1 to  6) := (others=>' ') ;
            variable display_line       : LINE                             ;
            variable decimal            : BOOLEAN := FALSE                 ;
            variable temp_memWord       : std_logic_vector(31 downto 0)    ;
            variable currentVal         : std_logic_vector(31 downto 0) := (others=>'0') ;

        begin

            if    (bankA='1' and conv_integer(wordA)< membank_size ) then currentVal := memMapA(conv_integer(wordA)) ; 
            elsif (bankB='1' and conv_integer(wordB)< membank_size ) then currentVal := memMapB(conv_integer(wordB)) ; 
            elsif (bankC='1' and conv_integer(wordC)< membank_size ) then currentVal := memMapC(conv_integer(wordC)) ; 
            elsif (bankD='1' and conv_integer(wordD)< membank_size ) then currentVal := memMapD(conv_integer(wordD)) ; 
            elsif (bankE='1' and conv_integer(wordE)< membank_size ) then currentVal := memMapE(conv_integer(wordE)) ; 
            elsif (bankF='1' and conv_integer(wordF)< membank_size ) then currentVal := memMapF(conv_integer(wordF)) ; 
            end if;

            currentValSig <= currentVal;

            if ( slavehclk'event and slavehclk='1') 
            then
	
                if (doWork='1' and slavehreadyo_out_r='1') 
                then
 
                    temp_memWord := currentVal ;

                    if (writeReg='1') 
                    then
                        if (be0='1') then temp_memWord( 7 downto  0) := slavehwdata( 7 downto  0); end if;
                        if (be1='1') then temp_memWord(15 downto  8) := slavehwdata(15 downto  8); end if;
                        if (be2='1') then temp_memWord(23 downto 16) := slavehwdata(23 downto 16); end if;
                        if (be3='1') then temp_memWord(31 downto 24) := slavehwdata(31 downto 24); end if;
		
                        if (bankA='1') then memMapA(conv_integer(wordA)) := temp_memWord; end if;
                        if (bankB='1') then memMapB(conv_integer(wordB)) := temp_memWord; end if;
                        if (bankC='1') then memMapC(conv_integer(wordC)) := temp_memWord; end if;
                        if (bankD='1') then memMapD(conv_integer(wordD)) := temp_memWord; end if;
                        if (bankE='1') then memMapE(conv_integer(wordE)) := temp_memWord; end if;
                        if (bankF='1') then memMapF(conv_integer(wordF)) := temp_memWord; end if;
                    end if;


                 -- display a text report on screen
                    report_string_10 := "SLAVE:    " ;
                    std.textio.write (display_line , report_string_10);
                    report_string_10 := "          " ;
                    std.textio.write (display_line , report_string_10);
                    report_string_9  := "   addr=[" ;
                    std.textio.write (display_line , report_string_9);
                    if (decimal) then 
                        std.textio.write (display_line, conv_integer(addrReg),right,8);
                    else
                        IEEE.std_logic_textio.hwrite (display_line,addrReg,right,8);
                    end if ; 
                    if (writeReg = '1') 
                    then 
                        report_string_8 := "] WRITE " ;
                    else 
                        report_string_8 := "] READ  " ; 
                    end if;
                    std.textio.write (display_line, report_string_8);

                    report_string_6 := "data=[" ;
                    std.textio.write (display_line , report_string_6);
                    if (writeReg='1') 
                    then 
                        if (decimal) then 
                            std.textio.write (display_line, conv_integer(slavehwdata),right,8);
                        else
                            IEEE.std_logic_textio.hwrite (display_line,slavehwdata,right,8);
                        end if ; 
                    else
                        if (decimal) then 
                            std.textio.write (display_line, conv_integer(slavehrdata_out),right,8);
                        else
                            IEEE.std_logic_textio.hwrite (display_line,slavehrdata_out,right,8);
                        end if ; 
                    end if;
                    report_string_6  := "]     " ;
                    std.textio.write (display_line , report_string_6);
                    report_string_11 := "           " ;
                    std.textio.write (display_line , report_string_11);
                    report_string_10 := "          " ;
                    std.textio.write (display_line , report_string_10);

                    case sizeReg 
                    is
                        when H_BYTE => report_string_10 :=  "     BYTE " ;
                        when H_HWORD=> report_string_10 :=  "HALF WORD " ;
                        when H_WORD => report_string_10 :=  "     WORD " ;
                        when others => report_string_10 :=  " <error>  " ;
                    end case;
                    std.textio.write (display_line , report_string_10);

                    std.textio.writeline(results_file, display_line );		

                end if;

            end if;

        end process memwork;

     -- +==========================================+
     -- |                                          |
     -- | process reportcomptrans :                |
     -- |                                          |
     -- |        Report completed transactions     |
     -- |                                          |
     -- +==========================================+

        reportcomptrans : process ( masterhclk )

            variable report_string_14   : string(1 to 14) := (others=>' ') ;
            variable report_string_9    : string(1 to 9 ) := (others=>' ') ;
            variable report_string_7    : string(1 to 7 ) := (others=>' ') ;
            variable display_line       : LINE                             ;
            variable decimal            : BOOLEAN := FALSE                 ;

        begin

            if ( masterhclk'event and masterhclk='1')
            then
                if (reportTrans(Trns_BUSCOMMD)='1') 
                then 
                    report_string_7 := "MASTER:" ;
                    std.textio.write (display_line, report_string_7);

                    report_string_9 := " trans=[ " ;	
                    std.textio.write (display_line, report_string_9);
                    std.textio.write (display_line, conv_integer(reportTrans(Trns_TRANSNUM_b downto Trns_TRANSNUM_l)),right,4);

                    report_string_9 := "]  addr=[" ;	
                    std.textio.write (display_line, report_string_9);
                    if (decimal) then 
                        std.textio.write (display_line, conv_integer(reportTrans(Trns_ADDR_b downto Trns_ADDR_l)),right,8);
                    else
                        IEEE.std_logic_textio.hwrite (display_line, reportTrans(Trns_ADDR_b downto Trns_ADDR_l),right,8);
                    end if ; 

                    if (reportTrans(Trns_WRITE) = '1') 
                    then 
                        report_string_14 := "] WRITE data=[" ;
                        std.textio.write (display_line, report_string_14);
                        if (decimal) then 
                            std.textio.write (display_line, conv_integer(reportTrans(Trns_DATA_b downto Trns_DATA_l)),right,8);
                        else
                            IEEE.std_logic_textio.hwrite (display_line, reportTrans(Trns_DATA_b downto Trns_DATA_l),right,8);
                        end if ; 
                    else 
                        report_string_14 := "] READ  data=[" ; 
                        std.textio.write (display_line, report_string_14);
                        if (decimal) then 
                            std.textio.write (display_line, conv_integer(reportTrans(Trns_READ_b downto Trns_READ_l)),right,8);
                        else
                            IEEE.std_logic_textio.hwrite (display_line, reportTrans(Trns_READ_b downto Trns_READ_l),right,8);
                        end if ; 
                    end if;

                    report_string_14 := "]   expected=[" ;	
                    std.textio.write (display_line, report_string_14);
                    if (decimal) then 
                        std.textio.write (display_line, conv_integer(reportTrans(Trns_EXPDATA_b downto Trns_EXPDATA_l)),right,8);
                    else
                        IEEE.std_logic_textio.hwrite (display_line, reportTrans(Trns_EXPDATA_b downto Trns_EXPDATA_l),right,8);
                    end if ; 

                    case reportTrans(Trns_SIZE_b-1 downto Trns_SIZE_l) -- (only 2 of the 3 bits are used)
                    is
                        when H_BYTE  => report_string_14 :=  "]         BYTE" ;
                        when H_HWORD => report_string_14 :=  "]    HALF WORD" ;
                        when H_WORD  => report_string_14 :=  "]         WORD" ;
                        when others  => report_string_14 :=  "]      <error>" ;
                    end case;
                    std.textio.write (display_line , report_string_14);

                    if (UNSIGNED(reportTrans(Trns_RESP_b downto Trns_RESP_l))=UNSIGNED(H_OKAY) ) then
                        report_string_9 :=  "  OKAY   " ;
                    else
                        report_string_9 :=  "  ERROR  " ;
                    end if ; 
                    std.textio.write (display_line , report_string_9);

                    std.textio.writeline(results_file, display_line );		
 
                end if;	
            end if;
        end process reportcomptrans;

     -- +====================================================================================+
     -- |                                                                                    |
     -- | process BUSREQUEST :                                                               |
     -- |                                                                                    |
     -- | Bus request state machine                                                          |
     -- |                                                                                    |
     -- |     Bus request machine follows the principle that the arbiter will generally      |
     -- |     only re-assign bus grants at the end of a burst transaction. For defined       |
     -- |     bursts masterhbusreq_out is removed as soon as we masterhave started the       |
     -- |     transaction. Undefined (INCR) bursts will masterhold masterhbusreq_out         |
     -- |     asserted until the last beat of the transaction.                               |
     -- |                                                                                    |
     -- |     Locked transactions must always assert masterhlock_out for at least one        |
     -- |     cycle before the address to be locked to allow the arbiter to see the lock.    |
     -- |     In practice, this means inserting an idle cycle.                               | 
     -- |                                                                                    |
     -- |     Have to be careful using burst and beats from the control word. As soon        |
     -- |     as the master address phase masterhas finished and the addr_ack is asserted    |
     -- |     the testbench can change the control word. So don't use them after the         | 
     -- |     initial request. Use the ahb outputs instead which will tell us what sort      |
     -- |     of transaction we're doing.                                                    |
     -- |                                                                                    |
     -- | Bus request machine master has 5 states:                                           |
     -- |                                                                                    |
     -- |    1) req_idle:  masterhbusreq_out negated. When we want to do something           |
     -- |                  we jump to req_first. The last beat may get a retry               |
     -- |                  response in which case we jump to req_again.                      |
     -- |                                                                                    |
     -- |    2) req_first: masterhbusreq_out asserted. Wait here for masterhgrant            |
     -- |                  and until the transaction starts. If granted and it's an          |
     -- |                  undefined and not a single beat then jump to req_masterhold.      |
     -- |                  Else if it's a single beat jump to req_idle. Otherwise jump       |
     -- |                  to req_using.                                                     |
     -- |                                                                                    |
     -- |    3) req_masterhold: masterhbusreq_out asserted. Hold masterhbusreq_out           |
     -- |                  asserted until last beat of an undefined. If there's a            |
     -- |                  new request then we jump to req_first, otherwise back to          |
     -- |                  req_idle. If we lose masterhgrant in this state then we           |
     -- |                  just stay here with masterhbusreq_out asserted until the          |
     -- |                  transaction can be finished. Also hold in this state if           |
     -- |                  retry is asserted to reduce the chance of releaseing the          |
     -- |                  bus and having to re-request it to complete a transaction.        |
     -- |                                                                                    |
     -- |    4) req_using: masterhbusreq_out negated. Wait here for last beat of             |
     -- |                  defined length transaction. If there's a new request then         |
     -- |                  we jump to req_first, otherwise back to req_idle. If a            |
     -- |                  posted write is errored before the last beat or a transaction     |
     -- |                  is retried or we lose masterhgrant then we jump to                | 
     -- |                  req_again.                                                        |
     -- |                                                                                    |
     -- |    5) req_again: masterhbusreq_out asserted for completion of transaction          |
     -- |                  interrupted by loss of masterhgrant. Wait here for                |
     -- |                  masterhgrant and until the transaction starts then                |
     -- |                  jump to req_using if first_beat is asserted or                    |
     -- |                  req_masterhold if not.                                            |
     -- |                  *** We may see a new address toggle whilst in this state.         |
     -- |                                                                                    |
     -- +====================================================================================+


        single_beat <= '1' when (UNSIGNED(burst)=UNSIGNED(H_SINGLE) or (UNSIGNED(burst)=UNSIGNED(H_INCR) and UNSIGNED(beats)=1))
                  else '0' ;

        last_beat   <= '1' when (address_bus_owned='1' and masterhready='1' and UNSIGNED(address_length) <= 1) 
                  else '0' ;

        retry       <= '1' when (data_bus_owned='1' and (UNSIGNED(masterhresp)=UNSIGNED(H_RETRY) or UNSIGNED(masterhresp)=UNSIGNED(H_SPLIT)))
                  else '0' ;

        error       <= '1' when (data_bus_owned='1' and masterhready='1' and UNSIGNED(masterhresp)=UNSIGNED(H_ERROR))
                  else '0' ;

        masterhbusreq_out <= '1' when (   (start     = '1'           )
                                       or (UNSIGNED(req_state) = UNSIGNED(req_first)     )
                                       or (UNSIGNED(req_state) = UNSIGNED(req_wait)      )
                                       or (UNSIGNED(req_state) = UNSIGNED(req_masterhold))
                                       or (UNSIGNED(req_state) = UNSIGNED(req_again)     ) )
                        else '0' ;

        busrequest : process ( masterhclk , reset)

        begin

            if (masterhclk'event and masterhclk = '1') then 
                if (addr_ack='1') then
                 -- save single_beat for use after it may have changed
                    single_beat_r <= single_beat;
                end if ;
            end if ;

            if (   (masterhclk'event and masterhclk = '1') 
                or (reset'event      and reset      = '1') ) then

                if (reset='1') then
                    req_state <= req_idle;
                else
                  case req_state 
                  is
                    when req_idle       => if (retry='1') then
                                               req_state <= req_again;
                                           elsif (start='1') then
                                               req_state <= req_first;
                                           else
                                               req_state <= req_idle ;
                                           end if ;

                    when req_first      => if (retry='1') then
                                               req_state <= req_again;
                                           elsif (masterhgrant/='1' and not (UNSIGNED(masterhtrans_out_r)=UNSIGNED(H_NONSEQ) and masterhready='1')) then
                                               req_state <= req_first;
                                           elsif (UNSIGNED(masterhtrans_out_r)=UNSIGNED(H_NONSEQ) and masterhready='1') then
                                               if (add_go='1') then
                                                   req_state <= req_first;
                                               elsif (UNSIGNED(burst)=UNSIGNED(H_INCR) and single_beat/='1') then
                                                   req_state <= req_masterhold;
                                               elsif (single_beat='1') then
                                                   req_state <= req_idle;
                                               else
                                                   req_state <= req_using;
                                               end if ;
                                           else
                                               req_state <= req_wait;
                                           end if ;

                    when req_wait       => if (retry='1') then
                                               req_state <= req_again;
                                           elsif (masterhgrant/='1' and not (UNSIGNED(masterhtrans_out_r)=UNSIGNED(H_NONSEQ) and masterhready='1')) then
                                               req_state <= req_first;
                                           elsif (masterhgrant='1' and not (UNSIGNED(masterhtrans_out_r)=UNSIGNED(H_NONSEQ) and masterhready='1')) then
                                               req_state <= req_wait;
                                           elsif (add_go='1') then
                                               req_state <= req_first;
                                           elsif (UNSIGNED(burst)=UNSIGNED(H_INCR) and single_beat/='1') then
                                               req_state <= req_masterhold;
                                           elsif (single_beat_r='1') then
                                               req_state <= req_idle;
                                           else
                                               req_state <= req_using;
                                           end if ;

                    when req_masterhold => if (error='1' and continue_after_error/='1') then
                                               req_state <= req_idle;
                                           elsif (   (masterhgrant/='1' and (conv_integer(address_length) > 1 )) 
                                                  or retry='1' ) then
                                               req_state <= req_again;
                                           elsif (last_beat='1') then
                                               if (start='1') then
                                                   req_state <= req_first;
                                               else
                                                   req_state <= req_idle;
                                               end if ;
                                           elsif (add_go='1') then
                                               req_state <= req_first;
                                           else
                                               req_state <= req_masterhold;
                                           end if ;

                    when req_using      => if (error='1' and continue_after_error/='1') then
                                               req_state <= req_idle;
                                           elsif (last_beat='1') then
                                               if (start='1') then
                                                   req_state <= req_first;
                                               else
                                                   req_state <= req_idle;
                                               end if ;
                                           elsif (   (masterhgrant/='1' and (conv_integer(address_length) > 1 )) 
                                                  or retry='1') then
                                               req_state <= req_again;
                                           else
                                               req_state <= req_using;
                                           end if ;

                    when req_again      => if (error='1' and continue_after_error/='1') then
                                               req_state <= req_idle;
                                           elsif (     (   (data_bus_owned/='1') 
                                                        or (data_bus_owned='1' and (UNSIGNED(masterhresp) = UNSIGNED(H_OKAY))) )
                                                   and address_bus_owned='1' 
                                                   and UNSIGNED(masterhtrans_out_r)=UNSIGNED(H_IDLE) 
                                                   and masterhready='1'
                                                   and masterhlock_out/='1') then
                                               req_state <= req_idle;
                                           elsif (     (masterhgrant/='1' and (conv_integer(address_length) > 1 )) 
                                                    or (not ((UNSIGNED(masterhtrans_out_r)=UNSIGNED(H_NONSEQ)) and (masterhready='1'))) ) then
                                               req_state <= req_again;
                                           elsif (     (    (last_beat='1') 
                                                        or  (UNSIGNED(masterhburst_out_r)=UNSIGNED(H_SINGLE)))
                                                    or (    (UNSIGNED(masterhburst_out_r)=UNSIGNED(H_INCR)) 
                                                        and (single_beat_r='1'))    ) then
                                               req_state <= req_idle;
                                           elsif (first_beat='1') then
                                               req_state <= req_using;
                                           else
                                               req_state <= req_masterhold;
                                           end if ;

                    when others         => req_state <= req_idle;
                                                 
                  end case ;
                end if ;
            end if ;
 
         end process busrequest ;

     -- +------------------------------------------------------+
     -- | Address and Data acknowledge assignments             |
     -- |                                                      |
     -- | Signals when an address has been transferred and a   |
     -- | new one may be presented for the next transaction.   |
     -- +------------------------------------------------------+

        addr_ack <= add_go  ;
        data_ack <= data_go ;

     -- +======================================================+
     -- | process busownership                                 |
     -- |                                                      |
     -- | Data bus ownership follows address by one cycle      |
     -- +======================================================+

        busownership : process (masterhclk, reset)
        begin
        if (   ( masterhclk'event and masterhclk='1')  
            or ( reset'event      and reset='1'     ) ) then 
            if ( reset ='1' ) then
                address_bus_owned <= '0' ;
                data_bus_owned    <= '0' ;
            elsif (masterhready ='1' ) then 
                address_bus_owned <= masterhgrant;
                data_bus_owned    <= address_bus_owned;
            end if ;
        end if ; 
        end process busownership ;

     -- +===============================================================+
     -- | process ENABLEADDRESSPHASE                                    |
     -- |                                                               |
     -- |      add_go enables the address phase for a new transaction   |
     -- |      (not the continuation of a retried transaction or a      |
     -- |      transaction during which we lose the bus).               |
     -- |      It asserts immediately on address request if we're not   |
     -- |      actively using the bus and not waiting for it to be      |
     -- |      re-granted to complete a previous transaction, the       |
     -- |               (masterhtrans_out_r = IDLE)                     |
     -- |      term ensuring it only asserts for one clock.             |
     -- +===============================================================+

        enableaddressphase : process (start, masterhbusreq_out, masterhgrant, masterhready, reading,
                                      writing, masterhtrans_out_r, req_state, length, reset   )
        begin
            if   (     start               = '1' 
                   and masterhbusreq_out   = '1' 
                   and masterhgrant        = '1' 
                   and masterhready        = '1' 
                   and reading            /= '1' 
                   and writing            /= '1'
                   and UNSIGNED(masterhtrans_out_r)=UNSIGNED(H_IDLE) 
                   and UNSIGNED(req_state)        /=UNSIGNED(req_again)
                   and reset              /= '1' ) then
                add_go <= '1' ;
            elsif (    start               = '1' 
                   and masterhbusreq_out   = '1' 
                   and masterhgrant        = '1' 
                   and masterhready        = '1' 
                   and conv_integer(length) < 2
                   and retry              /= '1'
                   and UNSIGNED(masterhtrans_out_r)/=UNSIGNED(H_BUSY) 
                   and UNSIGNED(masterhtrans_out_r)/=UNSIGNED(H_NONSEQ)
                   and reset              /= '1' ) then
                add_go <= '1' ;
            else
                add_go <= '0' ;
            end if ; 
        end process enableaddressphase ;

     -- +=================================================================+
     -- | process COMPLETEDATAPHASE			                                    |
     -- |                                                                 |
     -- |      data_go indicates the completion of the data phase for a   |
     -- |      transaction. Like add_go it asserts when the master        |
     -- |      takes control of the address lines to start a transaction. |
     -- |      It also asserts on all the accepted data beats of a burst  |
     -- |      except the last.                                           |
     -- +=================================================================+

        updatetrans_end : process ( data_bus_owned, reading, writing, masterhready, masterhresp )
            variable boolean_1 : std_logic ;
        begin
            if (UNSIGNED(masterhresp)=UNSIGNED(H_OKAY) or UNSIGNED(masterhresp)=UNSIGNED(H_ERROR)) then boolean_1 := '1' ; else boolean_1 := '0' ; end if ; 
            trans_end <=  (    data_bus_owned
                           and (reading or writing) 
                           and masterhready
                           and boolean_1 ) ;
        end process updatetrans_end ; 


        completedataphase : process (start,   masterhbusreq_out, writing,    reset, 
                                     reading, masterhgrant,  req_state,  trans_end,
                                     length,  masterhready,  need_retry, masterhtrans_out_r )

        begin

            if (    start                = '1' 
                and masterhbusreq_out    = '1' 
                and masterhgrant         = '1' 
                and masterhready         = '1' 
                and reading             /= '1' 
                and writing             /= '1'
                and UNSIGNED(masterhtrans_out_r) = UNSIGNED(H_IDLE)
                and UNSIGNED(req_state)         /= UNSIGNED(req_again)
                and reset               /= '1' 
                and need_retry          /= '1' ) then
                data_go <= '1' ;
            elsif ( start                = '1' 
                and masterhbusreq_out    = '1' 
                and masterhgrant         = '1' 
                and masterhready         = '1' 
                and conv_integer(address_length) > 1 
                and retry               /= '1'
                and reset               /= '1' 
                and (need_retry/='1' or trans_end='1') ) then
                data_go <= '1' ;
            else
                data_go <= '0' ;
            end if ; 

        end process completedataphase ;

     -- +======================================================+
     -- | process UPDATES                                      |
     -- |                                                      |
     -- |    update masterhwrite_out_r, Transaction size,      |
     -- |    busy state and counter on leading edge of clock   |
     -- |    or reset                                          |
     -- +======================================================+

        updates : process ( masterhclk, reset ) 

        begin

            if (   ( masterhclk'event and masterhclk='1')  
                or ( reset'event      and reset='1'     ) ) then 

             -- +------------------------------------------------------+
             -- | masterhwrite_out_r update                            |
             -- |                                                      |
             -- |  Updated on any clock that starts a new transaction  |
             -- +------------------------------------------------------+

                if ( reset='1' ) then
                    masterhwrite_out_r <= '1' ;
                elsif ( addr_ack='1' ) then 
                    masterhwrite_out_r <= write ;
                end if ;

             -- +------------------------------------------------------+
             -- | Transaction size update                              |
             -- |                                                      |
             -- |  Updated on any clock that starts a new transaction  |
             -- +------------------------------------------------------+

                if ( reset ='1' ) then
                    masterhsize_outl <= "000" ;
                elsif ( addr_ack ='1' ) then 
                    masterhsize_outl <= size ;
                end if ;
 
             -- +------------------------------------------------------+
             -- | Busy state and counter update                        |
             -- |                                                      |
             -- |  Insert BUSY states into burst transactions.         |
             -- |  Capture control word. Load counter on every active  |
             -- |  phase and decrement to zero.                        |
             -- +------------------------------------------------------+

                if ( reset ='1' ) then
                    busy_states <= "00" ;
                elsif ( addr_ack ='1' ) then 
                    busy_states <= insert_busy ;
                end if ;
  
                if ( reset ='1' ) then
                    busy_counter <= "00" ;
                elsif ((UNSIGNED(masterhtrans_out_r)=UNSIGNED(H_NONSEQ)) or (UNSIGNED(masterhtrans_out_r) = UNSIGNED(H_SEQ))) then 
                    busy_counter <= busy_states - 1;
                else
                    busy_counter <= busy_counter - ur_OR(busy_counter);
                end if ;

            end if ; 

        end process updates ; 

     -- +=================================================================+
     -- |  RESTART TRANS processes                                        |
     -- |                                                                 |
     -- |  first_masterhtrans_out_r is asserted to enable the first beat  |
     -- |  of a transaction addr_ack (which is always NONSEQ) to :        |
     -- |       + restart a transaction that was interrupted by loss      |
     -- |         of masterhgrant if we receive a new masterhgrant        |
     -- |         whilst in req_again or req_masterhold states.           |
     -- |       + restart a transaction after a RETRY response.           |
     -- |       + restart a transaction after a SPLIT response.           |
     -- |       + break an undefined INCR replay of a retried or split    |
     -- |         wrapping burst at the wrap address boundary.            |
     -- +-----------------------------------------------------------------+

        restarttrans : process (masterhclk, reset)

            variable masterhgrant_in_leading_edge  : std_logic := '0' ;

        begin

            if ( masterhclk'event and masterhclk ='1') then
                masterhgrant_in_r <= masterhgrant;
            end if ;

            masterhgrant_in_leading_edge := masterhgrant and not masterhgrant_in_r ;

            if (   ( masterhclk'event and masterhclk ='1') 
                or ( reset'event      and reset='1'     ) ) then
                if (reset = '1') then
                    new_grant <= '0' ;
                elsif (masterhgrant_in_leading_edge = '1' and first_masterhtrans_out_r /= '1') then
                    new_grant <= '1' ;
                elsif (first_masterhtrans_out_r = '1' or masterhgrant /= '1') then
                    new_grant <= '0' ;
                end if ;
            end if ;

         end process restarttrans ;


         updatefirst_masterhtrans_out_r : process ( addr_ack, masterhgrant, masterhgrant_in_r, new_grant, masterhready, masterhwrite_out_r, 
                                                    first_beat, data_bus_owned, address_bus_owned, length, masterhresp, req_state)

            variable boolean_1 : std_logic := '0' ; 
            variable boolean_2 : std_logic := '0' ; 
            variable boolean_3 : std_logic := '0' ; 
            variable boolean_4 : std_logic := '0' ; 
            variable boolean_5 : std_logic := '0' ; 
            variable masterhgrant_in_leading_edge  : std_logic := '0' ;

         begin

            -- convert booleans in expression to '1's or '0's

            masterhgrant_in_leading_edge := masterhgrant and not masterhgrant_in_r ;

            if (UNSIGNED(req_state)  =UNSIGNED(req_masterhold)) then boolean_1 := '1' ; else boolean_1 := '0' ; end if ;
            if (UNSIGNED(req_state)  =UNSIGNED(req_again)     ) then boolean_2 := '1' ; else boolean_2 := '0' ; end if ;
            if (UNSIGNED(masterhresp)=UNSIGNED(H_RETRY)       ) then boolean_3 := '1' ; else boolean_3 := '0' ; end if ;
            if (UNSIGNED(masterhresp)=UNSIGNED(H_SPLIT)       ) then boolean_4 := '1' ; else boolean_4 := '0' ; end if ;
            if (conv_integer(length) > 1  ) then boolean_5 := '1' ; else boolean_5 := '0' ; end if ;

            first_masterhtrans_out_r <= (addr_ack 
                or ((((masterhgrant_in_leading_edge or (masterhgrant and new_grant)) and masterhready) and (not masterhwrite_out_r)) and (boolean_1 or boolean_2))
                or ((masterhgrant_in_leading_edge or (masterhgrant and new_grant)) and (masterhready and masterhwrite_out_r) and (boolean_1 or boolean_2)  )
                or ((data_bus_owned and masterhready) and (boolean_3 or boolean_4)))
                or (((address_bus_owned and masterhready) and (not first_beat)) and (break_wrap and boolean_5));

         end process updatefirst_masterhtrans_out_r ; 
 
         -- +------------------------------------------------------+
         -- | The only time masterhtrans_out_r changes is when     |
         -- | masterhready is negated is during reset or after     |
         -- | the first cycle of a two-cyle error response.        |
         -- | Otherwise, masterhtrans_out_r can only change when   |
         -- | masterhgrant and masterhready are asserted.          |
         -- +------------------------------------------------------+

         updatemasterhtrans_out_r : process ( masterhclk, reset )

         begin

            if (   ( masterhclk'event and masterhclk='1')  
                or ( reset'event      and reset='1'     ) ) then 
                if (reset='1') then
                    masterhtrans_out_r <= H_IDLE;
                elsif (    data_bus_owned        = '1'
                       and masterhready         /= '1'
                       and UNSIGNED(masterhresp)/=UNSIGNED(H_OKAY)
                       and continue_after_error /= '1' ) then   -- ERROR'ed transactions cancelled
                    masterhtrans_out_r <= H_IDLE;
                elsif (    data_bus_owned        = '1' 
                       and masterhready         /= '1' 
                       and UNSIGNED(masterhresp)/= UNSIGNED(H_OKAY)
                       and UNSIGNED(masterhresp)/= UNSIGNED(H_ERROR)
                       and continue_after_error  = '1' ) then       -- ERROR'ed transactions not cancelled
                    masterhtrans_out_r <= H_IDLE;
                elsif (    masterhgrant='1' 
                       and masterhready='1') then
                    case masterhtrans_out_r
                    is
                        when H_IDLE    => if (first_masterhtrans_out_r='1') then 
                                              masterhtrans_out_r <= H_NONSEQ;
                                          else
                                              masterhtrans_out_r <= H_IDLE;
                                          end if ;
    
                        when H_NONSEQ  |
                             H_SEQ     => if (first_masterhtrans_out_r='1') then 
                                              masterhtrans_out_r <= H_NONSEQ;
                                          elsif ((UNSIGNED(masterhburst_out_r)=UNSIGNED(H_SINGLE)) or (conv_integer(address_length) <= 1) ) then -- Last beat
                                              masterhtrans_out_r <= H_IDLE;
                                          elsif (go_busy = '1') then 
                                              masterhtrans_out_r <= H_BUSY;
                                          else
                                              masterhtrans_out_r <= H_SEQ;
                                          end if ;
    
                        when H_BUSY    => if (first_masterhtrans_out_r='1') then 
                                              masterhtrans_out_r <= H_NONSEQ;
                                          elsif (go_busy='1') then 
                                              masterhtrans_out_r <= H_BUSY;
                                          else
                                              masterhtrans_out_r <= H_SEQ;
                                          end if ;
                        when others    => assert false report "error in masterhtrans_out_r" severity warning;
                    end case ;
                elsif (masterhready='1' and masterhgrant/='1') then
                    masterhtrans_out_r <= H_IDLE;
                end if ;
            end if ;
  
         end process updatemasterhtrans_out_r ;

         -- +------------------------------------------------------+
         -- | One of reading or writing is asserted during any     | 
         -- | data beat for which we are actively using the bus.   |
         -- +------------------------------------------------------+

         updatereadingwriting : process (masterhclk, reset) 

         begin

            if (   ( masterhclk'event and masterhclk='1')  
                or ( reset'event      and reset='1'     ) ) then 
                if (reset='1') then
                    reading <= '0' ;
                    writing <= '0' ;
                elsif (masterhready = '1') then
                    if (    masterhwrite_out_r /= '1'
                        and address_bus_owned   = '1'
                        and UNSIGNED(masterhtrans_out_r)/= UNSIGNED(H_IDLE) 
                        and UNSIGNED(masterhtrans_out_r)/= UNSIGNED(H_BUSY) ) then 
                        reading <= '1' ; 
                    else 
                        reading <= '0' ;
                    end if ;  
                    if (    masterhwrite_out_r  = '1'
                        and address_bus_owned   = '1'
                        and UNSIGNED(masterhtrans_out_r) /= UNSIGNED(H_IDLE) 
                        and UNSIGNED(masterhtrans_out_r )/= UNSIGNED(H_BUSY) ) then  
                        writing <= '1' ;
                    else 
                        writing <= '0' ;
                    end if ; 
                end if ; 
            end if ; 

         end process updatereadingwriting ;

         -- +----------------------------------------------------------+
         -- | Burst size                                               |
         -- |                                                          |
         -- | first_beat is used to keep masterhburst_out_r unchanged  |
         -- | when the first beat is to be replayed. It alse controls  |
         -- | the bus request. A transaction that is split or retried  |
         -- | on any other beat will be replayed as INCR and           |
         -- | masterhbusreq_out must be held asserted.                 |
         -- |                                                          |
         -- | Tmasterhis means that a defined length read that us      |
         -- | interrupted mid-burst will complete as an undefined INCR |
         -- | and may pre-fetch past the end of the defined length     |
         -- | (unless, of course, no_prefetch is asserted).            |
         -- +----------------------------------------------------------+

         burst_beat : process (masterhclk, reset)
         begin
            if (   ( masterhclk'event and masterhclk='1')  
                or ( reset'event      and reset='1'     ) ) then 
                if (reset='1') then
                    first_beat <= '0' ;
                elsif (addr_ack='1') then 
                    first_beat <= '1' ;
                elsif (data_bus_owned='1' and (reading='1' or writing='1') and masterhready='1' and UNSIGNED(masterhresp)=UNSIGNED(H_OKAY)) then
                    first_beat <= '0' ;
                end if ; 
             end if ; 
         end process burst_beat ; 

         burst_burst_out : process (masterhclk, reset)
         begin
            if (   ( masterhclk'event and masterhclk='1')  
                or ( reset'event      and reset='1'     ) ) then 
                if (reset='1') then
                    masterhburst_out_r <= "000" ;
                elsif (addr_ack='1') then 
                    masterhburst_out_r <= burst ;
                elsif (first_masterhtrans_out_r='1' and first_beat/='1') then 
                    masterhburst_out_r <= H_INCR;
                end if ; 
             end if ; 
         end process burst_burst_out ; 

         -- +------------+
         -- | need_retry |
         -- +------------+

         burst_need_retry : process (masterhclk, reset)
         begin
            if (   ( masterhclk'event and masterhclk='1')  
                or ( reset'event      and reset='1'     ) ) then 
                if (reset='1') then
                    need_retry <= '0' ;
                elsif (    data_bus_owned='1' 
                       and masterhready /='1' 
                       and (UNSIGNED(masterhresp)=UNSIGNED(H_RETRY) or UNSIGNED(masterhresp)=UNSIGNED(H_SPLIT)) ) then
                    need_retry <= '1' ;
                elsif (    data_bus_owned='1' 
                       and masterhready  ='1' 
                       and (reading='1' or writing='1')
                       and (UNSIGNED(masterhresp)=UNSIGNED(H_OKAY) or UNSIGNED(masterhresp) = UNSIGNED(H_ERROR)) ) then
                    need_retry <= '0' ;
                end if ;  
             end if ; 
         end process burst_need_retry ; 
 
         burst_wraps : process (masterhclk, reset)
         begin
            if (   ( masterhclk'event and masterhclk='1')  
                or ( reset'event      and reset='1'     ) ) then 
                if (reset = '1') then
                    wrap           <= '0'   ;
                    original_burst <= "000" ;
                    replay_wrap    <= "000" ;
                elsif (addr_ack = '1') then
                    if (UNSIGNED(burst)=UNSIGNED(H_WRAP4) or UNSIGNED(burst)=UNSIGNED(H_WRAP8) or UNSIGNED(burst)=UNSIGNED(H_WRAP16)) then 
                        wrap       <= '1' ;
                    else
                        wrap       <= '0' ;
                    end if ; 
                    original_burst <= burst;
                    replay_wrap    <= "000" ;
                elsif (    data_bus_owned ='1' 
                       and masterhready  /='1' 
                       and wrap           ='1' 
                       and (UNSIGNED(masterhresp)=UNSIGNED(H_RETRY) or UNSIGNED(masterhresp)=UNSIGNED(H_SPLIT)) ) then
                    replay_wrap <= "001" ;
                end if ; 
            end if ; 
        end process burst_wraps ;

     -- +===========================================================+
     -- |  Compute Wrap Mask and Bound bit processes                |
     -- |                                                           |
     -- |  Used to modify next_masterhaddr_out_r during wrapping    |
     -- |  bursts. First case statement forms a mask based on the   |
     -- |  transfer size. This is then shifted left with '1's       |
     -- |  inserted to form the final mask.                         |
     -- |  E.g. masterhsize_outl == word (3'b010) wrapped at a four | 
     -- |  beat boundary results in wrap_mask set to 10'b0000001111 | 
     -- |  allowing the four lsbs of the address to increment and   |
     -- |  wrap addressing sixteen bytes in total.                  |
     -- +===========================================================+

        computeinitwrapmask : process ( masterhsize_outl )
        begin
            case masterhsize_outl
            is
                when "000"  => init_wrap_mask <= "0000000000" ;
                when "001"  => init_wrap_mask <= "0000000001" ;
                when "010"  => init_wrap_mask <= "0000000011" ;
                when "011"  => init_wrap_mask <= "0000000111" ;
                when "100"  => init_wrap_mask <= "0000001111" ;
                when "101"  => init_wrap_mask <= "0000011111" ;
                when "110"  => init_wrap_mask <= "0000111111" ;
                when "111"  => init_wrap_mask <= "0001111111" ;
                when others => init_wrap_mask <= "0000000000" ;
                               assert false report "error in masterhsize_outl" severity warning;
            end case ;
        end process computeinitwrapmask ; 


        computewrapmask : process ( original_burst, init_wrap_mask )
        begin
            case original_burst
            is
                when H_WRAP4  =>  wrap_mask(9 downto 2) <= init_wrap_mask(7 downto 0) ;
                                  wrap_mask(1 downto 0) <= "11" ;
                when H_WRAP8  =>  wrap_mask(9 downto 3) <= init_wrap_mask(6 downto 0) ;
                                  wrap_mask(2 downto 0) <= "111" ;
                when H_WRAP16 =>  wrap_mask(9 downto 4) <= init_wrap_mask(5 downto 0) ;
                                  wrap_mask(3 downto 0) <= "1111" ;
                when others   =>  wrap_mask             <= "0000000000" ;
            end case ;
        end process computewrapmask ;



        computeinitwrapbound : process ( masterhsize_outl )
        begin
            case masterhsize_outl
            is
                when "000"  => init_wrap_boundary_bit <= "00000001" ;
                when "001"  => init_wrap_boundary_bit <= "00000010" ;
                when "010"  => init_wrap_boundary_bit <= "00000100" ;
                when "011"  => init_wrap_boundary_bit <= "00001000" ;
                when "100"  => init_wrap_boundary_bit <= "00010000" ;
                when "101"  => init_wrap_boundary_bit <= "00100000" ;
                when "110"  => init_wrap_boundary_bit <= "01000000" ;
                when "111"  => init_wrap_boundary_bit <= "10000000" ;
                when others => init_wrap_boundary_bit <= "00000000" ;
                               assert false report "error in masterhsize_outl" severity warning;
            end case ;
        end process computeinitwrapbound ; 

        computewrapbound : process ( original_burst, init_wrap_boundary_bit )
        begin
            case original_burst
            is
                when H_WRAP4  => wrap_boundary_bit(9 downto 2) <= init_wrap_boundary_bit ;
                                 wrap_boundary_bit(1 downto 0) <= "00" ;
                when H_WRAP8  => wrap_boundary_bit(9 downto 3) <= init_wrap_boundary_bit(6 downto 0) ;
                                 wrap_boundary_bit(2 downto 0) <= "000" ;
                when H_WRAP16 => wrap_boundary_bit(9 downto 4) <= init_wrap_boundary_bit(5 downto 0) ;
                                 wrap_boundary_bit(3 downto 0) <= "0000" ;
                when others   => wrap_boundary_bit              <= "00000000000" ;
            end case ;
        end process computewrapbound ;

     -- +============================================================+
     -- | CALCULATE ADDRESS processes                                |
     -- +============================================================+

     -- +------------------------------------------------------------+
     -- | Compute address increment                                  |
     -- |                                                            |
     -- | This code allows for all possibilities by inferring        |
     -- | a 3-to-8 decoder on the transfer size. AHB spec is         |
     -- | unclear how a burst with a transfer size greater           |
     -- | than the bus width should be handled.                      |
     -- +------------------------------------------------------------+

        computeaddressincrement : process ( masterhsize_outl )
        begin
            masterhaddr_out_r_inc                                 <= "00000000" ;
            masterhaddr_out_r_inc(conv_integer(masterhsize_outl)) <= '1'        ;
        end process computeaddressincrement ; 

     -- +------------------------------------------------------------+
     -- | Compute next address                                       |
     -- |                                                            |
     -- | Next address is based on the increment computed from the   |
     -- | transfer size, and the burst type, which may tell us to    |
     -- | wrap. Wrapping is achieved by preserving some of the upper |
     -- | bits through use of wrap_mask.                             |
     -- |                                                            |
     -- | If beat n is retried, we're already putting out the        |
     -- | address for beat n+1 so we need to decrement.              |
     -- +------------------------------------------------------------+

        computeinitnextaddress : process (data_bus_owned, masterhresp, masterhready, 
                                          masterhaddr_out_r, masterhaddr_out_r_inc ) 
            variable temp_init_next_masterhaddr_out_r : std_logic_vector (10 downto 0) ;
        begin
            temp_init_next_masterhaddr_out_r(10)         := '1' ;
            temp_init_next_masterhaddr_out_r(9 downto 0) := masterhaddr_out_r(9 downto 0) ;
            if (data_bus_owned='1' and ((UNSIGNED(masterhresp)=UNSIGNED(H_RETRY)) or (UNSIGNED(masterhresp)=UNSIGNED(H_SPLIT)))) then
                init_next_masterhaddr_out_r <= temp_init_next_masterhaddr_out_r - masterhaddr_out_r_inc;
            else
                init_next_masterhaddr_out_r <= temp_init_next_masterhaddr_out_r + masterhaddr_out_r_inc;
            end if ; 
        end process computeinitnextaddress ; 
  

        computenextaddress : process (original_burst, wrap_mask, masterhaddr_out_r, 
                                      init_next_masterhaddr_out_r ) 
        begin
            if (   (UNSIGNED(original_burst)=UNSIGNED(H_WRAP4) )
                or (UNSIGNED(original_burst)=UNSIGNED(H_WRAP8) )
                or (UNSIGNED(original_burst)=UNSIGNED(H_WRAP16)) ) then
                next_masterhaddr_out_r <=    (wrap_mask and init_next_masterhaddr_out_r(9 downto 0)) 
                                          or (not wrap_mask and masterhaddr_out_r(9 downto 0)) ;
            else
                next_masterhaddr_out_r <= init_next_masterhaddr_out_r (9 downto 0) ;
            end if ;
        end process computenextaddress ; 

        break_wrap <=       replay_wrap(1)
                      and ( ur_OR(init_next_masterhaddr_out_r and wrap_boundary_bit) xor ur_OR(masterhaddr_out_r(10 downto 0) and wrap_boundary_bit)  ) ;


     -- +------------------------------------------------------------+
     -- | Address Generation                                         |
     -- |                                                            |
     -- | AHB address has to track the changing address during       |
     -- | bursts. next_masterhaddr_out_r computes the next address.  | 
     -- |                                                            |
     -- | NOTE: It is incumbent upon the command file not to attempt |
     -- | a transaction that would cross a 1Kbyte address boundary.  |
     -- |                                                            |
     -- | Address is normally updated after each address phase. It   |
     -- | is also updated during the second cycle of a two cycle     |
     -- | retry or split response to rewind the address and allow    |
     -- | the transaction to be replayed.                            |
     -- +------------------------------------------------------------+

        addressgeneration : process (masterhclk, reset)
        begin
            if (   ( masterhclk'event and masterhclk='1')  
                or ( reset'event      and reset='1'     ) ) then 
                if (reset='1') then
                    masterhaddr_out_r <= (OTHERS => '0' ) ;
                elsif (addr_ack='1') then
                    masterhaddr_out_r <= start_address ;
                elsif (    data_bus_owned ='1'
                       and masterhready   ='1' 
                       and (UNSIGNED(masterhresp)=UNSIGNED(H_RETRY) or UNSIGNED(masterhresp)=UNSIGNED(H_SPLIT)) ) then 
                    masterhaddr_out_r(9 downto 0) <= next_masterhaddr_out_r ;
                elsif (    address_bus_owned ='1'
                       and masterhready      ='1'
                       and (UNSIGNED(masterhtrans_out_r)=UNSIGNED(H_NONSEQ) or UNSIGNED(masterhtrans_out_r)=UNSIGNED(H_SEQ)) ) then
                    masterhaddr_out_r(9 downto 0) <= next_masterhaddr_out_r ;
                end if ; 
            end if; 
        end process addressgeneration ;

     -- +============================================================+
     -- | WRITEDATA processes                                        |
     -- |                                                            |
     -- | If generate_data is negated then initial data is taken     |
     -- | from data input. If generate_data is asserted then data is |
     -- | generated from the address offset to match that expected   |
     -- | by the checkers.                                           |
     -- |                                                            |
     -- | The expected data and the transaction number follow the    |
     -- | write data.                                                |
     -- |                                                            |
     -- | At the end of a burst data is set to X so we can ensure    |
     -- | nothing is relying on invalid data.                        |
     -- +============================================================+

        writedatapipeandretry: process (masterhclk)
        begin
            if ( masterhclk'event and masterhclk='1') then
                if (    data_bus_owned  ='1' 
                    and masterhready   /='1' 
                    and (UNSIGNED(masterhresp)=UNSIGNED(H_RETRY) or UNSIGNED(masterhresp)=UNSIGNED(H_SPLIT)) ) then
                    masterhwdata_out_r_retry <= masterhwdata_out_r ;
                elsif (addr_ack='1' or data_ack='1') then
                    masterhwdata_out_r_pipe  <= data ;
                end if ;
            end if ; 
        end process writedatapipeandretry ;

        addr_offset(7 downto 2) <= masterhaddr_out_r (7 downto 2) ;
        addr_offset(1 downto 0) <= "00" ;      

        writedata: process (masterhclk, reset)
        begin
            if (   ( masterhclk'event and masterhclk='1')  
                or ( reset'event      and reset='1'     ) ) then 
                if (reset='1') then 
                    masterhwdata_out_r <= (OTHERS=> 'X') ;
                elsif (address_bus_owned /='1' and masterhready='1') then
                    masterhwdata_out_r <= (OTHERS=> 'X') ;
                elsif (masterhready='1' and generate_data /='1') then
                    if (address_bus_owned='1' and masterhwrite_out_r='1' and need_retry='1' and trans_end/='1') then
                        masterhwdata_out_r <= masterhwdata_out_r_retry;
                    elsif (address_bus_owned='1' and masterhwrite_out_r='1' and UNSIGNED(masterhtrans_out_r)=UNSIGNED(H_NONSEQ)) then
                        masterhwdata_out_r <= masterhwdata_out_r_pipe ; 
                    elsif (UNSIGNED(length) = 0 ) then
                        masterhwdata_out_r <= (OTHERS=> 'X') ;
                    elsif (address_bus_owned='1' and masterhwrite_out_r='1' and UNSIGNED(masterhtrans_out_r)=UNSIGNED(H_SEQ)) then
                        masterhwdata_out_r <= masterhwdata_out_r_pipe; 
                    else
                        masterhwdata_out_r <= (OTHERS=> 'X') ;
                    end if ;
                elsif (masterhready='1' and generate_data='1') then
                    if (address_bus_owned='1' and masterhwrite_out_r='1' and UNSIGNED(masterhtrans_out_r)=UNSIGNED(H_NONSEQ)) then
                        masterhwdata_out_r (31 downto 24) <= addr_offset ;
                        masterhwdata_out_r (23 downto 16) <= addr_offset ;
                        masterhwdata_out_r (15 downto  8) <= addr_offset ;
                        masterhwdata_out_r ( 7 downto  0) <= addr_offset ;
                    elsif (UNSIGNED(length) = 0 ) then
                        masterhwdata_out_r <= (OTHERS=> 'X') ;
                    elsif (address_bus_owned='1' and masterhwrite_out_r='1' and UNSIGNED(masterhtrans_out_r)=UNSIGNED(H_SEQ)) then
                        masterhwdata_out_r (31 downto 24) <= addr_offset ;
                        masterhwdata_out_r (23 downto 16) <= addr_offset ;
                        masterhwdata_out_r (15 downto  8) <= addr_offset ;
                        masterhwdata_out_r ( 7 downto  0) <= addr_offset ;
                    end if ;
                end if ; 
            end if ; 
        end process writedata ;

     -- +================================================================+
     -- | Transaction Details processes                                  |
     -- |                                                                |
     -- | The transactor pipeline consists of four stages                |
     -- |                                                                |
     -- |    nextTrans - the next transaction from the store             |
     -- | controlTrans - the current control / address stage transaction |
     -- |    dataTrans - the data stage transaction                      |
     -- |  reportTrans - the completed stage for reporting               |
     -- |                                                                |
     -- |  controlTrans is updated from nextTrans when a new transaction |
     -- |  begins or from dataTrans in the case of split/retry           |
     -- +================================================================+

        transdetails : process (masterhclk)
        begin 
            if ( masterhclk'event and masterhclk='1') then
                if (    data_bus_owned ='1' 
                    and masterhready /='1'
                    and (UNSIGNED(masterhresp)=UNSIGNED(H_RETRY) or UNSIGNED(masterhresp)=UNSIGNED(H_SPLIT)) ) then
                    retryTrans   <= dataTrans ;
                elsif (addr_ack='1' or data_ack='1') then
                    controlTrans <= nextTrans ;
                end if ;
            end if ;
        end process transdetails ;

        setdatatransdetails : process (masterhclk, reset)
        begin 
            if (   ( masterhclk'event and masterhclk='1')  
                or ( reset'event      and reset='1'     ) ) then 
                if (    address_bus_owned = '1' 
                    and masterhready = '1' 
                    and reset /= '1'
                    and (need_retry/='1' or trans_end='1') ) then

                    dataTrans                                             <= controlTrans       ;
                    dataTrans (Trns_ADDR_b      downto Trns_ADDR_l     )  <= masterhaddr_out_r  ;
                    dataTrans (Trns_WRITE                              )  <= masterhwrite_out_r ;
                    dataTrans (Trns_LOCK                               )  <= masterhlock_out    ;
                    dataTrans (Trns_TRANSTYPE_b downto Trns_TRANSTYPE_l)  <= masterhtrans_out_r ;
                    dataTrans (Trns_BURSTTYPE_b downto Trns_BURSTTYPE_l)  <= masterhburst_out_r ;
                    dataTrans (Trns_SIZE_b      downto Trns_SIZE_l     )  <= masterhsize_outl   ;

                elsif (    address_bus_owned = '1' 
                       and masterhready = '1' 
                       and reset /='1' 
                       and need_retry ='1' ) then

                    dataTrans                                             <= retryTrans         ;
                    dataTrans (Trns_ADDR_b      downto Trns_ADDR_l     )  <= masterhaddr_out_r  ;
                    dataTrans (Trns_WRITE                              )  <= masterhwrite_out_r ;
                    dataTrans (Trns_LOCK                               )  <= masterhlock_out    ;
                    dataTrans (Trns_TRANSTYPE_b downto Trns_TRANSTYPE_l)  <= masterhtrans_out_r ;
                    dataTrans (Trns_BURSTTYPE_b downto Trns_BURSTTYPE_l)  <= masterhburst_out_r ;
                    dataTrans (Trns_SIZE_b      downto Trns_SIZE_l     )  <= masterhsize_outl   ;

                elsif ( (address_bus_owned /= '1' and masterhready='1') or reset='1') then

                    dataTrans                                             <= idleTrans          ;

                end if ; 
            end if ;
        end process setdatatransdetails ;


        setreporttransdetails : process (masterhclk)
        begin 
            if ( masterhclk'event and masterhclk='1') then
                if (trans_end='1' and need_retry /='1') then
                    reportTrans (Trns_RESP_b downto Trns_RESP_l) <= masterhresp;
                    reportTrans (Trns_READ_b downto Trns_READ_l) <= masterhrdata; 
                    reportTrans (Trns_BUSY   downto          0 ) <= dataTrans(Trns_BUSY downto 0);
                elsif (trans_end='1' and need_retry = '1') then
                    reportTrans (Trns_RESP_b downto Trns_RESP_l) <= masterhresp;
                    reportTrans (Trns_READ_b downto Trns_READ_l) <= masterhrdata; 
                    reportTrans (Trns_BUSY   downto          0 ) <= retryTrans(Trns_BUSY downto 0);
                else
                    reportTrans                                  <= idleTrans;
                end if ; 
            end if ;
        end process setreporttransdetails ;  

     -- +------------------+
     -- | masterhlock_out  |
     -- +------------------+
  
        masterhlock_out <= '0' ;
  
-- +--------------------------------------------------------------------+
-- +--------------------------------------------------------------------+

end behaviour ; -- alt_exc_upcore


----------------------------------------------------------
--                   ALTDDIO_IN
----------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;

entity altddio_in is
	generic (width : positive := 1;
                 intended_device_family : string := "MERCURY";
		 power_up_high : string := "OFF");
	port (datain : in std_logic_vector(width-1 downto 0);
	      inclock : in std_logic;
	      inclocken : in std_logic := '1';
	      aset : in std_logic := '0';
	      aclr : in std_logic := '0';
	      dataout_H : out std_logic_vector(width-1 downto 0);
	      dataout_L : out std_logic_vector(width-1 downto 0));
end altddio_in;
	 
architecture behave of altddio_in is
begin
process (inclock, aset, aclr)
variable dataout_H_tmp, dataout_L_tmp, datain_latched : std_logic_vector(width-1 downto 0);
begin
        if (NOW = 0 ps) then
           if (power_up_high = "OFF") then
              dataout_H_tmp := (others => '0');
              dataout_L_tmp := (others => '0');
              datain_latched := (others => '0');
           else
              dataout_H_tmp := (others => '1');
              dataout_L_tmp := (others => '1');
              datain_latched := (others => '1');
           end if;
        end if;
        if (aclr = '1') then
           dataout_H_tmp := (others => '0');
           dataout_L_tmp := (others => '0');
              datain_latched := (others => '0');
        elsif (aset = '1') then
              dataout_H_tmp := (others => '1');
              dataout_L_tmp := (others => '1');
              datain_latched := (others => '1');
        elsif (inclock'event and inclock = '1') then
           if (inclocken = '1') then
              dataout_H_tmp := datain;
              dataout_L_tmp := datain_latched;
           end if;
	    elsif inclock'event and inclock = '0' then
           if ((intended_device_family = "APEXII" ) or 
              (intended_device_family = "APEX II") or 
              (intended_device_family = "Stratix")) then
              if inclocken = '1' then
                 datain_latched := datain;
              end if; 
           elsif intended_device_family = "MERCURY" then
              datain_latched := datain;
           else -- for future families.
              datain_latched := datain;
           end if;
        end if;
        dataout_L <= dataout_L_tmp;
        dataout_H <= dataout_H_tmp;
end process;
end behave;
----------------------------------------------------------
--                   ALTDDIO_OUT
----------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;

entity altddio_out is
	generic (width : positive := 1;
                 intended_device_family : string := "MERCURY";
                 power_up_high : string := "OFF";
                 oe_reg : string := "UNUSED";
                 extend_oe_disable : string := "UNUSED");
	port (datain_H : in std_logic_vector(width-1 downto 0);
	      datain_L : in std_logic_vector(width-1 downto 0);
	      outclock : in std_logic;
	      outclocken : in std_logic := '1';
	      aset : in std_logic := '0';
	      aclr : in std_logic := '0';
	      oe : in std_logic := '1';
	      dataout : out std_logic_vector(width-1 downto 0));
end altddio_out;
	 
architecture behave of altddio_out is
signal dataout_h : std_logic_vector(width-1 downto 0);
signal dataout_l : std_logic_vector(width-1 downto 0);

signal oe_rgd, oe_reg_ext, apexii_oe, output_enable : std_logic;
begin
output_enable <= apexii_oe when (intended_device_family = "APEXII" or intended_device_family = "APEX II") else oe;

apexii_oe <= (oe_reg_ext and oe_rgd) when extend_oe_disable = "ON"
             else oe_rgd when
                 ((oe_reg = "REGISTERED") and (extend_oe_disable /= "ON"))
             else oe;

process (outclock, aset, aclr)
begin
        if (NOW = 0 ps) then
           if (power_up_high = "OFF") then
              dataout_h <= (others => '0');
              dataout_l <= (others => '0');
              oe_rgd <= '0';
              oe_reg_ext <= '0';
           else
              dataout_h <= (others => '1');
              dataout_l <= (others => '1');
              oe_rgd <= '1';
              oe_reg_ext <= '1';
           end if;
        end if;
        if (aclr = '1') then
           dataout_h <= (others => '0');
           dataout_l <= (others => '0');
              oe_rgd <= '0';
              oe_reg_ext <= '0';
        elsif (aset = '1') then
           dataout_h <= (others => '1');
           dataout_l <= (others => '1');
              oe_rgd <= '1';
              oe_reg_ext <= '1';
        elsif (outclocken = '1') then
           if (outclock = '1') then
              dataout_h <= datain_H;
              dataout_l <= datain_L;
              oe_rgd <= oe;
           else
              oe_reg_ext <= oe_rgd;
           end if;
        end if;
end process;

process(outclock, dataout_h, dataout_l, output_enable)
begin
        if output_enable = '1' then
            if (outclock = '1') then
                dataout <= dataout_h;
            else
                dataout <= dataout_l;
            end if;
        else
           dataout <= (others => 'Z');
        end if;
end process;

end behave;
-------------------------------------------------------------
--  		ALTDDIO_BIDIR 
-------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;

entity altddio_bidir is
	generic(width : positive := 1; 
                intended_device_family : string := "MERCURY";
                power_up_high : string := "OFF";
                oe_reg : string := "UNUSED";
                implement_input_in_lcell : string := "UNUSED";
                extend_oe_disable : string := "UNUSED");
	port (datain_H : in std_logic_vector(width-1 downto 0);
	      datain_L : in std_logic_vector(width-1 downto 0);
	      inclock : in std_logic;
	      inclocken : in std_logic := '1';
	      outclock : in std_logic;
	      outclocken : in std_logic := '1';
	      aset : in std_logic := '0';
	      aclr : in std_logic := '0';
	      oe : in std_logic := '1';
	      combout : out std_logic_vector(width-1 downto 0);
	      dataout_H : out std_logic_vector(width-1 downto 0);
	      dataout_L : out std_logic_vector(width-1 downto 0);
	      padio : inout std_logic_vector(width-1 downto 0));

end altddio_bidir;
architecture struct of altddio_bidir is

component altddio_in
	generic(width : positive := 8;
                intended_device_family : string := "MERCURY";
                power_up_high : string := "OFF");
        port (datain : in std_logic_vector(width-1 downto 0);
              inclock : in std_logic;
              inclocken : in std_logic := '1';
	      aset : in std_logic := '0';
	      aclr : in std_logic := '0';
              dataout_H : out std_logic_vector(width-1 downto 0);
              dataout_L : out std_logic_vector(width-1 downto 0));
end component;

component altddio_out
        generic (width : positive := 8;
                power_up_high : string := "OFF";
                intended_device_family : string := "MERCURY";
                oe_reg : string := "UNUSED";
                extend_oe_disable : string := "UNUSED");
        port (datain_H : in std_logic_vector(width-1 downto 0);
              datain_L : in std_logic_vector(width-1 downto 0);
              outclock : in std_logic;
              outclocken : in std_logic := '1';
	      aset : in std_logic := '0';
	      aclr : in std_logic := '0';
	      oe : in std_logic := '1';
              dataout : out std_logic_vector(width-1 downto 0));
end component;

begin


u1: altddio_in 
	generic map (width => width,
		     intended_device_family => intended_device_family,
                     power_up_high => power_up_high)
	port map (datain => padio, inclock => inclock,
                  inclocken => inclocken, aset => aset, aclr => aclr,
                  dataout_H => dataout_H, dataout_L => dataout_L);

u2: altddio_out 
	generic map (width => width,
                     power_up_high => power_up_high,
		     intended_device_family => intended_device_family,
                     oe_reg => oe_reg,
                     extend_oe_disable => extend_oe_disable)
     	port map (datain_H => datain_H, datain_L => datain_L, 
                  outclock => outclock, outclocken => outclocken, 
                  aset => aset, aclr => aclr, oe => oe, dataout => padio);
	combout <= padio;
end struct;
------------------------------------------------------------------
-- 			HSSI_PLL
------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.all;

entity hssi_pll is
    generic
      ( clk0_multiply_by        : integer := 1;
        clk1_divide_by          : integer := 1;
        input_frequency         : integer := 1000);
    port
      ( clk    : in std_logic := '0';
        areset : in std_logic := '0';
        clk0   : out std_logic;
        clk1   : out std_logic;
        locked : out std_logic );
end hssi_pll;

architecture hssi_pll of hssi_pll is

SIGNAL clk0_period, clk1_period, half_inclk : time;
SIGNAL clk0_phase_delay, clk1_phase_delay : time := 0 ps;
SIGNAL new_clk0, new_clk1 : std_logic;
SIGNAL start_outclk : std_logic;
SIGNAL pll_lock : std_logic := '0';
SIGNAL lock_on_rise, lock_on_fall : integer := 0;
SIGNAL clk_check : std_logic := '0';

begin

process (clk, pll_lock, clk_check)
variable expected_cycle, real_cycle : real;
variable inclk_ps : time;
variable violation : boolean := false;
variable pll_lock_tmp : std_logic := '0';
variable start_lock_count, stop_lock_count : integer := 0;
variable pll_last_rising_edge, pll_last_falling_edge : time := 0 ps;
variable pll_rising_edge_count : integer := 0;
variable pll_cycle, pll_duty_cycle : time ;
variable clk_per_tolerance : time;
variable expected_next_clk_edge : time := 0 ps;
variable lock_low : integer := 2;
variable lock_high : integer := 2;
begin
    if (areset = '1') then
        pll_rising_edge_count := 0;
        violation := false;
        pll_lock_tmp := '0';
        pll_lock <= '0';
        start_lock_count := 0;
        stop_lock_count := 0;
    else
        if (clk'event and clk = '1' and now > 0 ns) then
            if (pll_lock_tmp = '1') then
                clk_check <= not clk_check after (inclk_ps+clk_per_tolerance)/2.0;
            end if;
            if pll_rising_edge_count = 0 then      -- at 1st rising edge
                inclk_ps := (input_frequency / 1 ) * 1 ps;
                half_inclk <= inclk_ps/2;
                clk_per_tolerance := 0.025 * inclk_ps;
                clk0_period <= inclk_ps / clk0_multiply_by;
                clk1_period <= inclk_ps / clk0_multiply_by * clk1_divide_by;
                pll_duty_cycle := inclk_ps/2;
                start_outclk <= '0';
                clk0_phase_delay <= 0 ps;
                clk1_phase_delay <= 0 ps;
    
            elsif pll_rising_edge_count = 1 then      -- at 2nd rising edge
                pll_cycle := now - pll_last_rising_edge;    -- calculate period
                expected_cycle := real(input_frequency) / 1000.0;
                real_cycle := REAL( (NOW - pll_last_rising_edge) / 1 ns);
                if ((NOW - pll_last_rising_edge) < (inclk_ps - clk_per_tolerance)  or
                    (NOW - pll_last_rising_edge) > (inclk_ps + clk_per_tolerance)) then
                    ASSERT FALSE
                    REPORT " Inclock_Period Violation "
                    SEVERITY WARNING;
                    violation := true;
                    if (pll_lock = '1') then
                        stop_lock_count := stop_lock_count + 1;
                        if (stop_lock_count = lock_low) then
                            pll_lock_tmp := '0';
                        end if;
                    end if;
                else
                    violation := false;
                end if;
                if ((now - pll_last_falling_edge) < (pll_duty_cycle - clk_per_tolerance/2) or
                    (now - pll_last_falling_edge) > (pll_duty_cycle + clk_per_tolerance/2)) then
                    ASSERT FALSE
                    REPORT "Duty Cycle Violation"
                    SEVERITY WARNING;
                    violation := true;
                else
                    violation := false;
                end if;
            else
                if ((now - pll_last_rising_edge) < (inclk_ps - clk_per_tolerance) or
                    (now - pll_last_rising_edge) > (inclk_ps + clk_per_tolerance)) then
                    ASSERT FALSE
                    REPORT "Cycle Violation"
                    SEVERITY WARNING;
                    violation := true;
                    if (pll_lock = '1') then
                        stop_lock_count := stop_lock_count + 1;
                        if (stop_lock_count = lock_low) then
                            pll_lock_tmp := '0';
                        end if;
                    end if;
                else
                    violation := false;
                end if;
            end if;
            pll_last_rising_edge := now;
            pll_rising_edge_count := pll_rising_edge_count +1;
        
            if (not violation) then
                start_lock_count := start_lock_count + 1;
            else
                start_lock_count := 0;
            end if;
        
            if (start_lock_count = (lock_high + 1)) then
                pll_lock_tmp := '1';
                lock_on_rise <= 1;
            end if;
        elsif (clk'event and clk= '0' and now > 0 ns) then
            if (pll_lock_tmp = '1') then
                clk_check <= not clk_check after (inclk_ps+clk_per_tolerance)/2.0;
            end if;
            if (now > 0 ns and ((now - pll_last_rising_edge) < (pll_duty_cycle - clk_per_tolerance/2) or
               (now - pll_last_rising_edge) > (pll_duty_cycle + clk_per_tolerance/2))) then
                ASSERT FALSE
                REPORT "Duty Cycle Violation"
                SEVERITY WARNING;
                violation := true;
                if (pll_lock = '1') then
                    stop_lock_count := stop_lock_count + 1;
                    if (stop_lock_count = lock_low) then
                        pll_lock_tmp := '0';
                    end if;
                end if;
            else
                violation := false;
            end if;
            pll_last_falling_edge := now;
            if (not violation) then
                start_lock_count := start_lock_count + 1;
            else
                start_lock_count := 0;
            end if;
            if (start_lock_count = (lock_high + 1)) then
                pll_lock_tmp := '1';
                lock_on_fall <= 1;
            end if;
        else
            if pll_lock_tmp = '1' then
                if (clk = '1') then
                    expected_next_clk_edge := pll_last_rising_edge + (inclk_ps+clk_per_tolerance)/2.0;
                else
                    expected_next_clk_edge := pll_last_falling_edge + (inclk_ps+clk_per_tolerance)/2.0;
                end if;
                violation := false;
                if (now < expected_next_clk_edge) then
                    clk_check <= not clk_check after (expected_next_clk_edge - now);
                elsif (now = expected_next_clk_edge) then
                    clk_check <= not clk_check after (inclk_ps+clk_per_tolerance)/2.0;
                else
                    ASSERT FALSE
                    REPORT "Inclock_Period Violation"
                    SEVERITY WARNING;
                    violation := true;
                    if (pll_lock = '1') then
                        stop_lock_count := stop_lock_count + 1;
                        if (stop_lock_count = lock_low) then
                            pll_lock_tmp := '0';
                        else
                            clk_check <= not clk_check after (inclk_ps/2.0);
                        end if;
                    end if;
                end if;
            end if;
        end if;
        pll_lock <= pll_lock_tmp;
        if (pll_lock'event and pll_lock = '0') then
            start_lock_count := 0;
            stop_lock_count := 0;
            lock_on_rise <= 0;
            lock_on_fall <= 0;
        end if;
    end if;  -- if not areset
end process;

process (new_clk0, pll_lock)
variable first_clk0_cycle : boolean := true;
variable clk0_tmp : std_logic;
begin
    if (areset = '1') then
        clk0_tmp := 'X';
    else
        if (pll_lock = '0') then
            first_clk0_cycle := true;
            new_clk0 <= start_outclk;
        elsif (pll_lock = '1') then
            if (first_clk0_cycle) then
                first_clk0_cycle := false;
    --          clk0_tmp := start_outclk;
                if (lock_on_rise = 1) then
                    new_clk0 <= not start_outclk after clk0_phase_delay;
                elsif (lock_on_fall = 1) then
                    new_clk0 <= not start_outclk after (clk0_phase_delay + half_inclk);
                end if;
            else
                clk0_tmp := new_clk0;
                new_clk0 <= not new_clk0 after (clk0_period/2.0);
            end if;
        end if;
    end if;
clk0 <= clk0_tmp;
end process;

process (new_clk1, pll_lock)
variable first_clk1_cycle : boolean := true;
variable clk1_tmp : std_logic;
begin
    if (areset = '1') then
        clk1_tmp := 'X';
    else
        if (pll_lock = '0') then
            first_clk1_cycle := true;
            new_clk1 <= start_outclk;
        elsif (pll_lock = '1') then
            if (first_clk1_cycle) then
                first_clk1_cycle := false;
    --          clk1_tmp := start_outclk;
                if (lock_on_rise = 1) then
                    new_clk1 <= not start_outclk after clk1_phase_delay;
                elsif (lock_on_fall = 1) then
                    new_clk1 <= not start_outclk after (clk1_phase_delay + half_inclk);
                end if;
            else
                clk1_tmp := new_clk1;
                new_clk1 <= not new_clk1 after (clk1_period/2.0);
            end if;
        end if;
    end if;
clk1 <= clk1_tmp;
end process;

locked <= pll_lock;

end hssi_pll;
------------------------------------------------------------------------
-- ram7x20_syn
--
library IEEE;
use IEEE.std_logic_1164.all;

entity ram7x20_syn is
	generic (
		ram_width					: integer := 20
	);	

	port (
		wclk	: in std_logic;
		rstl	: in std_logic := '1'; 
		waddr : in std_logic_vector(2 downto 0);
		raddr : in std_logic_vector(2 downto 0);
		datain	: in std_logic_vector(ram_width-1 downto 0);
		we			: in std_logic := '0';
		re			: in std_logic := '0';
		dataout	: out std_logic_vector(ram_width-1 downto 0)
	);

end ram7x20_syn;


architecture hssi_ram7x20_syn of ram7x20_syn is
	signal ram_array_d_0 : std_logic_vector(ram_width-1 downto 0) := (OTHERS => '0');
	signal ram_array_d_1 : std_logic_vector(ram_width-1 downto 0) := (OTHERS => '0');
	signal ram_array_d_2 : std_logic_vector(ram_width-1 downto 0) := (OTHERS => '0');
	signal ram_array_d_3 : std_logic_vector(ram_width-1 downto 0) := (OTHERS => '0');
	signal ram_array_d_4 : std_logic_vector(ram_width-1 downto 0) := (OTHERS => '0');
	signal ram_array_d_5 : std_logic_vector(ram_width-1 downto 0) := (OTHERS => '0');
	signal ram_array_d_6 : std_logic_vector(ram_width-1 downto 0) := (OTHERS => '0');
	signal ram_array_q_0 : std_logic_vector(ram_width-1 downto 0) := (OTHERS => '0');
	signal ram_array_q_1 : std_logic_vector(ram_width-1 downto 0) := (OTHERS => '0');
	signal ram_array_q_2 : std_logic_vector(ram_width-1 downto 0) := (OTHERS => '0');
	signal ram_array_q_3 : std_logic_vector(ram_width-1 downto 0) := (OTHERS => '0');
	signal ram_array_q_4 : std_logic_vector(ram_width-1 downto 0) := (OTHERS => '0');
	signal ram_array_q_5 : std_logic_vector(ram_width-1 downto 0) := (OTHERS => '0');
	signal ram_array_q_6 : std_logic_vector(ram_width-1 downto 0) := (OTHERS => '0');
	signal data_reg_0 : std_logic_vector(ram_width-1 downto 0) := (OTHERS => '0');
	signal data_reg_1 : std_logic_vector(ram_width-1 downto 0) := (OTHERS => '0');
	signal data_reg_2 : std_logic_vector(ram_width-1 downto 0) := (OTHERS => '0');
	signal data_reg_3 : std_logic_vector(ram_width-1 downto 0) := (OTHERS => '0');
	signal data_reg_4 : std_logic_vector(ram_width-1 downto 0) := (OTHERS => '0');
	signal data_reg_5 : std_logic_vector(ram_width-1 downto 0) := (OTHERS => '0');
	signal data_reg_6 : std_logic_vector(ram_width-1 downto 0) := (OTHERS => '0');
	signal data_out_i : std_logic_vector(ram_width-1 downto 0);

begin
	
	-- Modelling the read port 
	-- Assuming address trigerred operation only 
	--assignment
	
	data_reg_0 <= datain when waddr = "000" else
						ram_array_q_0;
	data_reg_1 <= datain when waddr = "001" else
						ram_array_q_1;
	data_reg_2 <= datain when waddr = "010" else
						ram_array_q_2;
	data_reg_3 <= datain when waddr = "011" else
						ram_array_q_3;
	data_reg_4 <= datain when waddr = "100" else
						ram_array_q_4;
	data_reg_5 <= datain when waddr = "101" else
						ram_array_q_5;
	data_reg_6 <= datain when waddr = "110" else
						ram_array_q_6;

	dataout <= data_out_i when re = '1' else
						(OTHERS => '0');

	process (wclk, rstl, waddr, raddr, datain, ram_array_q_0, ram_array_q_1, ram_array_q_2, ram_array_q_3, ram_array_q_4, ram_array_q_5, ram_array_q_6, data_reg_0, data_reg_1, data_reg_2, data_reg_3, data_reg_4, data_reg_5, data_reg_6)
	variable dataout_tmp : std_logic_vector(ram_width-1 downto 0);
	begin

		case raddr is  
			when "000" => data_out_i <= ram_array_q_0;
			when "001" => data_out_i <= ram_array_q_1;
			when "010" => data_out_i <= ram_array_q_2;
			when "011" => data_out_i <= ram_array_q_3;
			when "100" => data_out_i <= ram_array_q_4;
			when "101" => data_out_i <= ram_array_q_5;
			when "110" => data_out_i <= ram_array_q_6;
			when others => data_out_i <= data_out_i;
		end case;

		if (re = '1') then
			dataout_tmp := data_out_i;
		else
			dataout_tmp := (OTHERS => '0');
		end if;

		--Modelling the write port 
		if (rstl = '0') then
			ram_array_q_0 <= (OTHERS => '0');
			ram_array_q_1 <= (OTHERS => '0');
			ram_array_q_2 <= (OTHERS => '0'); 
			ram_array_q_3 <= (OTHERS => '0'); 
			ram_array_q_4 <= (OTHERS => '0'); 
			ram_array_q_5 <= (OTHERS => '0'); 
			ram_array_q_6 <= (OTHERS => '0'); 
		else 
			if (wclk'event and wclk = '1') then
				ram_array_q_0 <= ram_array_d_0;
				ram_array_q_1 <= ram_array_d_1;
				ram_array_q_2 <= ram_array_d_2;
				ram_array_q_3 <= ram_array_d_3;
				ram_array_q_4 <= ram_array_d_4;
				ram_array_q_5 <= ram_array_d_5;
				ram_array_q_6 <= ram_array_d_6;
	  		end if;
			if (we = '1') then
				ram_array_d_0 <= data_reg_0;
				ram_array_d_1 <= data_reg_1;
				ram_array_d_2 <= data_reg_2;
				ram_array_d_3 <= data_reg_3;
				ram_array_d_4 <= data_reg_4;
				ram_array_d_5 <= data_reg_5;
				ram_array_d_6 <= data_reg_6;
	  		else 
				ram_array_d_0 <= ram_array_q_0;
				ram_array_d_1 <= ram_array_q_1;
				ram_array_d_2 <= ram_array_q_2;
				ram_array_d_3 <= ram_array_q_3;
				ram_array_d_4 <= ram_array_q_4;
				ram_array_d_5 <= ram_array_q_5;
				ram_array_d_6 <= ram_array_q_6;
	  		end if;
		end if;


	end process;

end hssi_ram7x20_syn;

----------------------------------------------------------------
-- 			hssi_fifo
----------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.all;

entity hssi_fifo is
	generic (
		channel_width           : integer := 20);

	port (
			clk0		: in std_logic;
			clk1		: in std_logic;
			datain	: in std_logic_vector(channel_width-1 downto 0);
			we			: in std_logic := '1';
			re			: in std_logic := '1';
			reset		: in std_logic := '0';
			devclrn	: in std_logic := '1';
			devpor	: in std_logic := '1';
			empty		: out std_logic;
			overflow	: out std_logic;
			dataout	: out std_logic_vector(channel_width-1 downto 0));


end hssi_fifo;

architecture synchronizer of hssi_fifo is
	signal ram_we: std_logic;
	signal ram_reset, ram_re : std_logic;
	signal ram_datain : std_logic_vector (channel_width-1 downto 0);
	signal ram_dataout : std_logic_vector (channel_width-1 downto 0);
	signal wrPtr0 : std_logic_vector (2 downto 0) := "000";
	signal wrPtr1 : std_logic_vector (2 downto 0) := "000";
	signal wrPtr2 : std_logic_vector (2 downto 0) := "000";
	signal wrPtr : std_logic_vector (2 downto 0) := "000";
	signal rdPtr : std_logic_vector (2 downto 0) := "000";
	signal preRdPtr : std_logic_vector (2 downto 0) := "111";
	signal preRdPtr1 : std_logic_vector (2 downto 0) := "111";
	signal preRdPtr2 : std_logic_vector (2 downto 0) := "111";
	signal wrAddr : std_logic_vector (2 downto 0) := "000";
	signal dataout_tmp : std_logic_vector (channel_width-1 downto 0);
	signal empty_tmp : std_logic;

component ram7x20_syn
	generic (
		ram_width					: integer := 20
	);

	port (
		wclk	: in std_logic;
		rstl	: in std_logic := '1'; 
		waddr	: in std_logic_vector(2 downto 0);
		raddr	: in std_logic_vector(2 downto 0);
		datain	: in std_logic_vector(ram_width-1 downto 0);
		we			: in std_logic := '0';
		re			: in std_logic := '0';
		dataout	: out std_logic_vector(ram_width-1 downto 0)
	);
end component;

begin

	ram_reset <= NOT reset;
	ram_re <= re AND empty_tmp;

	-- ram7x20_syn
	fifo_ram: ram7x20_syn
					generic map (ram_width => channel_width)
					port map (wclk => clk0, rstl => ram_reset, waddr => wrAddr, raddr => rdPtr, datain => ram_datain, we => ram_we, re => ram_re, dataout => ram_dataout);

	process (clk0, clk1, reset, we, re, datain, devclrn, devpor, wrPtr, wrPtr0, wrPtr1, wrPtr2, rdPtr, preRdPtr, preRdPtr1, preRdPtr2)  

	variable overflow_tmp : std_logic;
	variable empty_tmp_var : std_logic;


	begin
		if (now = 0 ns) then
			dataout_tmp <= (OTHERS => '0');
			ram_datain <= (OTHERS => '0');
			overflow_tmp :=  '1';
			empty_tmp <=  '1';
			empty_tmp_var :=  '1';
			wrAddr <=  "000";
			rdPtr <=  "000";
			wrPtr <=  "000";
			wrPtr0 <=  "000";
			wrPtr1 <=  "000";
			wrPtr2 <=  "000";
			preRdPtr <=  "111";
			preRdPtr1 <=  "111";
			preRdPtr2 <=  "111";
			ram_we <= '0';
		end if;

		if ((devpor = '0') or (devclrn = '0')) then
			dataout_tmp <= (OTHERS => '0');
			ram_datain <= (OTHERS => '0');
			overflow_tmp :=  '0';
			empty_tmp <=  '0';
			empty_tmp_var :=  '0';
			wrAddr <=  "000";
			rdPtr <=  "000";
			wrPtr <=  "000";
			wrPtr0 <=  "000";
			wrPtr1 <=  "000";
			wrPtr2 <=  "000";
			preRdPtr <=  "111";
			preRdPtr1 <=  "111";
			preRdPtr2 <=  "111";
			ram_we <= '0';
		end if;
		if (reset = '1') then
			dataout_tmp <= (OTHERS => '0');
			ram_datain <= (OTHERS => '0');
			overflow_tmp :=  '0';
			empty_tmp <=  '0';
			empty_tmp_var :=  '0';
			wrAddr <=  "000";
			rdPtr <=  "000";
			wrPtr <=  "000";
			wrPtr0 <=  "000";
			wrPtr1 <=  "000";
			wrPtr2 <=  "000";
			preRdPtr <=  "111";
			preRdPtr1 <=  "111";
			preRdPtr2 <=  "111";
			ram_we <= '0';
		else
			if (clk1'event and clk1 = '1') then
				if (re = '1' and empty_tmp_var = '1') then
					dataout_tmp <= ram_dataout;
					case rdPtr is
						when "000" => rdPtr <= "001";
						when "001" => rdPtr <= "010";
						when "010" => rdPtr <= "011";
						when "011" => rdPtr <= "100";
						when "100" => rdPtr <= "101";
						when "101" => rdPtr <= "110";
						when "110" => rdPtr <= "000";
						when others => rdPtr <= "000";
					end case;
					preRdPtr <= rdPtr;
				else
					dataout_tmp <= dataout_tmp;
				end if;
				--synchronize write pointers.
				wrPtr1 <= wrPtr;
				wrPtr2 <= wrPtr1;
			end if;
			if (clk0'event and clk0 = '1') then
				wrPtr <= wrPtr0;
				if (we = '1' and overflow_tmp = '1') then
					ram_we <= '1';
					ram_datain <= datain;
					wrAddr <= wrPtr0;
					case wrPtr0 is
						when "000" => wrPtr0 <= "001";
						when "001" => wrPtr0 <= "010";
						when "010" => wrPtr0 <= "011";
						when "011" => wrPtr0 <= "100";
						when "100" => wrPtr0 <= "101";
						when "101" => wrPtr0 <= "110";
						when "110" => wrPtr0 <= "000";
						when others => wrPtr0 <= "000";
					end case;
				else
					ram_we <= '0';
					wrAddr <= wrAddr;
					ram_datain <= ram_datain;
					wrPtr0 <= wrPtr0;
				end if;
				--synchronize read pointers.
				preRdPtr1 <= preRdPtr;
				preRdPtr2 <= preRdPtr1;
			end if;

			if (wrPtr0 = preRdPtr2) then
				overflow_tmp := '0';
			else
				overflow_tmp := '1';
			end if;
			if (rdPtr = wrPtr2) then
				empty_tmp_var := '0';
			else
				empty_tmp_var := '1';
			end if;
		
		end if;


	empty_tmp <= empty_tmp_var;
        overflow <= overflow_tmp;
	end process;
        empty <= empty_tmp;
        dataout <= dataout_tmp;

end synchronizer;
-------------------------------------------------------------
--			 HSSI_RX
-------------------------------------------------------------
--
-- MERCURY_HSSI_RECEIVER
--

library IEEE, std;
use IEEE.std_logic_1164.all;

entity hssi_rx is
    generic (
                channel_width           : integer := 20;
                operation_mode          : String  := "lvds";
                run_length  	          : integer := 1);

        port (
					clk		: in std_logic;
					coreclk		: in std_logic;
					datain		: in std_logic;
					areset		: in std_logic := '0';
					feedback	: in std_logic := '0';
					fbkcntl		: in std_logic := '0';
					devclrn		: in std_logic := '1';
					devpor		: in std_logic := '1';
					clkout		: out std_logic;
					rlv		: out std_logic;
					locked		: out std_logic;
					dataout		: out std_logic_vector(channel_width-1 downto 0));
end hssi_rx;

architecture vital_receiver_atom of hssi_rx is
signal databuf: std_logic;

begin

VITAL: process (clk, coreclk, devpor, devclrn, areset, fbkcntl)
variable clk_count : integer := channel_width; --follow the 1st edge
variable deser_data_arr : std_logic_vector(channel_width-1 downto 0);
variable dataout_tmp : std_logic_vector(channel_width-1 downto 0);
variable clkout_tmp : std_logic;
variable rlv_tmp : std_logic := '0';
variable locked_tmp : std_logic := '0';
variable clkout_last_value : std_logic;
variable datain_int : std_logic;
variable last_datain : std_logic;
variable rlv_count : integer := 0; 
variable data_changed : std_logic := '0';
variable rlv_flag : std_logic := '0';
variable rlv_set : std_logic := '0';

begin

	if (now = 0 ns) then
		dataout_tmp := (OTHERS => '0');
		clkout_last_value := clk;
		rlv_tmp :=  '0';
		clkout_tmp :=  '0';
		data_changed :=  '0';
		clk_count := channel_width;
	end if;


	if ((devpor = '0') or (devclrn = '0')) then
		dataout_tmp := (OTHERS => '0');
		clkout_last_value := clk;
		rlv_tmp :=  '0';
		clkout_tmp :=  '0';
		rlv_count := 0;
		rlv_flag := '0';
		rlv_set := '0';
		data_changed :=  '0';
		last_datain := 'X';
	end if;
	if (areset = '1') then
		dataout_tmp := (OTHERS => '0');
		clkout_tmp :=  '0';
		clk_count := channel_width;
		rlv_tmp := '0';
		locked_tmp := '0';
		clkout_last_value := clk;
      for i in channel_width-1 downto 0 loop
			deser_data_arr(i) := '0';
      end loop;
		rlv_count := 0;
		rlv_flag := '0';
		rlv_set := '0';
		last_datain := 'X';
		data_changed := '0';
	else
		if (fbkcntl = '1') then
			datain_int := feedback;
		else
			datain_int := datain;
		end if;
	   if (clk'event and clk = '1') then
			if (clkout_last_value = 'U') then
				clkout_last_value := clk;
				clkout_tmp := clk;
			end if;
			if (clk_count = channel_width) then
				clk_count :=0;
				clkout_tmp := NOT (clkout_last_value);
			elsif (clk_count = (channel_width+1)/2) then
				clkout_tmp := NOT (clkout_last_value);
			elsif (clk_count < channel_width) then
				clkout_tmp := clkout_last_value;
			end if;
			clk_count := clk_count + 1;
			if (operation_mode = "cdr") then
				if (last_datain /= datain_int) then
					data_changed := '1';
					last_datain := datain_int;
				else
					rlv_count := rlv_count + 1;
					data_changed := '0';
				end if;
				if (rlv_count > run_length) then
					rlv_set := '1';
					rlv_flag := '1';
				else
					rlv_set := '0';
				end if;
				if (data_changed = '1') then
					rlv_count := 1;
				end if;
			end if;
	   end if;
	   if (coreclk'event and coreclk = '1') then
			if (operation_mode = "cdr") then
				if (rlv_flag = '1') then
					rlv_tmp := '1';
					if (rlv_set = '0') then
						rlv_flag := '0';
					end if;
				else
					rlv_tmp := '0';
				end if;
			end if;
	   end if;
	   if (clk'event and clk = '0') then
			if (clkout_last_value = 'U') then
				clkout_last_value := clk;
				clkout_tmp := clk;
			end if;
	      if (clk_count = 3) then
				dataout_tmp(channel_width-1 downto 0) := deser_data_arr;
	      end if;
	      for i in channel_width-1 downto 1 loop
				deser_data_arr(i) := deser_data_arr(i-1);
	      end loop;
	      deser_data_arr(0) := datain_int;
	   end if;

		if (clkout_tmp /= 'U') then
			clkout_last_value := clkout_tmp;
		end if;
	end if;
clkout <= clkout_tmp;
dataout <= dataout_tmp;
rlv <= rlv_tmp;
locked <= locked_tmp;

end process;

end vital_receiver_atom;


----------------------------------------------------------------------
-- 				HSSI_TX
----------------------------------------------------------------------
library IEEE;
use IEEE.std_logic_1164.all;

entity hssi_tx is
    generic (
                channel_width           : integer := 20);

        port (
                clk             : in std_logic;
                areset          : in std_logic := '0';
                datain          : in std_logic_vector(channel_width-1 downto 0);
                devclrn         : in std_logic := '1';
					 devpor			  : in std_logic := '1';
                clkout          : out std_logic;
                dataout         : out std_logic);

end hssi_tx;

architecture transmitter of hssi_tx is

begin

process (clk, areset, devclrn, devpor)  -- need to generate clkout here
variable i : integer := 0;
variable dataout_tmp : std_logic := '0';
variable indata : std_logic_vector(channel_width-1 downto 0);
variable regdata : std_logic_vector(channel_width-1 downto 0);
variable fast_clk_count : integer := channel_width; -- always follow the first edge
variable clkout_tmp : std_logic;
variable clkout_last_value : std_logic;
begin

	if (now = 0 ns) then
		dataout_tmp := '0';
		clkout_last_value := clk;
   end if;

   if ((devpor = '0') or (devclrn = '0') or (areset = '1')) then
		dataout_tmp := '0';
		clkout_tmp := '0';
		fast_clk_count := channel_width;
      for i in channel_width-1 downto 0 loop --reset register
			indata(i) := '0';
			regdata(i) := '0';
		end loop;
   else
      if (clk'event and clk = '1') then
      	if (clkout_last_value = 'U') then --for initial value
				clkout_last_value := clk;
				clkout_tmp := clk;
			end if;
			if (fast_clk_count = channel_width) then
				fast_clk_count := 0;
				clkout_tmp := NOT (clkout_last_value);
			elsif (fast_clk_count = (channel_width+1)/2) then
				clkout_tmp := NOT (clkout_last_value);
			elsif (fast_clk_count < channel_width) then
				clkout_tmp := clkout_last_value;
	      end if;
			fast_clk_count := fast_clk_count + 1;
			if (fast_clk_count = 3) then --shifting out new data 
         	for i in channel_width-1 downto 0 loop
					regdata(i) := indata(i);
				end loop;
			end if;
         dataout_tmp := regdata(channel_width - 1);
         for i in channel_width-1 downto 1 loop
				regdata(i) := regdata(i-1);
			end loop;
      end if;
      if (clk'event and clk = '0') then  -- falling edge
      	if (clkout_last_value = 'U') then --for initial value
				clkout_last_value := clk;
				clkout_tmp := clk;
			end if;
			if (fast_clk_count = 3) then --loading 
				indata := datain(channel_width-1 downto 0);
			end if;
      end if;
   end if;

	if (clkout_tmp /= 'U') then
		clkout_last_value := clkout_tmp;
	end if;

	dataout <= dataout_tmp;
        clkout <= clkout_tmp;
        
end process;

end transmitter;
------------------------------------------------------------------------
--			ALTCDR_RX
------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;

entity altcdr_rx is
	generic (
		number_of_channels: positive := 1;
		deserialization_factor : positive := 1;
		inclock_period : positive;
		run_length : integer := 62;
		inclock_boost : positive := 1;
                bypass_fifo : string := "OFF";
		intended_device_family : string := "MERCURY");
	port (
		rx_in : in std_logic_vector(number_of_channels-1 downto 0);
		rx_inclock : in std_logic;
		rx_coreclock : in std_logic;
		rx_fifo_rden : in std_logic_vector(number_of_channels-1 downto 0) := (others => '1');
		rx_aclr : in std_logic := '0';
		rx_pll_aclr : in std_logic := '0';
		rx_out : out std_logic_vector(deserialization_factor*number_of_channels-1 downto 0);
		rx_outclock : out std_logic;
		rx_rec_clk : out std_logic_vector(number_of_channels-1 downto 0);
		rx_locklost : out std_logic_vector(number_of_channels-1 downto 0);
		rx_pll_locked : out std_logic;
		rx_empty : out std_logic_vector(number_of_channels-1 downto 0);
		rx_full : out std_logic_vector(number_of_channels-1 downto 0);
		rx_rlv : out std_logic_vector(number_of_channels-1 downto 0));
end altcdr_rx;	

architecture struct of altcdr_rx is

begin

process
begin
        assert FALSE report
"CDR functionality disabled. Designing with CDR can be support intensive and requires close engagement with Altera early in the design cycle. Please contact your Altera representative to discuss support requirements."
        severity error;
        wait;
end process;

end struct;
----------------------------------------------------------------------
--			ALTCDR_TX
----------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;

entity altcdr_tx is
	generic (
		number_of_channels: positive := 1;
		deserialization_factor : positive := 1;
		inclock_period : positive;
		inclock_boost : positive := 1;
                bypass_fifo : string := "OFF";
		intended_device_family : string := "MERCURY");
	port (
		tx_in : in std_logic_vector(deserialization_factor*number_of_channels-1 downto 0);
		tx_inclock : in std_logic;
		tx_coreclock : in std_logic;
		tx_fifo_wren : in std_logic_vector(number_of_channels-1 downto 0) := (others => '1');
		tx_aclr : in std_logic := '0';
		tx_pll_aclr : in std_logic := '0';
		tx_out : out std_logic_vector(number_of_channels-1 downto 0);
		tx_outclock : out std_logic;
		tx_pll_locked : out std_logic;
		tx_empty : out std_logic_vector(number_of_channels-1 downto 0);
		tx_full : out std_logic_vector(number_of_channels-1 downto 0));
end altcdr_tx;	

architecture struct of altcdr_tx is

begin

process
begin
        assert FALSE report
"CDR functionality disabled. Designing with CDR can be support intensive and requires close engagement with Altera early in the design cycle. Please contact your Altera representative to discuss support requirements."
        severity error;
        wait;
end process;

end struct;

--------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

entity LCELL is
   port(
      A_IN                           :  in    std_logic;
      A_OUT                          :  out   std_logic);
end LCELL;

architecture BEHAVIOR of LCELL is
begin
        A_OUT <= A_IN;
end BEHAVIOR;


library ieee;
use ieee.std_logic_1164.all;

entity GLOBAL is
   port(
      A_IN                           :  in    std_logic;
      A_OUT                          :  out   std_logic);
end GLOBAL;

architecture BEHAVIOR of GLOBAL is
begin
        A_OUT <= A_IN;
end BEHAVIOR;

library ieee;
use ieee.std_logic_1164.all;

entity CARRY is
   port(
      A_IN                           :  in    std_logic;
      A_OUT                          :  out   std_logic);
end CARRY;

architecture BEHAVIOR of CARRY is
begin
        A_OUT <= A_IN;
end BEHAVIOR;


library ieee;
use ieee.std_logic_1164.all;

entity CASCADE is
   port(
      A_IN                           :  in    std_logic;
      A_OUT                          :  out   std_logic);
end CASCADE;

architecture BEHAVIOR of CASCADE is
begin
        A_OUT <= A_IN;
end BEHAVIOR;

--------------------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;

entity altaccumulate is
		generic (width_in:integer:=4;
						width_out:integer:=8;
						lpm_representation: string := "SIGNED";
						extra_latency:integer:=0;
						use_wys:string := "ON";
						lpm_hint : string := "bogus");
						
    port (data : in std_logic_vector(width_in -1 downto 0) ;
				clock    : in std_logic;
				clken  : in std_logic := '1';
				sload  : in std_logic := '0';
				aclr  : in std_logic := '0';
				add_sub : in std_logic := '1';
				sign_data : in std_logic := '0';
				cin : in std_logic := 'Z';
				result  : out std_logic_vector(width_out -1 downto 0) := (others => '0') ;
				cout  : out std_logic := '0';
				overflow  : out std_logic := '0');
end altaccumulate;

architecture behaviour of altaccumulate is


signal temp_sum : std_logic_vector (width_out downto 0):= (others => '0');
signal cout_int :std_logic:='0';
signal overflow_int: std_logic :='0';
signal result_int: std_logic_vector (width_out +1 downto 0) := (others => '0');

type pipeline is array (extra_latency-1 downto 0) of std_logic_vector (width_out +1 downto 0); 

signal result_pipe: pipeline := (others => (others => '0'));

signal head: integer := 0;

begin

addsub: process (data, add_sub, sload, cin, sign_data, result_int (width_out-1 downto 0))
variable fb_int : std_logic_vector (width_out downto 0) := (others => '0');
variable data_int : std_logic_vector (width_out-1 downto 0) := (others => '0');
variable zeropad : std_logic_vector ((width_out - width_in)- 1 downto 0) := (others => '0');
variable temp_sum_int : std_logic_vector (width_out downto 0):= (others => '0');
variable cout_temp, borrow : std_logic;
variable result_full : std_logic_vector (width_out downto 0);
variable temp_sum_zero : std_logic_vector (width_out downto 0) := (others => '0');
variable cin_int : std_logic;
begin

if LPM_REPRESENTATION = "SIGNED" or sign_data = '1' then
		zeropad := (others => data (width_in -1));
else
		zeropad := (others => '0');
end if;

if sload ='1' then
	fb_int := (others => '0');
else
	fb_int := ('0' & result_int (width_out -1 downto 0));
end if;

if (data (0) = '1' or data (0) = '0') then
	data_int :=  (zeropad & data);
end if;

if cin /= '0' and cin /='1' then
	cin_int := not add_sub;
else
	cin_int := cin;
end if;

if sload ='1' then
	temp_sum_int := unsigned(temp_sum_zero) + unsigned(data_int);
else
	if add_sub = '1' then
		temp_sum_int :=  unsigned(temp_sum_zero) + unsigned (fb_int) + unsigned (data_int) + cin_int;
		cout_temp := temp_sum_int (width_out);
	else
		borrow := not cin_int;
		if (borrow /= '1' and borrow /= '0') then
			borrow := '0';
		end if;

		temp_sum_int := unsigned(temp_sum_zero) + unsigned (fb_int) - unsigned (data_int) - borrow;	
		result_full := unsigned(temp_sum_zero) + unsigned(data_int) + borrow;

		if fb_int >= result_full then
			cout_temp :='1';
		else
			cout_temp :='0';
		end if;
	end if;
end if;

if sload ='0' then
	if LPM_REPRESENTATION = "SIGNED" or sign_data = '1' then
		overflow_int <= ((not (data (width_in-1) xor result_int (width_out -1))) xor (not (add_sub))) and (result_int (width_out -1) xor temp_sum_int (width_out -1));
	else
		overflow_int <= not (add_sub xor cout_temp);
	end if;
else
	overflow_int <='0';
	cout_temp := not add_sub;
end if;

cout_int <= cout_temp;
temp_sum <= temp_sum_int;

end process addsub;

acc: process (clock, aclr, cout_int)
variable head_pipe : integer;
variable full_res: std_logic_vector (width_out +1 downto 0);
begin

	head_pipe := head;

	if aclr = '1' then
		result <= (others => '0');
		result_int <= (others => '0');
		cout <= '0';
		overflow <= '0';
		result_pipe <= (others => (others => '0'));
	else
		if extra_latency = 0 then
			cout <= cout_int;
		end if;
	
		if clock'event and (clock = '1' and clken = '1') then
			if extra_latency > 0 then
				result_pipe (head_pipe) <= (result_int (width_out+1) & cout_int & result_int (width_out-1 downto 0));

				head_pipe := (head_pipe +1) mod (extra_latency);			
				
				if (head_pipe = head) then
					full_res := (result_int (width_out+1) & cout_int & result_int (width_out-1 downto 0));
				else
					full_res := result_pipe (head_pipe);
				end if;

				cout <= full_res (width_out);
				result <= full_res (width_out-1 downto 0);
				overflow <= full_res (width_out +1);
			else
				overflow <= overflow_int;
				result <= temp_sum (width_out-1 downto 0);
			end if;	

		result_int <= (overflow_int & cout_int & temp_sum (width_out-1 downto 0));	

		end if;
	end if;
	head <= head_pipe;
end process acc;

end behaviour;


--------------------------------------------------------------------------


library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;


entity altmult_accum is
		generic (width_a: integer := 3;
						width_b: integer :=3;
						width_result: integer := 10;
						input_reg_a : string := "UNREGISTERED";
						input_aclr_a : string := "NONE";
						input_reg_b: string := "UNREGISTERED";
						input_aclr_b : string := "NONE";
						addnsub_reg: string := "UNREGISTERED";
						addnsub_aclr : string := "NONE";
						addnsub_pipeline_reg: string := "UNREGISTERED";
						addnsub_pipeline_aclr : string := "NONE";
						accum_direction : string := "UNUSED";
						accum_sload_reg: string := "UNREGISTERED";
						accum_sload_aclr : string := "NONE";
						accum_sload_pipeline_reg: string := "UNREGISTERED";
						accum_sload_pipeline_aclr : string := "NONE";
						representation_a: string := "UNSIGNED";
						sign_reg_a: string := "UNREGISTERED";
						sign_aclr_a : string := "NONE";
						sign_pipeline_reg_a: string := "UNREGISTERED";
						sign_pipeline_aclr_a : string := "NONE";
						representation_b: string := "UNSIGNED";
						sign_reg_b: string := "UNREGISTERED";
						sign_aclr_b : string := "NONE";
						sign_pipeline_reg_b: string := "UNREGISTERED";
						sign_pipeline_aclr_b : string := "NONE";
						multiplier_reg: string := "UNREGISTERED";
						multiplier_aclr : string := "NONE";
						output_reg: string := "CLOCK0";
						output_aclr : string := "ACLR0";
						extra_multiplier_latency:integer :=0;
						extra_accumulator_latency:integer :=0;
						lpm_hint : string := "bogus"
						);
						
    port (dataa : in std_logic_vector(width_a -1 downto 0);
				datab : in std_logic_vector(width_b -1 downto 0);
				-- control signals
				addnsub	: in std_logic := '1';
				accum_sload : in std_logic := '0';
				signa, signb : in std_logic := '0';
				-- clock ports
				clock0, clock1, clock2, clock3    : in std_logic := '1';
				ena0, ena1, ena2, ena3 :	in std_logic := '1';
				aclr0, aclr1, aclr2, aclr3 : in std_logic := '0';
				-- output ports
				result  : out std_logic_vector(width_result -1 downto 0);
				overflow  : out std_logic:='0';
				scanouta : out std_logic_vector (width_a -1 downto 0):=(others => '0');
				scanoutb : out std_logic_vector (width_b -1 downto 0):= (others=> '0')
				);

end altmult_accum;

architecture behaviour of altmult_accum is
signal temp_sum : std_logic_vector (width_result + 1 downto 0):= (others => '0');
--signal zeropad : std_logic_vector ((width_result - width_a - width_b) downto 0) := (others => '0');
--signal answer : std_logic_vector (width_result + 1 downto 0);
signal mult_a : std_logic_vector (width_a -1 downto 0):= (others => '0');
signal mult_b : std_logic_vector (width_b -1 downto 0):= (others => '0');
signal mult_res :std_logic_vector (width_result -1 downto 0):= (others => '0');
signal acc_sload_reg, accum_sload_pipe: std_logic := '0';
signal sign_a_reg, sign_a_pipe: std_logic := '0';
signal sign_b_reg, sign_b_pipe: std_logic := '0';
signal addsub_reg, addsub_pipe: std_logic := '1';

type pipeline_accum is array (extra_accumulator_latency downto 0) of std_logic_vector (width_result downto 0); 
type pipeline_multi is array (extra_multiplier_latency downto 0) of std_logic_vector (width_result downto 0); 

signal result_pipe: pipeline_accum := (others => (others => '0'));
signal mult_pipe: pipeline_multi := (others => (others => '0'));

signal temp_mult, result_int: std_logic_vector (width_result -1 downto 0):= (others => '0');
signal temp_mult_zero: std_logic_vector (width_result -1 downto 0):= (others => '0');

signal mult_signed, do_add, temp_mult_signed : std_logic := '0';
signal head_result, head_mult : integer := 0;

begin

scanouta <= mult_a;
scanoutb <= mult_b;


-- clock and cke setup for all registers
process (clock0, clock1, clock2, clock3, aclr0, aclr1, aclr2, aclr3, dataa)

begin

if input_reg_a = "UNREGISTERED" then
	mult_a <= dataa;
else
	if (input_aclr_a= "ACLR0" and aclr0 = '1') or  
		(input_aclr_a= "ACLR1" and aclr1 = '1') or  
		(input_aclr_a= "ACLR2" and aclr2 = '1') or  
		(input_aclr_a= "ACLR3" and aclr3 = '1') then
			mult_a <= (others => '0');
	elsif (input_reg_a = "CLOCK0" and clock0= '1' and clock0'event and ena0 ='1') or
			(input_reg_a = "CLOCK1" and clock1= '1' and clock1'event and ena1 ='1') or
			(input_reg_a = "CLOCK2" and clock2= '1' and clock2'event and ena2 ='1') or
			(input_reg_a = "CLOCK3" and clock3= '1' and clock3'event and ena3 ='1') then
			mult_a <= dataa;
	end if;
end if;

end process;

process (clock0, clock1, clock2, clock3, aclr0, aclr1, aclr2, aclr3, datab)
begin
if input_reg_b = "UNREGISTERED" then
	mult_b <= datab;
else
	if (input_aclr_b= "ACLR0" and aclr0 = '1') or  
		(input_aclr_b= "ACLR1" and aclr1 = '1') or  
		(input_aclr_b= "ACLR2" and aclr2 = '1') or  
		(input_aclr_b= "ACLR3" and aclr3 = '1') then
			mult_b <= (others => '0');

	elsif (input_reg_b = "CLOCK0" and clock0= '1' and clock0'event and ena0 ='1') or
		(input_reg_b = "CLOCK1" and clock1= '1' and clock1'event and ena1 ='1') or
		(input_reg_b = "CLOCK2" and clock2= '1' and clock2'event and ena2 ='1') or
		(input_reg_b = "CLOCK3" and clock3= '1' and clock3'event and ena3 ='1') then
			mult_b <= datab;
	end if;
end if;

end process;

process (clock0, clock1, clock2, clock3, aclr0, aclr1, aclr2, aclr3, addnsub)
begin
if addnsub_reg = "UNREGISTERED" then
	addsub_reg <= addnsub;
	if (addnsub_aclr= "ACLR0" and aclr0 = '1') or  
		(addnsub_aclr= "ACLR1" and aclr1 = '1') or  
		(addnsub_aclr= "ACLR2" and aclr2 = '1') or  
		(addnsub_aclr= "ACLR3" and aclr3 = '1') then
		addsub_reg <= '0';
	elsif (addnsub_reg = "CLOCK0" and clock0= '1' and clock0'event and ena0 ='1') or
		(addnsub_reg = "CLOCK1" and clock1= '1' and clock1'event and ena1 ='1') or
		(addnsub_reg = "CLOCK2" and clock2= '1' and clock2'event and ena2 ='1') or
		(addnsub_reg = "CLOCK3" and clock3= '1' and clock3'event and ena3 ='1') then
		addsub_reg <= addnsub;
	end if;
end if;
end process;

process (clock0, clock1, clock2, clock3, aclr0, aclr1, aclr2, aclr3, addsub_reg)
begin
if addnsub_pipeline_reg = "UNREGISTERED" then
	addsub_pipe <= addsub_reg;
	if (addnsub_pipeline_aclr= "ACLR0" and aclr0 = '1') or  
		(addnsub_pipeline_aclr= "ACLR1" and aclr1 = '1') or  
		(addnsub_pipeline_aclr= "ACLR2" and aclr2 = '1') or  
		(addnsub_pipeline_aclr= "ACLR3" and aclr3 = '1') then
		addsub_pipe <= '0';
	elsif (addnsub_pipeline_reg = "CLOCK0" and clock0= '1' and clock0'event and ena0 ='1') or
		(addnsub_pipeline_reg = "CLOCK1" and clock1= '1' and clock1'event and ena1 ='1') or
		(addnsub_pipeline_reg = "CLOCK2" and clock2= '1' and clock2'event and ena2 ='1') or
		(addnsub_pipeline_reg = "CLOCK3" and clock3= '1' and clock3'event and ena3 ='1') then
		addsub_pipe <= addsub_reg;

	end if;
end if;
end process;

process (clock0, clock1, clock2, clock3, aclr0, aclr1, aclr2, aclr3, accum_sload)
begin
if accum_sload_reg = "UNREGISTERED" then
	acc_sload_reg <= accum_sload;
	if (accum_sload_aclr= "ACLR0" and aclr0 = '1') or  
		(accum_sload_aclr= "ACLR1" and aclr1 = '1') or  
		(accum_sload_aclr= "ACLR2" and aclr2 = '1') or  
		(accum_sload_aclr= "ACLR3" and aclr3 = '1') then
		acc_sload_reg <= '0';
	elsif (accum_sload_reg = "CLOCK0" and clock0= '1' and clock0'event and ena0 ='1') or
		(accum_sload_reg = "CLOCK1" and clock1= '1' and clock1'event and ena1 ='1') or
		(accum_sload_reg = "CLOCK2" and clock2= '1' and clock2'event and ena2 ='1') or
		(accum_sload_reg = "CLOCK3" and clock3= '1' and clock3'event and ena3 ='1') then
		acc_sload_reg <= accum_sload;

end if;
end if;
end process;


process (clock0, clock1, clock2, clock3, aclr0, aclr1, aclr2, aclr3, acc_sload_reg)
begin

if accum_sload_pipeline_reg = "UNREGISTERED" then
	accum_sload_pipe <= acc_sload_reg;
	if (accum_sload_pipeline_aclr= "ACLR0" and aclr0 = '1') or  
		(accum_sload_pipeline_aclr= "ACLR1" and aclr1 = '1') or  
		(accum_sload_pipeline_aclr= "ACLR2" and aclr2 = '1') or  
		(accum_sload_pipeline_aclr= "ACLR3" and aclr3 = '1') then
		accum_sload_pipe <= '0';

	elsif (accum_sload_pipeline_reg = "CLOCK0" and clock0= '1' and clock0'event and ena0 ='1') or
		(accum_sload_pipeline_reg = "CLOCK1" and clock1= '1' and clock1'event and ena1 ='1') or
		(accum_sload_pipeline_reg = "CLOCK2" and clock2= '1' and clock2'event and ena2 ='1') or
		(accum_sload_pipeline_reg = "CLOCK3" and clock3= '1' and clock3'event and ena3 ='1') then
		accum_sload_pipe <= acc_sload_reg;

	end if;
end if;

end process;

process (clock0, clock1, clock2, clock3, aclr0, aclr1, aclr2, aclr3, signa)
begin
if sign_reg_a= "UNREGISTERED" then
	sign_a_reg <= signa;
if (sign_aclr_a= "ACLR0" and aclr0 = '1') or  
	(sign_aclr_a= "ACLR1" and aclr1 = '1') or  
	(sign_aclr_a= "ACLR2" and aclr2 = '1') or  
	(sign_aclr_a= "ACLR3" and aclr3 = '1') then
	sign_a_reg <= '0';

elsif (sign_reg_a = "CLOCK0" and clock0= '1' and clock0'event and ena0 ='1') or
		(sign_reg_a = "CLOCK1" and clock1= '1' and clock1'event and ena1 ='1') or
		(sign_reg_a = "CLOCK2" and clock2= '1' and clock2'event and ena2 ='1') or
		(sign_reg_a = "CLOCK3" and clock3= '1' and clock3'event and ena3 ='1') then
	sign_a_reg <= signa;

end if;
end if;
end process;

process (clock0, clock1, clock2, clock3, aclr0, aclr1, aclr2, aclr3, signb)
begin
if sign_reg_b= "UNREGISTERED" then
	sign_b_reg <= signb;

if (sign_aclr_b= "ACLR0" and aclr0 = '1') or  
	(sign_aclr_b= "ACLR1" and aclr1 = '1') or  
	(sign_aclr_b= "ACLR2" and aclr2 = '1') or  
	(sign_aclr_b= "ACLR3" and aclr3 = '1') then
	sign_b_reg <= '0';

elsif (sign_reg_b = "CLOCK0" and clock0= '1' and clock0'event and ena0 ='1') or
		(sign_reg_b = "CLOCK1" and clock1= '1' and clock1'event and ena1 ='1') or
		(sign_reg_b = "CLOCK2" and clock2= '1' and clock2'event and ena2 ='1') or
		(sign_reg_b = "CLOCK3" and clock3= '1' and clock3'event and ena3 ='1') then
	sign_b_reg <= signb;

end if;
end if;
end process;

process (clock0, clock1, clock2, clock3, aclr0, aclr1, aclr2, aclr3, sign_a_reg)
begin
if sign_pipeline_aclr_a = "UNREGISTERED" then
	sign_a_pipe <= sign_a_reg;
if (sign_pipeline_aclr_a= "ACLR0" and aclr0 = '1') or  
	(sign_pipeline_aclr_a= "ACLR1" and aclr1 = '1') or  
	(sign_pipeline_aclr_a= "ACLR2" and aclr2 = '1') or  
	(sign_pipeline_aclr_a= "ACLR3" and aclr3 = '1') then
	sign_a_pipe <= '0';

elsif (sign_pipeline_reg_a = "CLOCK0" and clock0= '1' and clock0'event and ena0 ='1') or
		(sign_pipeline_reg_a = "CLOCK1" and clock1= '1' and clock1'event and ena1 ='1') or
		(sign_pipeline_reg_a = "CLOCK2" and clock2= '1' and clock2'event and ena2 ='1') or
		(sign_pipeline_reg_a = "CLOCK3" and clock3= '1' and clock3'event and ena3 ='1') then
	sign_a_pipe <= sign_a_reg;

end if;
end if;
end process;

process (clock0, clock1, clock2, clock3, aclr0, aclr1, aclr2, aclr3, sign_b_reg)
begin

if sign_pipeline_aclr_b = "UNREGISTERED" then
	sign_b_pipe <= sign_b_reg;
if (sign_pipeline_aclr_b= "ACLR0" and aclr0 = '1') or  
	(sign_pipeline_aclr_b= "ACLR1" and aclr1 = '1') or  
	(sign_pipeline_aclr_b= "ACLR2" and aclr2 = '1') or  
	(sign_pipeline_aclr_b= "ACLR3" and aclr3 = '1') then
	sign_b_pipe <= '0';

elsif (sign_pipeline_reg_b = "CLOCK0" and clock0= '1' and clock0'event and ena0 ='1') or
		(sign_pipeline_reg_b = "CLOCK1" and clock1= '1' and clock1'event and ena1 ='1') or
		(sign_pipeline_reg_b = "CLOCK2" and clock2= '1' and clock2'event and ena2 ='1') or
		(sign_pipeline_reg_b = "CLOCK3" and clock3= '1' and clock3'event and ena3 ='1') then
	sign_b_pipe <= sign_b_reg;

end if;
end if;
end process;

process (clock0, clock1, clock2, clock3, aclr0, aclr1, aclr2, aclr3, mult_a, mult_b)
variable mult_a_int : std_logic_vector (width_a -1 downto 0);
variable mult_b_int : std_logic_vector (width_b -1 downto 0);
variable neg_a, neg_b : std_logic;
variable temp_mult_int : std_logic_vector (width_result-1 downto 0);
begin
	if ((multiplier_aclr= "ACLR0" and aclr0 = '1') or  
		(multiplier_aclr= "ACLR1" and aclr1 = '1') or  
		(multiplier_aclr= "ACLR2" and aclr2 = '1') or  
		(multiplier_aclr= "ACLR3" and aclr3 = '1')) and
		not (multiplier_reg = "UNREGISTERED") then

		temp_mult <= (others => '0');

	elsif (multiplier_reg = "CLOCK0" and clock0= '1' and clock0'event and ena0 ='1') or
			(multiplier_reg = "CLOCK1" and clock1= '1' and clock1'event and ena1 ='1') or
			(multiplier_reg = "CLOCK2" and clock2= '1' and clock2'event and ena2 ='1') or
			(multiplier_reg = "CLOCK3" and clock3= '1' and clock3'event and ena3 ='1') or 
			multiplier_reg = "UNREGISTERED" then

		neg_a := mult_a (width_a-1) and (sign_a_reg);
		neg_b := mult_b (width_b-1) and (sign_b_reg);
	
		if neg_a ='1' then
			mult_a_int := unsigned (not mult_a) + 1;
		else
			mult_a_int := mult_a;
		end if;

		if neg_b ='1' then
			mult_b_int := unsigned (not mult_b) + 1;
		else
			mult_b_int := mult_b;
		end if;
	
		temp_mult_int := unsigned (temp_mult_zero) + unsigned(mult_a_int) * unsigned(mult_b_int);
		temp_mult_signed <= sign_a_reg or sign_b_reg;

		if (neg_a xor neg_b) = '1' then
		  temp_mult_int := unsigned(temp_mult_zero) - unsigned(temp_mult_int);
		end if;
		temp_mult <= temp_mult_int;

	end if;
end process;

process (clock0, clock1, clock2, clock3, aclr0, aclr1, aclr2, aclr3)
variable head_result_int : integer;
variable result_full, temp_sum : std_logic_vector (width_result downto 0) := (others => '0');
variable cout_int, overflow_int :std_logic;
variable temp_sum_zero : std_logic_vector (width_result downto 0) := (others => '0');

begin

if (output_aclr= "ACLR0" and aclr0 = '1') or  
	(output_aclr= "ACLR1" and aclr1 = '1') or  
	(output_aclr= "ACLR2" and aclr2 = '1') or  
	(output_aclr= "ACLR3" and aclr3 = '1') then
		
		temp_sum := (others => '0');

elsif (output_reg = "CLOCK0" and clock0= '1' and clock0'event and ena0 ='1') or
		(output_reg = "CLOCK1" and clock1= '1' and clock1'event and ena0 ='1') or
		(output_reg = "CLOCK2" and clock2= '1' and clock2'event and ena0 ='1') or
		(output_reg = "CLOCK3" and clock3= '1' and clock3'event and ena0 ='1') then
	

		if accum_sload_pipe ='1' then
			cout_int := '0';
			temp_sum := mult_res;
		else
			if addsub_pipe = '1' then
				temp_sum := unsigned (temp_sum_zero) + unsigned (result_int) + unsigned (mult_res);
				cout_int := temp_sum (width_result);
			else
				temp_sum := unsigned (temp_sum_zero) - unsigned (result_int) - unsigned (mult_res);	
			
				if result_int >= mult_res then
					cout_int :='1';
				else
					cout_int :='0';
				end if;
			end if;
		end if;

		if accum_sload_pipe ='0' then
			if (mult_signed = '1' and (not (mult_res = temp_mult_zero))) then
				overflow_int := ((not (mult_res (width_result-1) xor result_int (width_result -1))) xor (not (addsub_pipe))) and (mult_res (width_result -1) xor temp_sum (width_result -1));
			else
				overflow_int := not (addsub_pipe xor cout_int);
			end if;
		else
			overflow_int :='0';
		end if;


		if extra_accumulator_latency = 0 then
			result <= temp_sum (width_result-1 downto 0);
			overflow <= overflow_int;
		else
			head_result_int :=head_result;
			result_pipe (head_result_int) <= (overflow_int & temp_sum (width_result-1 downto 0));
				
			head_result_int := (head_result_int +1) mod (extra_accumulator_latency);

			result_full := result_pipe(head_result_int);
				
			result <= result_full (width_result-1 downto 0);	
			overflow <= result_full (width_result);					
		end if;

		result_int <= temp_sum (width_result-1 downto 0);	
			
end if;

end process;


process (clock0, clock1, clock2, clock3, aclr0, aclr1, aclr2, aclr3, temp_mult, temp_mult_signed)
variable head_mult_int: integer;
variable mult_full: std_logic_vector (width_result downto 0);
begin
	if (multiplier_reg = "CLOCK0" and clock0= '1' and clock0'event and ena0 ='1') or
			(multiplier_reg = "CLOCK1" and clock1= '1' and clock1'event and ena1 ='1') or
			(multiplier_reg = "CLOCK2" and clock2= '1' and clock2'event and ena2 ='1') or
			(multiplier_reg = "CLOCK3" and clock3= '1' and clock3'event and ena3 ='1') or 
			(multiplier_reg = "UNREGISTERED" and clock0 ='1' and clock0'event and ena0='1') then

		if extra_multiplier_latency >0 then 
			head_mult_int := head_mult;
			mult_pipe (head_mult_int) <= temp_mult;
			
			head_mult_int := (head_mult_int +1) mod (extra_multiplier_latency+1);

			mult_full := (temp_mult_signed & mult_pipe(head_mult_int));
				
			mult_res <= mult_full (width_result-1 downto 0);
			mult_signed <= mult_full (width_result);						
			head_mult <= head_mult_int;
		else 
			mult_res <= temp_mult;
			mult_signed <= temp_mult_signed;
		end if;
	end if;
end process;

end behaviour;


--------------------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_arith.all;

entity 	altmult_add is
		generic (width_a: integer := 3;
						width_b: integer :=3;
						width_result: integer := 16;
						number_of_multipliers: integer := 0;
						
						-- A inputs

						input_register_a0 : string := "UNREGISTERED";
						input_aclr_a0 : string := "NONE";
						input_source_a0: string := "DATAA";

						input_register_a1 : string := "UNREGISTERED";
						input_aclr_a1 : string := "NONE";
						input_source_a1: string := "DATAA";

						input_register_a2 : string := "UNREGISTERED";
						input_aclr_a2 : string := "NONE";
						input_source_a2: string := "DATAA";

						input_register_a3 : string := "UNREGISTERED";
						input_aclr_a3 : string := "NONE";
						input_source_a3: string := "DATAA";

						representation_a: string := "UNUSED";
						signed_register_a: string := "UNREGISTERED";
						signed_aclr_a : string := "NONE";
						signed_pipeline_register_a: string := "UNREGISTERED";
						signed_pipeline_aclr_a : string := "NONE";

						-- B inputs

						input_register_b0 : string := "UNREGISTERED";
						input_aclr_b0 : string := "NONE";
						input_source_b0: string := "DATAB";

						input_register_b1 : string := "UNREGISTERED";
						input_aclr_b1 : string := "NONE";
						input_source_b1: string := "DATAB";

						input_register_b2 : string := "UNREGISTERED";
						input_aclr_b2 : string := "NONE";
						input_source_b2: string := "DATAB";

						input_register_b3 : string := "UNREGISTERED";
						input_aclr_b3 : string := "NONE";
						input_source_b3: string := "DATAB";

						representation_b: string := "UNUSED";
						signed_register_b: string := "UNREGISTERED";
						signed_aclr_b : string := "NONE";
						signed_pipeline_register_b: string := "UNREGISTERED";
						signed_pipeline_aclr_b : string := "NONE";

						multiplier_register0: string := "UNREGISTERED";
						multiplier_aclr0 : string := "NONE";
						multiplier_register1: string := "UNREGISTERED";
						multiplier_aclr1 : string := "NONE";
						multiplier_register2: string := "UNREGISTERED";
						multiplier_aclr2 : string := "NONE";
						multiplier_register3: string := "UNREGISTERED";
						multiplier_aclr3 : string := "NONE";

						addnsub_multiplier_register1: string := "UNREGISTERED";
						addnsub_multiplier_aclr1 : string := "NONE";
						addnsub_multiplier_pipeline_register1: string := "UNREGISTERED";
						addnsub_multiplier_pipeline_aclr1 : string := "NONE";
				
						addnsub_multiplier_register3: string := "UNREGISTERED";
						addnsub_multiplier_aclr3 : string := "NONE";
						addnsub_multiplier_pipeline_register3: string := "UNREGISTERED";
						addnsub_multiplier_pipeline_aclr3 : string := "NONE";
			
						multiplier1_direction : string := "ADD";
						multiplier3_direction : string := "ADD";			

						output_register: string := "CLOCK0";
						output_aclr : string := "ACLR0";

						extra_latency: integer :=0;
						use_dedicated_circuitry:string  := "OFF";
						lpm_hint : string := "bogus"
						);
						
    port (dataa : in std_logic_vector(number_of_multipliers * width_a -1 downto 0);
				datab : in std_logic_vector(number_of_multipliers * width_b -1 downto 0);
				
				-- clock ports
				clock3, clock2, clock1, clock0    : in std_logic := '1';
				aclr3, aclr2, aclr1, aclr0 : in std_logic := '0';
				ena3, ena2, ena1, ena0 :	in std_logic := '1';

				-- control signals
				signa, signb : in std_logic := '0';
				addnsub1, addnsub3	: in std_logic := 'Z';

				-- output ports
				result  : out std_logic_vector(width_result -1 downto 0);
				scanouta : out std_logic_vector (width_a -1 downto 0);
				scanoutb : out std_logic_vector (width_b -1 downto 0)
				);

end altmult_add;

architecture behaviour of altmult_add is
signal zeropad : std_logic_vector ((width_result - width_a - width_b)/2  -1 downto 0) := (others => '0');



signal answer : std_logic_vector (width_result + 1 downto 0):= (others => '0');
signal mult_a : std_logic_vector (4 * width_a -1 downto 0):= (others => '0');
signal mult_b : std_logic_vector (4 * width_b -1 downto 0):= (others => '0');
signal mult_res :std_logic_vector (number_of_multipliers * width_result -1 downto 0) := (others => '0');
signal zero_acc_reg, zero_acc_pipe: std_logic := '0';
signal sign_a_reg, sign_a_pipe: std_logic := '0';
signal sign_b_reg, sign_b_pipe: std_logic := '0';
signal addsub_reg1, addsub_pipe1: std_logic := '1';
signal addsub_reg3, addsub_pipe3: std_logic  := '1';

signal out_sum : std_logic_vector (width_result + 1 downto 0);
type pipeline_accum is array (extra_latency downto 0) of std_logic_vector (width_result-1 downto 0); 

signal result_pipe: pipeline_accum := (others => (others => '0'));

signal mult_clock : std_logic_vector (3 downto 0):= (others => '0');
signal mult_ena : std_logic_vector (3 downto 0):= (others => '0');
signal mult_aclr : std_logic_vector (3 downto 0):= (others => '0');

signal clock_vector : std_logic_vector (3 downto 0):= (others => '0');
signal ena_vector : std_logic_vector (3 downto 0):= (others => '0');
signal aclr_vector : std_logic_vector (3 downto 0):= (others => '0');
--signal mux_clock : std_logic;
--signal scana: std_logic_vector (number_of_multipliers * width_a -1 downto 0);
--signal scanb: std_logic_vector (number_of_multipliers * width_b -1 downto 0);

signal dataa_int :std_logic_vector (4 * width_a -1 downto 0):= (others => '0');
signal datab_int :std_logic_vector (4 * width_b -1 downto 0):= (others => '0');
signal head_result : integer := 0;
signal signed_mult : std_logic := '1';
signal is_reg: std_logic_vector (3 downto 0):= (others => '0'); 

signal temp_mult_zero : std_logic_vector (width_result -1 downto 0) := (others => '0');
function resolve_clock (ARG : string) return integer is
variable clock_num:integer := 0;

begin

if ARG = "CLOCK0" then
	clock_num := 0;
elsif ARG = "CLOCK1" then
	clock_num := 1;
elsif ARG = "CLOCK2" then
	clock_num := 2;
elsif ARG = "CLOCK3" then
	clock_num := 3;
end if;

return clock_num;

end resolve_clock;

function resolve_aclr (ARG : string) return integer is
variable aclr_num:integer := 0;

begin

if ARG = "ACLR0" then
	aclr_num := 0;
elsif ARG = "ACLR1" then
	aclr_num := 1;
elsif ARG = "ACLR2" then
	aclr_num := 2;
elsif ARG = "ACLR3" then
	aclr_num := 3;
end if;

return aclr_num;

end resolve_aclr;

function check_clock (arg: integer) return string is
variable  ret_val:string (1 to 6);

begin
if arg = 0 then
	ret_val := multiplier_register0 (1 to 6);
elsif arg =1 then
	ret_val := multiplier_register1 (1 to 6);
elsif arg=2 then
	ret_val := multiplier_register2 (1 to 6);
elsif arg=3 then
	ret_val := multiplier_register3 (1 to 6);
else
	ret_val := "CLOCK0";
end if;

return ret_val;

end check_clock;



begin

scanouta <= mult_a ((number_of_multipliers * width_a) -1 downto (number_of_multipliers -1 ) * width_a) ;
scanoutb <= mult_b ((number_of_multipliers * width_b) -1 downto (number_of_multipliers -1 ) * width_b) ;


clock_vector (0) <= clock0;
clock_vector (1) <= clock1;
clock_vector (2) <= clock2;
clock_vector (3) <= clock3;

ena_vector (0) <= ena0;
ena_vector (1) <= ena1;
ena_vector (2) <= ena2;
ena_vector (3) <= ena3;

aclr_vector (0) <= aclr0;
aclr_vector (1) <= aclr1;
aclr_vector (2) <= aclr2;
aclr_vector (3) <= aclr3;

process (dataa)
variable i :integer;
begin
	for i in 0 to ((number_of_multipliers * width_a) -1) loop
		dataa_int (i) <= dataa (i);
	end loop; 

end process;

process (datab)
variable i :integer;
begin
	for i in 0 to ((number_of_multipliers * width_b) -1) loop
		datab_int (i) <= datab (i);
	end loop; 
end process;



-- clock and cke setup for all registers
process (clock0, clock1, clock2, clock3, aclr0, aclr1, aclr2, aclr3, dataa_int, datab_int)

variable temp_clock: integer := 0;
variable temp_aclr: integer := 0;

variable x : integer;
--variable temp_sum : std_logic_vector (width_result + 1 downto 0);



begin

--setup clocks

if not (multiplier_register0 = "UNREGISTERED") then
	temp_clock := resolve_clock (multiplier_register0);
	mult_clock(0) <= clock_vector (temp_clock);
	mult_ena(0) <= ena_vector(temp_clock);
	temp_aclr := resolve_aclr (multiplier_aclr0);
	mult_aclr (0) <= aclr_vector (temp_aclr);
	is_reg (0) <= '1';
end if;

if not (multiplier_register1 = "UNREGISTERED") then
	temp_clock := resolve_clock (multiplier_register1);
	mult_clock(1) <= clock_vector (temp_clock);
	mult_ena(1) <= ena_vector(temp_clock);
	temp_aclr := resolve_aclr (multiplier_aclr1);
	mult_aclr (1) <= aclr_vector (temp_aclr);
	is_reg (1) <= '1';

end if;

if not (multiplier_register2 = "UNREGISTERED") then
	temp_clock := resolve_clock (multiplier_register2);
	mult_clock(2) <= clock_vector (temp_clock);
	mult_ena(2) <= ena_vector(temp_clock);
	temp_aclr := resolve_aclr (multiplier_aclr2);
	mult_aclr (2) <= aclr_vector (temp_aclr);
	is_reg (2) <= '1';

end if;

if not (multiplier_register3 = "UNREGISTERED") then
	temp_clock := resolve_clock (multiplier_register3);
	mult_clock(3) <= clock_vector (temp_clock);
	mult_ena(3) <= ena_vector(temp_clock);
	temp_aclr := resolve_aclr (multiplier_aclr3);
	mult_aclr (3) <= aclr_vector (temp_aclr);
	is_reg (3) <= '1';
end if;




-- clock inputs for multipliers 3 to N
for i in 3 to (number_of_multipliers -1) loop 

if ((input_aclr_a3= "ACLR0" and aclr0 = '1') or  
		(input_aclr_a3= "ACLR1" and aclr1 = '1') or  
		(input_aclr_a3= "ACLR2" and aclr2 = '1') or  
		(input_aclr_a3= "ACLR3" and aclr3 = '1')) and
		not (input_register_a3 = "UNREGISTERED") then
			mult_a ( (i+1)*width_a-1 downto (i*width_a)) <= (others => '0');
elsif (input_register_a3 = "CLOCK0" and clock0= '1' and clock0'event and ena0 ='1') or
			(input_register_a3 = "CLOCK1" and clock1= '1' and clock1'event and ena1 ='1') or
			(input_register_a3= "CLOCK2" and clock2= '1' and clock2'event and ena2 ='1') or
			(input_register_a3 = "CLOCK3" and clock3= '1' and clock3'event and ena3 ='1') or
			input_register_a3 = "UNREGISTERED" then
			if (input_source_a3) = "DATAA" then
				mult_a ( (i+1)*width_a-1 downto (i*width_a)) <= dataa_int ((i+1)*width_a-1 downto (i*width_a));
			else
				mult_a ( (i+1)*width_a-1 downto (i*width_a)) <= mult_a ( (i)*width_a-1 downto ((i-1)*width_a));
			end if;
end if;


if ((input_aclr_b3= "ACLR0" and aclr0 = '1') or  
		(input_aclr_b3= "ACLR1" and aclr1 = '1') or  
		(input_aclr_b3= "ACLR2" and aclr2 = '1') or  
		(input_aclr_b3= "ACLR3" and aclr3 = '1')) and 
		not (input_register_b3 = "UNREGISTERED") then

		mult_b ( (i+1)*width_b-1 downto (i*width_b)) <= (others => '0');

elsif (input_register_b3= "CLOCK0" and clock0= '1' and clock0'event and ena0 ='1') or
		(input_register_b3 = "CLOCK1" and clock1= '1' and clock1'event and ena1 ='1') or
		(input_register_b3 = "CLOCK2" and clock2= '1' and clock2'event and ena2 ='1') or
		(input_register_b3 = "CLOCK3" and clock3= '1' and clock3'event and ena3 ='1') or 
		input_register_b3 = "UNREGISTERED" then
		if (input_source_b3) = "DATAB" then
			mult_b ( (i+1)*width_b-1 downto (i*width_b)) <= datab_int ((i+1)*width_b-1 downto (i*width_b));
		else
			mult_b ( (i+1)*width_b-1 downto (i*width_b)) <= mult_b ( (i)*width_b-1 downto ((i-1)*width_b));
		end if;
end if;

end loop;


	if ((input_aclr_a0= "ACLR0" and aclr0 = '1') or  
		(input_aclr_a0= "ACLR1" and aclr1 = '1') or  
		(input_aclr_a0= "ACLR2" and aclr2 = '1') or  
		(input_aclr_a0= "ACLR3" and aclr3 = '1')) and 
		not (input_register_a0 = "UNREGISTERED") then
				mult_a (width_a-1 downto 0) <= (others => '0');
	elsif (input_register_a0 = "CLOCK0" and clock0= '1' and clock0'event and ena0 ='1') or
			(input_register_a0 = "CLOCK1" and clock1= '1' and clock1'event and ena1 ='1') or
			(input_register_a0= "CLOCK2" and clock2= '1' and clock2'event and ena2 ='1') or
			(input_register_a0 = "CLOCK3" and clock3= '1' and clock3'event and ena3 ='1') or 
			input_register_a0 = "UNREGISTERED" then
			if (input_source_a0 = "DATAA") then
				mult_a ( width_a-1 downto 0) <= dataa_int (width_a-1 downto 0);
			else
				-- wrap around the scan-chain?
				mult_a ( width_a-1 downto 0) <= mult_a ((number_of_multipliers * width_a) -1 downto (number_of_multipliers -1 ) * width_a) ;
			end if;
	end if;



	if ((input_aclr_b0= "ACLR0" and aclr0 = '1') or  
		(input_aclr_b0= "ACLR1" and aclr1 = '1') or  
		(input_aclr_b0= "ACLR2" and aclr2 = '1') or  
		(input_aclr_b0= "ACLR3" and aclr3 = '1')) and
		not (input_register_b0 = "UNREGISTERED") then
			mult_b (width_b-1 downto 0) <= (others => '0');
	elsif (input_register_b0= "CLOCK0" and clock0= '1' and clock0'event and ena0 ='1') or
		(input_register_b0 = "CLOCK1" and clock1= '1' and clock1'event and ena1 ='1') or
		(input_register_b0 = "CLOCK2" and clock2= '1' and clock2'event and ena2 ='1') or
		(input_register_b0 = "CLOCK3" and clock3= '1' and clock3'event and ena3 ='1') or 
		input_register_b0 = "UNREGISTERED" then
		if (input_source_b0) = "DATAB" then
			mult_b (width_b-1 downto 0) <= datab_int (width_b-1 downto 0);
		else
			-- wrap around the scan-chain?
			mult_b (width_b-1 downto 0) <= mult_b ((number_of_multipliers * width_b) -1 downto (number_of_multipliers -1 ) * width_b) ;
		end if;
	end if;


	if ((input_aclr_a1= "ACLR0" and aclr0 = '1') or  
		(input_aclr_a1= "ACLR1" and aclr1 = '1') or  
		(input_aclr_a1= "ACLR2" and aclr2 = '1') or  
		(input_aclr_a1= "ACLR3" and aclr3 = '1')) and
		not (input_register_a1 = "UNREGISTERED") then
			mult_a (  (2)*width_a-1 downto (width_a)) <= (others => '0');
	elsif (input_register_a1 = "CLOCK0" and clock0= '1' and clock0'event and ena0 ='1') or
			(input_register_a1 = "CLOCK1" and clock1= '1' and clock1'event and ena1 ='1') or
			(input_register_a1= "CLOCK2" and clock2= '1' and clock2'event and ena2 ='1') or
			(input_register_a1 = "CLOCK3" and clock3= '1' and clock3'event and ena3 ='1') then
			if (input_source_a1) = "DATAA" then
				mult_a (  (2)*width_a-1 downto (width_a)) <= dataa_int ( (2)*width_a-1 downto (width_a));
			else
				mult_a (  (2)*width_a-1 downto (width_a)) <= mult_a ( width_a-1 downto 0);
		end if;		
	end if;


	if ((input_aclr_b1= "ACLR0" and aclr0 = '1') or  
		(input_aclr_b1= "ACLR1" and aclr1 = '1') or  
		(input_aclr_b1= "ACLR2" and aclr2 = '1') or  
		(input_aclr_b1= "ACLR3" and aclr3 = '1')) and
		not (input_register_b1 = "UNREGISTERED") then
			mult_b (  (2)*width_b-1 downto (width_b)) <= (others => '0');
	elsif (input_register_b1= "CLOCK0" and clock0= '1' and clock0'event and ena0 ='1') or
		(input_register_b1 = "CLOCK1" and clock1= '1' and clock1'event and ena1 ='1') or
		(input_register_b1 = "CLOCK2" and clock2= '1' and clock2'event and ena2 ='1') or
		(input_register_b1 = "CLOCK3" and clock3= '1' and clock3'event and ena3 ='1') or
		input_register_b1 = "UNREGISTERED" then
		if (input_source_b1) = "DATAB" then
				mult_b (  (2)*width_b-1 downto (width_b)) <= datab_int ( (2)*width_b-1 downto (width_b));
		else
				mult_b (  (2)*width_b-1 downto (width_b)) <= mult_b ( width_b-1 downto 0);
		end if;	
	end if;


	if ((input_aclr_a2= "ACLR0" and aclr0 = '1') or  
		(input_aclr_a2= "ACLR1" and aclr1 = '1') or  
		(input_aclr_a2= "ACLR2" and aclr2 = '1') or  
		(input_aclr_a2= "ACLR3" and aclr3 = '1')) and
		not (input_register_a2 = "UNREGISTERED") then
			mult_a ( (3)*width_a-1 downto (2*width_a)) <= (others => '0');
	elsif (input_register_a2 = "CLOCK0" and clock0= '1' and clock0'event and ena0 ='1') or
			(input_register_a2 = "CLOCK1" and clock1= '1' and clock1'event and ena1 ='1') or
			(input_register_a2= "CLOCK2" and clock2= '1' and clock2'event and ena2 ='1') or
			(input_register_a2 = "CLOCK3" and clock3= '1' and clock3'event and ena3 ='1') or 
			input_register_a2 = "UNREGISTERED" then
		if (input_source_a2) = "DATAA" then
				mult_a ( (3)*width_a-1 downto (2*width_a)) <= dataa_int ((3)*width_a-1 downto (2*width_a));
		else
				mult_a ( (3)*width_a-1 downto (2*width_a)) <= mult_a (  (2)*width_a-1 downto (width_a));
		end if;	
			
	end if;



	if ((input_aclr_b2= "ACLR0" and aclr0 = '1') or  
		(input_aclr_b2= "ACLR1" and aclr1 = '1') or  
		(input_aclr_b2= "ACLR2" and aclr2 = '1') or  
		(input_aclr_b2= "ACLR3" and aclr3 = '1')) and 
		not (input_register_b2 = "UNREGISTERED") then
			mult_b ( (3)*width_b-1 downto (2*width_b)) <= (others => '0');
	elsif (input_register_b2= "CLOCK0" and clock0= '1' and clock0'event and ena0 ='1') or
		(input_register_b2 = "CLOCK1" and clock1= '1' and clock1'event and ena1 ='1') or
		(input_register_b2 = "CLOCK2" and clock2= '1' and clock2'event and ena2 ='1') or
		(input_register_b2 = "CLOCK3" and clock3= '1' and clock3'event and ena3 ='1') or
		input_register_b2 = "UNREGISTERED" then
		if (input_source_b2) = "DATAB" then
			mult_b ( (3)*width_b-1 downto (2*width_b)) <= datab_int ((3)*width_b-1 downto (2*width_b));
		else
			mult_b ( (3)*width_b-1 downto (2*width_b)) <= mult_b (  (2)*width_b-1 downto (width_b));
		end if;	
	end if;


if addnsub_multiplier_register1 = "UNREGISTERED" then
		addsub_reg1 <= addnsub1;
else
	if (addnsub_multiplier_aclr1= "ACLR0" and aclr0 = '1') or  
		(addnsub_multiplier_aclr1= "ACLR1" and aclr1 = '1') or  
		(addnsub_multiplier_aclr1= "ACLR2" and aclr2 = '1') or  
		(addnsub_multiplier_aclr1= "ACLR3" and aclr3 = '1') then
		addsub_reg1 <= '0';
	elsif (addnsub_multiplier_register1 = "CLOCK0" and clock0= '1' and clock0'event and ena0 ='1') or
		(addnsub_multiplier_register1 = "CLOCK1" and clock1= '1' and clock1'event and ena1 ='1') or
		(addnsub_multiplier_register1 = "CLOCK2" and clock2= '1' and clock2'event and ena2 ='1') or
		(addnsub_multiplier_register1 = "CLOCK3" and clock3= '1' and clock3'event and ena3 ='1') then
		addsub_reg1 <= addnsub1;
	end if;
end if;

if addnsub_multiplier_pipeline_register1 = "UNREGISTERED" then
	addsub_pipe1 <= addsub_reg1;
else
	if (addnsub_multiplier_pipeline_aclr1= "ACLR0" and aclr0 = '1') or  
		(addnsub_multiplier_pipeline_aclr1= "ACLR1" and aclr1 = '1') or  
		(addnsub_multiplier_pipeline_aclr1= "ACLR2" and aclr2 = '1') or  
		(addnsub_multiplier_pipeline_aclr1= "ACLR3" and aclr3 = '1') then
		addsub_pipe1<= '0';
	elsif (addnsub_multiplier_pipeline_register1 = "CLOCK0" and clock0= '1' and clock0'event and ena0 ='1') or
		(addnsub_multiplier_pipeline_register1 = "CLOCK1" and clock1= '1' and clock1'event and ena1 ='1') or
		(addnsub_multiplier_pipeline_register1 = "CLOCK2" and clock2= '1' and clock2'event and ena2 ='1') or
		(addnsub_multiplier_pipeline_register1 = "CLOCK3" and clock3= '1' and clock3'event and ena3 ='1') then
		addsub_pipe1 <= addsub_reg1;

	end if;
end if;

if addnsub_multiplier_register3 = "UNREGISTERED" then
		addsub_reg3 <= addnsub3;
else
	if (addnsub_multiplier_aclr3= "ACLR0" and aclr0 = '1') or  
		(addnsub_multiplier_aclr3= "ACLR1" and aclr1 = '1') or  
		(addnsub_multiplier_aclr3= "ACLR2" and aclr2 = '1') or  
		(addnsub_multiplier_aclr3= "ACLR3" and aclr3 = '1') then
		addsub_reg3 <= '0';
	elsif (addnsub_multiplier_register3 = "CLOCK0" and clock0= '1' and clock0'event and ena0 ='1') or
		(addnsub_multiplier_register3 = "CLOCK1" and clock1= '1' and clock1'event and ena1 ='1') or
		(addnsub_multiplier_register3 = "CLOCK2" and clock2= '1' and clock2'event and ena2 ='1') or
		(addnsub_multiplier_register3 = "CLOCK3" and clock3= '1' and clock3'event and ena3 ='1') then
		addsub_reg3 <= addnsub3;
	end if;
end if;

if addnsub_multiplier_pipeline_register3 = "UNREGISTERED" then
	addsub_pipe3 <= addsub_reg3;
else
	if (addnsub_multiplier_pipeline_aclr3= "ACLR0" and aclr0 = '1') or  
		(addnsub_multiplier_pipeline_aclr3= "ACLR1" and aclr1 = '1') or  
		(addnsub_multiplier_pipeline_aclr3= "ACLR2" and aclr2 = '1') or  
		(addnsub_multiplier_pipeline_aclr3= "ACLR3" and aclr3 = '1') then
		addsub_pipe3<= '0';
	elsif (addnsub_multiplier_pipeline_register3 = "CLOCK0" and clock0= '1' and clock0'event and ena0 ='1') or
		(addnsub_multiplier_pipeline_register3 = "CLOCK1" and clock1= '1' and clock1'event and ena1 ='1') or
		(addnsub_multiplier_pipeline_register3 = "CLOCK2" and clock2= '1' and clock2'event and ena2 ='1') or
		(addnsub_multiplier_pipeline_register3 = "CLOCK3" and clock3= '1' and clock3'event and ena3 ='1') then
		addsub_pipe3 <= addsub_reg3;

	end if;
end if;





if signed_register_a= "UNREGISTERED" then
	sign_a_reg <= signa;
else
if (signed_aclr_a= "ACLR0" and aclr0 = '1') or  
	(signed_aclr_a= "ACLR1" and aclr1 = '1') or  
	(signed_aclr_a= "ACLR2" and aclr2 = '1') or  
	(signed_aclr_a= "ACLR3" and aclr3 = '1') then
	sign_a_reg <= '0';

elsif (signed_register_a = "CLOCK0" and clock0= '1' and clock0'event and ena0 ='1') or
		(signed_register_a = "CLOCK1" and clock1= '1' and clock1'event and ena1 ='1') or
		(signed_register_a = "CLOCK2" and clock2= '1' and clock2'event and ena2 ='1') or
		(signed_register_a = "CLOCK3" and clock3= '1' and clock3'event and ena3 ='1') then
	sign_a_reg <= signa;

end if;
end if;

if signed_register_b= "UNREGISTERED" then
	sign_b_reg <= signb;
else
if (signed_aclr_b= "ACLR0" and aclr0 = '1') or  
	(signed_aclr_b= "ACLR1" and aclr1 = '1') or  
	(signed_aclr_b= "ACLR2" and aclr2 = '1') or  
	(signed_aclr_b= "ACLR3" and aclr3 = '1') then
	sign_b_reg <= '0';

elsif (signed_register_b = "CLOCK0" and clock0= '1' and clock0'event and ena0 ='1') or
		(signed_register_b = "CLOCK1" and clock1= '1' and clock1'event and ena1 ='1') or
		(signed_register_b = "CLOCK2" and clock2= '1' and clock2'event and ena2 ='1') or
		(signed_register_b = "CLOCK3" and clock3= '1' and clock3'event and ena3 ='1') then
	sign_b_reg <= signb;

end if;
end if;

if signed_pipeline_register_a = "UNREGISTERED" then
	sign_a_pipe <= sign_a_reg;
else
if (signed_pipeline_aclr_a= "ACLR0" and aclr0 = '1') or  
	(signed_pipeline_aclr_a= "ACLR1" and aclr1 = '1') or  
	(signed_pipeline_aclr_a= "ACLR2" and aclr2 = '1') or  
	(signed_pipeline_aclr_a= "ACLR3" and aclr3 = '1') then
	sign_a_pipe <= '0';

elsif (signed_pipeline_register_a = "CLOCK0" and clock0= '1' and clock0'event and ena0 ='1') or
		(signed_pipeline_register_a = "CLOCK1" and clock1= '1' and clock1'event and ena1 ='1') or
		(signed_pipeline_register_a = "CLOCK2" and clock2= '1' and clock2'event and ena2 ='1') or
		(signed_pipeline_register_a = "CLOCK3" and clock3= '1' and clock3'event and ena3 ='1') then
	sign_a_pipe <= sign_a_reg;

end if;
end if;


if signed_pipeline_register_b = "UNREGISTERED" then
	sign_b_pipe <= sign_b_reg;
else
if (signed_pipeline_aclr_b= "ACLR0" and aclr0 = '1') or  
	(signed_pipeline_aclr_b= "ACLR1" and aclr1 = '1') or  
	(signed_pipeline_aclr_b= "ACLR2" and aclr2 = '1') or  
	(signed_pipeline_aclr_b= "ACLR3" and aclr3 = '1') then
	sign_b_pipe <= '0';

elsif (signed_pipeline_register_b = "CLOCK0" and clock0= '1' and clock0'event and ena0 ='1') or
		(signed_pipeline_register_b = "CLOCK1" and clock1= '1' and clock1'event and ena1 ='1') or
		(signed_pipeline_register_b = "CLOCK2" and clock2= '1' and clock2'event and ena2 ='1') or
		(signed_pipeline_register_b = "CLOCK3" and clock3= '1' and clock3'event and ena3 ='1') then
	sign_b_pipe <= sign_b_reg;

end if;
end if;

end process;


process (clock0, clock1, clock2, clock3, mult_aclr)
variable mult_a_int : std_logic_vector (width_a -1 downto 0);
variable mult_b_int : std_logic_vector (width_b -1 downto 0);
variable neg_a, neg_b : std_logic;
variable temp_mult_int : std_logic_vector (width_result-1 downto 0);
variable mux_clock : std_logic;
variable check_clock_out : string (1 to 6);

variable x: integer;
begin

for i in 0 to (number_of_multipliers -1) loop 
	
	x := i;
	if (i > 3) then
		x := 3;
	end if;
			mux_clock := mult_clock (x);

	check_clock_out := check_clock (x);
	if (is_reg (x) = '1' and mult_aclr (x) = '1') then
			mult_res ( (i+1)*width_result-1 downto (i*width_result)) <= (others => '0');

--	elsif (mux_clock = '1' and check_event (x) and mult_ena (x) = '1') or is_reg(i) = '0' then

	elsif  (check_clock_out = "CLOCK0" and clock0'event and clock0='1' and ena0 = '1') or
				(check_clock_out = "CLOCK1" and clock1'event and clock1='1' and ena1 = '1') or
				(check_clock_out = "CLOCK2" and clock2'event and clock2='1' and ena2 = '1') or
				(check_clock_out = "CLOCK3" and clock3'event and clock3='1' and ena3 = '1')	or
				is_reg (x) = '0' then

		if (representation_a = "SIGNED" or sign_a_reg = '1') then
			neg_a := mult_a ( (i+1)*width_a-1);
		end if;
		if (representation_b = "SIGNED" or sign_b_reg = '1') then
			neg_b := mult_b ( (i+1)*width_b-1);
		end if;
		
		if neg_a ='1' then
			mult_a_int := unsigned (not mult_a ( (i+1)*width_a-1 downto (i*width_a))) + 1;
		else
			mult_a_int := mult_a ( (i+1)*width_a-1 downto (i*width_a));
		end if;

		if neg_b ='1' then
			mult_b_int := unsigned (not mult_b ( (i+1)*width_b-1 downto (i*width_b))) + 1;
		else
			mult_b_int := mult_b ( (i+1)*width_b-1 downto (i*width_b));
		end if;
	
		temp_mult_int := unsigned (temp_mult_zero) + unsigned(mult_a_int) * unsigned(mult_b_int);
		--temp_mult_signed <= sign_a_reg or sign_b_reg;

		if (neg_a xor neg_b) = '1' then
		  temp_mult_int := unsigned(temp_mult_zero) - unsigned(temp_mult_int);
		end if;

		mult_res ( (i+1)*width_result-1 downto (i*width_result)) <= temp_mult_int;

	end if;

end loop;
end process;

process (clock0, clock1, clock2, clock3, aclr0, aclr1, aclr2, aclr3)
variable head_result_int : integer;
variable temp_sum : std_logic_vector (width_result downto 0) := (others => '0');
variable do_add : boolean;
begin

if (output_aclr= "ACLR0" and aclr0 = '1') or  
	(output_aclr= "ACLR1" and aclr1 = '1') or  
	(output_aclr= "ACLR2" and aclr2 = '1') or  
	(output_aclr= "ACLR3" and aclr3 = '1') then
		
		temp_sum := (others => '0');

elsif (output_register = "CLOCK0" and clock0= '1' and clock0'event and ena0 ='1') or
		(output_register = "CLOCK1" and clock1= '1' and clock1'event and ena1 ='1') or
		(output_register = "CLOCK2" and clock2= '1' and clock2'event and ena2 ='1') or
		(output_register = "CLOCK3" and clock3= '1' and clock3'event and ena3 ='1') then
		
		temp_sum := (others => '0');
		for i in 0 to (number_of_multipliers -1) loop

				if ( (i = 1 and addsub_pipe1 = '1') or (i =3 and addsub_pipe3 ='1') or
								(i = 1 and addnsub1 = 'Z' and multiplier1_direction = "ADD") or
								(i = 3 and addnsub3 = 'Z' and multiplier3_direction = "ADD") or
								i =0 or i=2 or i > 3) then
							do_add := true;
				else
							do_add := false;
				end if;


				if (do_add= true)  then
					if (sign_a_pipe='1' or sign_b_pipe='1') then 
						temp_sum := signed (temp_sum) + signed((mult_res( (i+1)*width_result-1 downto (i*width_result))));
					else
						temp_sum := unsigned (temp_sum) + unsigned((mult_res( (i+1)*width_result-1 downto (i*width_result))));
					end if;
				else
					if (sign_a_pipe='1' or sign_b_pipe='1') then 
						temp_sum := signed (temp_sum) - signed((mult_res( (i+1)*width_result-1 downto (i*width_result))));
					else
						temp_sum := unsigned (temp_sum) - unsigned((mult_res( (i+1)*width_result-1 downto (i*width_result))));
					end if;
				end if;			
		end loop;



		if extra_latency = 0 then
			result <= temp_sum (width_result-1 downto 0);
		else
			head_result_int := head_result;
			result_pipe (head_result_int) <= temp_sum (width_result-1 downto 0);
				
			head_result_int := (head_result_int +1) mod (extra_latency);

			result  <= result_pipe(head_result_int);

			head_result <= head_result_int;
		end if;

end if;

end process;

end behaviour;


library IEEE;
use IEEE.std_logic_1164.all;

entity altshift_taps is
generic (number_of_taps:integer := 4;
				tap_distance:integer :=3;
				width:integer := 8;
				width_taps:integer :=1;
				lpm_hint : string := "bogus");

port (shiftin: in std_logic_vector (width-1 downto 0);
			clock: in std_logic;
			clken: in std_logic := '1';
			shiftout: out std_logic_vector (width-1 downto 0):=(others => '0');
			taps: out std_logic_vector ((width*number_of_taps)-1 downto 0):= (others => '0'));

end altshift_taps;

architecture behavioural of altshift_taps is
type mxn_array is array ((number_of_taps * tap_distance)-1 downto 0) of std_logic_vector (width-1 downto 0);
signal contents : mxn_array:= (others => (others => '0'));
signal head_pipe: integer :=0;

signal i: integer:=0;

begin

shift: process (clock)
variable head: integer :=0;
begin
	if clock'event and clock = '1' and clken = '1' then
		head := head_pipe;

		contents (head) <= shiftin;
		shiftout <= contents ((head +1) mod (number_of_taps * tap_distance));
		head := (head +1) mod (number_of_taps * tap_distance);
		
		for i in 0 to number_of_taps -1
		loop
			taps ( ( (i+1) * width) -1 downto (i* width) ) <= contents ((((number_of_taps - i-1)*tap_distance) + head) mod (number_of_taps * tap_distance)); 
		end loop; 

		head_pipe <= head;
	end if;

end process shift;


end behavioural;


----------------------------------------------------------------------------

-- altsyncram megafunction

----------------------------------------------------------------------------

--`define NO_PLI

library ieee;
use ieee.std_logic_1164.all;
--use ieee.numeric_std.all;
use ieee.std_logic_arith.all;
use std.textio.all;

ENTITY altsyncram IS
   GENERIC (
      operation_mode                 :  string := "SINGLE_PORT";    -- "BIDIR_DUAL_PORT";
      -- port a parameters
      width_a                        :  integer := 8;    -- 1;
      widthad_a                      :  integer := 2;    -- 1;
      numwords_a                     :  integer := 4;    -- 1;
      -- registering parameters
      -- port a read parameters
      outdata_reg_a                  :  string := "UNREGISTERED";    
      -- clearing parameters
      address_aclr_a                 :  string := "NONE";    
      outdata_aclr_a                 :  string := "NONE";    
      -- clearing parameters
      -- port a write parameters
      indata_aclr_a                  :  string := "CLEAR0";    
      wrcontrol_aclr_a               :  string := "CLEAR0";    
      -- clear for the byte enable port reigsters which are clocked by clk0
      byteena_aclr_a                 :  string := "NONE";    
      -- width of the byte enable ports. if it is used, must be WIDTH_WRITE_A/8 or /9
      width_byteena_a                :  integer := 1;    
      -- port b parameters
      width_b                        :  integer := 8;    -- 1;
      widthad_b                      :  integer := 4;    -- 1;
      numwords_b                     :  integer := 4;    -- 1;
      -- registering parameters
      -- port b read parameters
      rdcontrol_reg_b                :  string := "CLOCK1";    
      address_reg_b                  :  string := "CLOCK1";    
      outdata_reg_b                  :  string := "UNREGISTERED";    
      -- clearing parameters
      outdata_aclr_b                 :  string := "NONE";    
      rdcontrol_aclr_b               :  string := "NONE";    
      -- registering parameters
      -- port b write parameters
      indata_reg_b                   :  string := "CLOCK1";    
      wrcontrol_wraddress_reg_b      :  string := "CLOCK1";    
      -- registering parameter for the byte enable reister for port b
      byteena_reg_b                  :  string := "CLOCK1";    
      -- clearing parameters
      indata_aclr_b                  :  string := "NONE";    
      wrcontrol_aclr_b               :  string := "NONE";    
      address_aclr_b                 :  string := "NONE";    
      -- clear parameter for byte enable port register
      byteena_aclr_b                 :  string := "NONE";    
      -- width of the byte enable ports. if it is used, must be WIDTH_WRITE_A/8 or /9
      width_byteena_b                :  integer := 1;    
      -- width of a byte for byte enables
      -- global parameters
      byte_size                      :  integer := 8;    
      read_during_write_mode_port_a  :  string := "NEW_DATA_ON_FALLING_EDGE";    
      read_during_write_mode_port_b  :  string := "NEW_DATA_ON_FALLING_EDGE";    
      -- mixed-port feed-through mode choices are "OLD_DATA" or "DONT_CARE"
      read_during_write_mode_mixed_ports: string := "DONT_CARE";    
      -- ram block type choices are "AUTO", "M512", "M4K" and "MEGARAM"
      ram_block_type                 :  string := "AUTO";    
      -- general operation parameters
      init_file                      :  string := "UNUSED";    
      init_file_layout               :  string := "UNKNOWN";    
      maximum_depth                  :  integer := 0;    
      intended_device_family         : string  := "Stratix";
	
	-- new parameter for port_feed_through but I have not eliminated the old parameter
	mixed_port_feed_through_mode : string := "DONT_CARE";
      -- bogus lpm_hint parameter?
      lpm_hint                       :  string := "bogus");
   PORT (
      wren_a                  : IN std_logic := '0';   
      wren_b                  : IN std_logic := '0';   
      rden_b                  : IN std_logic := '1';   
      data_a                  : IN std_logic_vector(width_a - 1 DOWNTO 0):= (others => '0');   
      data_b                  : IN std_logic_vector(width_b - 1 DOWNTO 0):= (others => '0');   
      address_a               : IN std_logic_vector(widthad_a - 1 DOWNTO 0) := (others => '0');   
      address_b               : IN std_logic_vector(widthad_b - 1 DOWNTO 0) := (others => '0');   
      -- two clocks only

      clock0                  : IN std_logic := '1';   
      clock1                  : IN std_logic := '1';   
      clocken0                : IN std_logic := '1';   
      clocken1                : IN std_logic := '1';   
      aclr0                   : IN std_logic := '0';   
      aclr1                   : IN std_logic := '0';     
			byteena_a               : IN std_logic_vector( (width_byteena_a)  - 1 downTO 0) := (others => 'Z'); 
		  byteena_b               : IN std_logic_vector( (width_byteena_b)  - 1 downTO 0) := (others => 'Z');   
		      
      q_a                     : OUT std_logic_vector(width_a - 1 DOWNTO 0);   
      q_b                     : OUT std_logic_vector(width_b - 1 DOWNTO 0));   


	function int_to_str( value : integer ) return string is
	variable ivalue,index : integer;
	variable digit : integer;
	variable line_no: string(8 downto 1) := "        ";  
	begin
		ivalue := value;
		index := 1;
		while (ivalue > 0) loop
			digit := ivalue MOD 10;
			ivalue := ivalue/10;
			case digit is
				when 0 =>
					line_no(index) := '0';
				when 1 =>
					line_no(index) := '1';
				when 2 =>
					line_no(index) := '2';
				when 3 =>
					line_no(index) := '3';
				when 4 =>
					line_no(index) := '4';
				when 5 =>
					line_no(index) := '5';
				when 6 =>
					line_no(index) := '6';
				when 7 =>
					line_no(index) := '7';
				when 8 =>
					line_no(index) := '8';
				when 9 =>
					line_no(index) := '9';
				when others =>
					ASSERT FALSE
					REPORT "Illegal number!"
					SEVERITY ERROR;
			end case;
			index := index + 1;
		end loop;
		return line_no;
	end;

	function hex_str_to_int( str : string ) return integer is
	variable len : integer := str'length;
	variable ivalue : integer := 0;
	variable digit : integer;
	begin
		for i in len downto 1 loop
			case str(i) is
				when '0' =>
					digit := 0;
				when '1' =>
					digit := 1;
				when '2' =>
					digit := 2;
				when '3' =>
					digit := 3;
				when '4' =>
					digit := 4;
				when '5' =>
					digit := 5;
				when '6' =>
					digit := 6;
				when '7' =>
					digit := 7;
				when '8' =>
					digit := 8;
				when '9' =>
					digit := 9;
				when 'A' =>
					digit := 10;
				when 'a' =>
					digit := 10;
				when 'B' =>
					digit := 11;
				when 'b' =>
					digit := 11;
				when 'C' =>
					digit := 12;
				when 'c' =>
					digit := 12;
				when 'D' =>
					digit := 13;
				when 'd' =>
					digit := 13;
				when 'E' =>
					digit := 14;
				when 'e' =>
					digit := 14;
				when 'F' =>
					digit := 15;
				when 'f' =>
					digit := 15;
				when others =>
					ASSERT FALSE
					REPORT "Illegal character "&  str(i) & "in Intel Hex File! "
					SEVERITY ERROR;
			end case;
			ivalue := ivalue * 16 + digit;
		end loop;
		return ivalue;
	end;

	procedure Shrink_line(L : inout LINE; pos : in integer) is
	subtype nstring is string(1 to pos);
	variable stmp : nstring;
	begin
		if pos >= 1 then
			read(l, stmp);
		end if;
    end;

TYPE xhdl_3 IS ARRAY (2 ** widthad_a - 1 downto 0) OF std_logic_vector(width_a - 1 DOWNTO 0);
TYPE xhdl_4 IS ARRAY (2 ** widthad_b - 1 downto 0) OF std_logic_vector(width_b - 1 DOWNTO 0);


procedure read_my_memory (constant use_a : in boolean;
											variable mem_data_a : out xhdl_3;
											variable mem_data_b : out xhdl_4 ) is 
--variable mem_data : xhdl_3;
--    variable mem_data_w : xhdl_4;
    variable mem_data_word_a : std_logic_vector(WIDTH_A-1 downto 0);
    variable mem_data_word_b : std_logic_vector(WIDTH_B-1 downto 0);
    variable i, j, k, n, m, lineno : integer := 0;
    variable buf : line ;
    variable booval : boolean ;
    --FILE unused_file : TEXT IS OUT "UNUSED";
    --FILE mem_data_file : TEXT IS IN INIT_FILE;
    FILE unused_file : TEXT OPEN WRITE_MODE IS "UNUSED";
    FILE mem_data_file : TEXT OPEN READ_MODE IS INIT_FILE;
	variable base, byte, rec_type, datain, addr, checksum: string(2 downto 1);
	variable startadd: string(4 downto 1);
	variable ibase: integer := 0;
	variable ibyte: integer := 0;
	variable istartadd: integer := 0;
	variable check_sum_vec, check_sum_vec_tmp: std_logic_vector(7 downto 0);
    variable init_numwords_write_a : integer := numwords_a;
    variable init_numwords_write_b : integer := numwords_b;
    variable init_numwords_read_a : integer := numwords_a;
    variable init_numwords_read_b : integer := numwords_b;
    variable init_om : integer := 0;
begin
-- INITIALIZE MEMORY --

            
                if (use_a) then     
                    -- INITIALIZE MEM_DATA_A TO ALL 0 --
                    for i in mem_data_a'LOW to mem_data_a'HIGH loop
                        mem_data_a(i) := (OTHERS => '0');
                    end loop;
                else                                -- if QP or DP mode
                    -- INITIALIZE MEM_DATA_b TO ALL 0 --
                    for i in mem_data_b'LOW to mem_data_b'HIGH loop
                        mem_data_b(i) := (OTHERS => '0');
                    end loop;
                end if;
            
				WHILE NOT ENDFILE(mem_data_file) loop
					booval := true;
					READLINE(mem_data_file, buf);
					lineno := lineno + 1;
					check_sum_vec := (OTHERS => '0');
					if (buf(buf'LOW) = ':') then
						i := 1;
						shrink_line(buf, i);
						READ(L=>buf, VALUE=>byte, good=>booval);
						if not (booval) then
							ASSERT FALSE
							REPORT "[Line "& int_to_str(lineno) & "]:Illegal Intel Hex Format!"
							SEVERITY ERROR;
						end if;
						ibyte := hex_str_to_int(byte);
						check_sum_vec := unsigned(check_sum_vec) + unsigned(CONV_STD_LOGIC_VECTOR(ibyte, 8));
						READ(L=>buf, VALUE=>startadd, good=>booval);
						if not (booval) then
							ASSERT FALSE
							REPORT "[Line "& int_to_str(lineno) & "]:Illegal Intel Hex Format! "
							SEVERITY ERROR;
						end if;
						istartadd := hex_str_to_int(startadd);
						addr(2) := startadd(4);
						addr(1) := startadd(3);
						check_sum_vec := unsigned(check_sum_vec) + unsigned(CONV_STD_LOGIC_VECTOR(hex_str_to_int(addr), 8));
						addr(2) := startadd(2);
						addr(1) := startadd(1);
						check_sum_vec := unsigned(check_sum_vec) + unsigned(CONV_STD_LOGIC_VECTOR(hex_str_to_int(addr), 8));
						READ(L=>buf, VALUE=>rec_type, good=>booval);
						if not (booval) then
							ASSERT FALSE
							REPORT "[Line "& int_to_str(lineno) & "]:Illegal Intel Hex Format! "
							SEVERITY ERROR;
						end if;
						check_sum_vec := unsigned(check_sum_vec) + unsigned(CONV_STD_LOGIC_VECTOR(hex_str_to_int(rec_type), 8));
					else
						ASSERT FALSE
						REPORT "[Line "& int_to_str(lineno) & "]:Illegal Intel Hex Format! "
						SEVERITY ERROR;
					end if;
					case rec_type is
						when "00"=>     -- Data record
							i := 0;
                 if (use_a) then -- if SP or BDP mode
                    k := (WIDTH_A + 7) / 8;  -- # of bytes per entry
                 else                        -- if ROM, QP or DP mode
                    k := (WIDTH_B + 7) / 8;  -- # of bytes per entry
                 end if;
							while (i < ibyte) loop
                    n := (k - 1)*8;
                    if (use_a) then
                       mem_data_word_a := (others => '0');
                       m := WIDTH_A - 1;
                    else
                       mem_data_word_b := (others => '0');
                       m := WIDTH_b - 1;        
								  end if;
								
									for j in 1 to k loop
									READ(L=>buf, VALUE=>datain,good=>booval); -- read in data a byte (2 hex chars) at a time.
									if not (booval) then
										ASSERT FALSE
										REPORT "[Line "& int_to_str(lineno) & "]:Illegal Intel Hex Format! "
										SEVERITY ERROR;
									end if;
									check_sum_vec := unsigned(check_sum_vec) + unsigned(CONV_STD_LOGIC_VECTOR(hex_str_to_int(datain), 8));
                     if (use_a) then 
                          mem_data_word_a(m downto n) := CONV_STD_LOGIC_VECTOR(hex_str_to_int(datain), m-n+1);
                     else                     
                          mem_data_word_b(m downto n) := CONV_STD_LOGIC_VECTOR(hex_str_to_int(datain), m-n+1);
                     end if;
                     m := n - 1;
                     n := n - 8;
								end loop;
								i := i + k;
                   if (use_a) then -- if SP or BDP mode
                       mem_data_a(ibase + istartadd) := mem_data_word_a;
                   else                        -- if ROM, QP or DP mode
                       mem_data_b(ibase + istartadd) := mem_data_word_b;
                   end if;
								istartadd := istartadd + 1;
							end loop;
						when "01"=>
							exit;
						when "02"=>
							ibase := 0;
							if (ibyte /= 2) then
								ASSERT FALSE
								REPORT "[Line "& int_to_str(lineno) & "]:Illegal Intel Hex Format for record type 02! "
								SEVERITY ERROR;
							end if;
							for i in 0 to (ibyte-1) loop
								READ(L=>buf, VALUE=>base,good=>booval);
								ibase := ibase * 256 + hex_str_to_int(base);
								if not (booval) then
									ASSERT FALSE
									REPORT "[Line "& int_to_str(lineno) & "]:Illegal Intel Hex Format! "
									SEVERITY ERROR;
								end if;
								check_sum_vec := unsigned(check_sum_vec) + unsigned(CONV_STD_LOGIC_VECTOR(hex_str_to_int(base), 8));
							end loop;
							ibase := ibase * 16;
						when OTHERS =>
							ASSERT FALSE
							REPORT "[Line "& int_to_str(lineno) & "]:Illegal record type in Intel Hex File! "
							SEVERITY ERROR;
					end case;
                    READ(L=>buf, VALUE=>checksum, good=>booval);
					if not (booval) then
						ASSERT FALSE
						REPORT "[Line "& int_to_str(lineno) & "]:Checksum is missing! "
						SEVERITY ERROR;
					end if;

            check_sum_vec := unsigned(not (check_sum_vec)) + 1;
					check_sum_vec_tmp := CONV_STD_LOGIC_VECTOR(hex_str_to_int(checksum),8);

					if (unsigned(check_sum_vec) /= unsigned(check_sum_vec_tmp)) then
						ASSERT FALSE
						REPORT "[Line "& int_to_str(lineno) & "]:Incorrect checksum!"
						SEVERITY ERROR;
					end if;
				end loop;
		
            --mem_init <= TRUE;
end read_my_memory;

function initial_rden (my_param : string ) return STD_LOGIC is
variable ret_val : STD_LOGIC;
	begin
	if my_param = "BIDIR_DUAL_PORT" then
			ret_val := '1';
	else
			ret_val := '0';
	end if;
	return ret_val;
end initial_rden; 

  
END altsyncram;

ARCHITECTURE translated OF altsyncram IS

  -- TYPE xhdl_3 IS ARRAY (2 ** widthad_a - 1 downto 0) OF std_logic_vector(width_a - 1 DOWNTO 0);
   --TYPE xhdl_4 IS ARRAY (2 ** widthad_b - 1 downto 0) OF std_logic_vector(width_b - 1 DOWNTO 0);

   -- internal reg 
   SIGNAL mem_data                 :  xhdl_3;   
   SIGNAL mem_data_b               :  xhdl_4;   
   SIGNAL i_data_reg_a             :  std_logic_vector(width_a - 1 DOWNTO 0) := (others => '0');   
   --SIGNAL temp_wa                  :  std_logic_vector(width_a - 1 DOWNTO 0) := (others => '0');   
   --SIGNAL temp_wa2                 :  std_logic_vector(width_a - 1 DOWNTO 0) := (others => '0');   
   SIGNAL i_data_reg_b             :  std_logic_vector(width_b - 1 DOWNTO 0) := (others => '0');   
   --SIGNAL temp_wb                  :  std_logic_vector(width_b - 1 DOWNTO 0) := (others => '0');   
   --SIGNAL temp_wb2                 :  std_logic_vector(width_b - 1 DOWNTO 0) := (others => '0');   
   SIGNAL temp                     :  std_logic := '0' ;   
   SIGNAL i_q_reg_a                :  std_logic_vector(width_a - 1 DOWNTO 0) := (others => '0');   
   SIGNAL i_q_tmp_a                :  std_logic_vector(width_a - 1 DOWNTO 0) := (others => '0');   
   SIGNAL i_q_tmp2_a               :  std_logic_vector(width_a - 1 DOWNTO 0) := (others => '0'); 
	SIGNAL i_q_tmp_wren_a  										:  std_logic_vector(width_a - 1 DOWNTO 0) := (others => '0');
   SIGNAL i_q_reg_b                :  std_logic_vector(width_b - 1 DOWNTO 0) := (others => '0');   
   SIGNAL i_q_tmp_b                :  std_logic_vector(width_b - 1 DOWNTO 0) := (others => '0');   
   SIGNAL i_q_tmp2_b               :  std_logic_vector(width_b - 1 DOWNTO 0) := (others => '0');   
   SIGNAL i_byteena_mask_reg_a     :  std_logic_vector(width_a - 1 DOWNTO 0) := (others => '0');   
   SIGNAL i_byteena_mask_reg_b     :  std_logic_vector(width_b - 1 DOWNTO 0) := (others => '0');   
   SIGNAL i_address_reg_a          :  std_logic_vector(widthad_a - 1 DOWNTO 0) := (others => '0');   
   SIGNAL i_address_reg_b          :  std_logic_vector(widthad_b - 1 DOWNTO 0) := (others => '0');   
   SIGNAL ram_initf                :  std_logic_vector(8 * 256 DOWNTO 1) := (others => '0');   
   SIGNAL i_wren_reg_a             :  std_logic:= '0';   
   SIGNAL i_wren_reg_b             :  std_logic:= '0';   
   SIGNAL i_rden_reg_b             :  std_logic:= '1';   
	  
        --  signal  i_rden_reg_b :std_logic := '1' when (operation_mode = "BIDIR_DUAL_PORT") else '0' ; 
     
	SIGNAL i_indata_aclr_a          :  std_logic:= '0';   
   SIGNAL i_address_aclr_a         :  std_logic:= '0';   
   SIGNAL i_wrcontrol_aclr_a       :  std_logic:= '0';   
   SIGNAL i_indata_aclr_b          :  std_logic:= '0';   
   SIGNAL i_address_aclr_b         :  std_logic:= '0';   
   SIGNAL i_wrcontrol_aclr_b       :  std_logic:= '0';   
   SIGNAL i_outdata_aclr_a         :  std_logic:= '0';   
   SIGNAL i_outdata_aclr_b         :  std_logic:= '0';   
   SIGNAL i_rdcontrol_aclr_b       :  std_logic:= '0';   
   SIGNAL i_byteena_aclr_a         :  std_logic:= '0';   
   SIGNAL i_byteena_aclr_b         :  std_logic:= '0';   
   SIGNAL i_q_a                    :  std_logic_vector(width_a - 1 DOWNTO 0) := (others => '0');   
   SIGNAL i_q_b                    :  std_logic_vector(width_b - 1 DOWNTO 0) := (others => '0');   
   SIGNAL write_by_a               :  integer := 0;   
   SIGNAL write_by_b               :  integer := 0;   
   SIGNAL write_by_a_reg_b         :  integer := 0;   
   SIGNAL write_by_b_reg_a         :  integer := 0;   
   SIGNAL i_numwords_a             :  integer := 0;   
   SIGNAL i_numwords_b             :  integer := 0;   
   SIGNAL i                        :  integer := 0;   
   SIGNAL j                        :  integer := 0;   
   SIGNAL k                        :  integer := 0;   
   SIGNAL good_to_go_a             :  std_logic:= '0';   
   SIGNAL good_to_go_b             :  std_logic:= '0';   
   SIGNAL i_clk_a                  :  std_logic:= '0';   
   SIGNAL i_outdata_clk_a          :  std_logic:= '0';   
   SIGNAL i_clken_a                :  std_logic:= '0';   
   SIGNAL i_outdata_clken_a        :  std_logic:= '0';   
   SIGNAL i_indata_clk_b           :  std_logic:= '0';   
   SIGNAL i_outdata_clk_b          :  std_logic:= '0';   
   SIGNAL i_indata_clken_b         :  std_logic:= '0';   
   SIGNAL i_outdata_clken_b        :  std_logic:= '0';   
   SIGNAL i_wrcontrol_wraddress_clk_b     :  std_logic:= '0';   
   SIGNAL i_rdcontrol_clk_b        :  std_logic:= '0';   
   SIGNAL i_address_clk_b          :  std_logic:= '0';   
   SIGNAL i_byteena_clk_b          :  std_logic:= '0';   
   SIGNAL i_wrcontrol_wraddress_clken_b   :  std_logic:= '0';   
   SIGNAL i_rdcontrol_clken_b      :  std_logic:= '0';   
   SIGNAL i_address_clken_b        :  std_logic:= '0';   
   SIGNAL i_byteena_clken_b        :  std_logic:= '0';   
   SIGNAL byteena_a_unconnected    :  std_logic:= '0';   
   SIGNAL byteena_b_unconnected    :  std_logic:= '0';   
   SIGNAL file_desc                :  std_logic_vector(31 DOWNTO 0)  := (others => '0');   
   -- does width_a ever have to equal width_b?
   --
   -- ROM       o           1 |                  0
   -- SP            o   o   2 |                  0
   -- DP    o   o   o   o   3 |                  0
   -- BDP           o   o   2 |          o   o   2
   --      EN ADDR EN ADDR %6 | EN ADDR EN ADDR %5
   --      RD  RD  WR  WR     | RD  RD  WR  WR
   --       << PORT A >>      |  << PORT B >>
   -- This is the table for encoding of om.
   --
   -- Check for operation mode
   -- ROM  |  x       |          |
   -- SP   |  x   x   |          |
   -- DP   |      x   |  x       |
   -- BDP  |  x   x   |  x   x   |
   --      |  RD  WR  |  RD  WR  |
   --      |  PORT A  |  PORT B  |
   -- legal operations for all operation modes:
   SIGNAL temp_xhdl5               :  integer;   
   SIGNAL temp_xhdl7               :  integer;   
   SIGNAL temp_xhdl9               :  integer;   
   SIGNAL temp_xhdl11              :  integer;   
   SIGNAL temp_xhdl13              :  integer;   
   SIGNAL temp_xhdl24              :  std_logic;   
   SIGNAL temp_xhdl25              :  std_logic;   
   -- port b:
   SIGNAL temp_xhdl26              :  std_logic;   
   SIGNAL temp_xhdl27              :  std_logic;   
   SIGNAL temp_xhdl28              :  std_logic;   
   SIGNAL temp_xhdl29              :  std_logic;   
   SIGNAL temp_xhdl30              :  std_logic;   
   SIGNAL temp_xhdl31              :  std_logic;   
   SIGNAL temp_xhdl32              :  std_logic;   
   SIGNAL temp_xhdl33              :  std_logic;   
   SIGNAL temp_xhdl34              :  std_logic;   
   SIGNAL temp_xhdl35              :  std_logic;   
   SIGNAL temp_xhdl36              :  std_logic;   
   SIGNAL temp_xhdl37              :  std_logic;   
   SIGNAL temp_xhdl38              :  std_logic;   
   SIGNAL temp_xhdl39              :  std_logic;   
   -- port b:
   SIGNAL temp_xhdl40              :  std_logic;   
   SIGNAL temp_xhdl41              :  std_logic;   
   SIGNAL temp_xhdl42              :  std_logic;   
   SIGNAL temp_xhdl43              :  std_logic;   
   SIGNAL temp_xhdl44              :  std_logic;   
   SIGNAL temp_xhdl45              :  std_logic;   
   SIGNAL temp_xhdl46              :  std_logic;   
   SIGNAL temp_xhdl47              :  std_logic;   
   SIGNAL temp_xhdl48              :  std_logic;   
   SIGNAL temp_xhdl49              :  std_logic;   
   SIGNAL temp_xhdl50              :  std_logic;   
   SIGNAL temp_xhdl51              :  std_logic;   
   -- port a:
   -- clears:
   SIGNAL temp_xhdl52              :  std_logic;   
   SIGNAL temp_xhdl53              :  std_logic;   
   SIGNAL temp_xhdl54              :  std_logic;   
   SIGNAL temp_xhdl55              :  std_logic;   
   SIGNAL temp_xhdl56              :  std_logic;   
   SIGNAL temp_xhdl57              :  std_logic;   
   SIGNAL temp_xhdl58              :  std_logic;   
   -- port b:
   SIGNAL temp_xhdl59              :  std_logic;   
   SIGNAL temp_xhdl60              :  std_logic;   
   SIGNAL temp_xhdl61              :  std_logic;   
   SIGNAL temp_xhdl62              :  std_logic;   
   SIGNAL temp_xhdl63              :  std_logic;   
   SIGNAL temp_xhdl64              :  std_logic;   
   SIGNAL temp_xhdl65              :  std_logic;   
   SIGNAL temp_xhdl66              :  std_logic;   
   SIGNAL temp_xhdl67              :  std_logic;   
   SIGNAL temp_xhdl68              :  std_logic;   
   SIGNAL temp_xhdl69              :  std_logic;   
   SIGNAL temp_xhdl70              :  std_logic;   
   SIGNAL temp_xhdl87              :  std_logic_vector(width_a - 1 DOWNTO 0);   
   SIGNAL temp_xhdl88              :  std_logic_vector(width_a - 1 DOWNTO 0);   
   SIGNAL temp_xhdl89              :  std_logic_vector(width_b - 1 DOWNTO 0);   
   SIGNAL temp_xhdl90              :  std_logic_vector(width_b - 1 DOWNTO 0);   
   SIGNAL q_a_xhdl1                :  std_logic_vector(width_a - 1 DOWNTO 0):= (others => '0');   
   SIGNAL q_b_xhdl2                :  std_logic_vector(width_b - 1 DOWNTO 0):= (others => '0');   
   SIGNAL i_aclr0                  :  std_logic:='0';   
   SIGNAL i_aclr1                  :  std_logic:='0';   
   SIGNAL i_rden_b                 :  std_logic :=  initial_rden ( operation_mode);
   SIGNAL i_wren_a                 :  std_logic:='0';   
   SIGNAL i_wren_b                 :  std_logic:='0';   
	signal rden_int_b							: std_logic :='0';

	signal need_init 	: boolean := true;
	signal default_val : std_logic;

	SIGNAL i_data_reg_ad             :  std_logic_vector(width_a - 1 DOWNTO 0) := (others => '0');   
  SIGNAL i_data_reg_bd             :  std_logic_vector(width_b - 1 DOWNTO 0) := (others => '0');   
 	SIGNAL i_byteena_mask_reg_ad     :  std_logic_vector(width_a - 1 DOWNTO 0) := (others => '0');   
   SIGNAL i_byteena_mask_reg_bd     :  std_logic_vector(width_b - 1 DOWNTO 0) := (others => '0');   
   SIGNAL i_address_reg_ad          :  std_logic_vector(widthad_a - 1 DOWNTO 0) := (others => '0');   
   SIGNAL i_address_reg_bd          :  std_logic_vector(widthad_b - 1 DOWNTO 0) := (others => '0');   
   
	signal i_wren_reg_ad, i_wren_reg_bd: std_logic;
	signal write_conflict, write_conflict_reg_a, write_conflict_reg_b :std_logic := '0';
	signal write_conflict_reg_ad, write_conflict_reg_bd :std_logic := '0';

BEGIN
   --q_a <= q_a_xhdl1;
   q_b <= q_b_xhdl2;
   -- what do these do? are they just to turn z into x?
   
	default_val <= 'X' when ( (ram_block_type = "AUTO" and mixed_port_feed_through_mode = "DONT_CARE") or ram_block_type = "MEGARAM") else '0';

   i_wren_a <= wren_a ;   
   i_wren_b <= wren_b ;   

   write_conflict <= i_wren_a and i_wren_b;

   i_rden_b <= rden_b ;   
   i_aclr0 <= aclr0 ;   
   i_aclr1 <= aclr1 ;   
   i_numwords_a  <= numwords_a WHEN (numwords_a) /= 0 ELSE 2 ** widthad_a;
   i_numwords_b  <= numwords_b WHEN (numwords_b) /= 0 ELSE 2 ** widthad_b;
   i <= width_a / width_b WHEN (width_a > width_b) ELSE width_b / width_a;
   j <= width_a / width_b WHEN (width_a > width_b) ELSE width_b / width_a;
   k <= width_a / width_a WHEN (width_a > width_a) ELSE width_a / width_a;

process (i_wrcontrol_wraddress_clk_b, i_wren_reg_b, i_data_reg_b, i_address_reg_b, i_byteena_mask_reg_b, write_conflict_reg_b)
begin
	if ((i_wrcontrol_wraddress_clk_b'event and i_wrcontrol_wraddress_clk_b ='0' and ram_block_type /= "MEGARAM") or 
		 (ram_block_type = "MEGARAM")) then
		i_wren_reg_bd <=  i_wren_reg_b;
		i_data_reg_bd <= i_data_reg_b;
		i_address_reg_bd <= i_address_reg_b;
		i_byteena_mask_reg_bd <= i_byteena_mask_reg_b;
		write_conflict_reg_bd <= write_conflict_reg_b;
	end if;
end process;

process (i_clk_a, i_wren_reg_a, i_data_reg_a, i_address_reg_a, i_byteena_mask_reg_a, write_conflict_reg_a)
begin
	if ((i_clk_a'event and i_clk_a ='0' and ram_block_type /= "MEGARAM") or 
		 (ram_block_type = "MEGARAM")) then
		i_wren_reg_ad <=  i_wren_reg_a;
		i_data_reg_ad <= i_data_reg_a;
		i_address_reg_ad <= i_address_reg_a;
		i_byteena_mask_reg_ad <= i_byteena_mask_reg_a;
		write_conflict_reg_ad <= write_conflict_reg_a;
	end if;
end process;


   PROCESS (i_wren_reg_ad, i_data_reg_ad, i_address_reg_ad, i_byteena_mask_reg_ad, --write_by_b,
					i_wren_reg_bd, i_data_reg_bd, i_address_reg_bd, i_byteena_mask_reg_bd, write_conflict_reg_ad, write_conflict_reg_bd) --, i_wrcontrol_wraddress_clk_b )
			
 --(wren_a, wren_b, rden_b, data_a, data_b,address_a, address_b, clock0, clock1, clocken0, clocken1, aclr0, aclr1, byteena_a, byteena_b)
      VARIABLE mem_data_xhdl15  : xhdl_3;
      VARIABLE temp_wa_xhdl16  : std_logic_vector(width_a - 1 DOWNTO 0);
      VARIABLE temp_xhdl17  : std_logic;
      VARIABLE temp_wb_xhdl18  : std_logic_vector(width_b - 1 DOWNTO 0);
      VARIABLE mem_data_b_xhdl19  : xhdl_4;
      VARIABLE write_by_a_xhdl20  : integer;
      VARIABLE write_by_b_xhdl21  : integer;
      VARIABLE byteena_a_unconnected_xhdl22  : std_logic;
      VARIABLE byteena_b_unconnected_xhdl23  : std_logic;
	 	 VARIABLE xhdl_initial : BOOLEAN := true;
			VARIABLE j  : integer;
      VARIABLE temp_wa2_xhdl74  : std_logic_vector(width_a - 1 DOWNTO 0):= (others => 'U');
      VARIABLE mem_data_xhdl75  : xhdl_3;
      VARIABLE write_by_a_xhdl76  : integer;
		VARIABLE temp_wa_xhdl78  : std_logic_vector(width_a - 1 DOWNTO 0) := (others => 'U');
      VARIABLE mem_data_xhdl79  : xhdl_3;
      VARIABLE write_by_b_xhdl80  : integer;
		VARIABLE mem_data_a_int: xhdl_3;
		VARIABLE mem_data_b_int :xhdl_4;
		variable int_numwords_a : integer;
		variable int_numwords_b : integer;
		variable init_file_b_port : boolean;
   BEGIN
			--xhdl_initial := need_init;
      IF (xhdl_initial) THEN
	
		if numwords_a /= 0 then
			int_numwords_a := numwords_a;
		else
			int_numwords_a := 2**widthad_a;
		end if;
	
	if numwords_b /= 0 then
			int_numwords_b := numwords_b;
		else
			int_numwords_b := 2**widthad_b;
		end if;
   

         --IF (((((i_xhdl10 /= 1 AND i_xhdl10 /= 2) AND i_xhdl10 /= 4) AND i_xhdl10 /= 8) AND i_xhdl10 /= 16) OR ((((j_xhdl12 /= 1 AND j_xhdl12 /= 2) AND j_xhdl12 /= 4) AND j_xhdl12 /= 8) AND j_xhdl12 /= 16) OR ((((k_xhdl14 /= 1 AND k_xhdl14 /= 2) AND k_xhdl14 /= 4) AND k_xhdl14 /= 8) AND k_xhdl14 /= 16)) THEN
           -- WRITE("Error! RAM size for port A and/or port B is invalid.");   
--         END IF;
         -- Initialize mem_data
         
         IF (init_file = "UNUSED" OR init_file = "") THEN
            IF (operation_mode = "ROM") THEN
            ELSE
               FOR i_xhdl10 IN 0 TO (int_numwords_a - 1) LOOP
								if ((ram_block_type = "AUTO" and mixed_port_feed_through_mode = "DONT_CARE") or ram_block_type = "MEGARAM") then
		                  mem_data_xhdl15(i_xhdl10) := (OTHERS => 'X');    
								else
									mem_data_xhdl15(i_xhdl10) := (OTHERS => '0'); 
								end if;
               END LOOP;

						FOR i_xhdl10 IN 0 TO (int_numwords_b - 1) LOOP
                  mem_data_b_xhdl19(i_xhdl10) := (OTHERS => 'X');    
               END LOOP;
               
            END IF;
         ELSE

            init_file_b_port := false;
            IF (init_file_layout = "UNKNOWN") THEN
            IF (operation_mode = "DUAL_PORT") THEN
                  init_file_b_port := true;
               ELSE
                  init_file_b_port := false;
               END IF;
            ELSE
               IF (init_file_layout = "PORT_A") THEN
                  init_file_b_port := false;
               ELSIF (init_file_layout = "PORT_B") THEN
                  init_file_b_port := true;
               END IF;
            END IF;

            IF (init_file_b_port) THEN
               -- init from port b parameters

						read_my_memory (false,  mem_data_xhdl15, mem_data_b_xhdl19);
              -- read_my_memory (false,  mem_data, mem_data_b);
               
               FOR i_xhdl10 IN 0 TO (int_numwords_b * width_b - 1) LOOP
                  -- do all bits in mem_data_b
                  
                  temp_wb_xhdl18 := mem_data_b_xhdl19(i_xhdl10 / width_b);    --  get the word from the b array
                  temp_xhdl17 := temp_wb_xhdl18((i_xhdl10)mod width_b);    --  get the bit we want
                  temp_wa_xhdl16 := mem_data_xhdl15(i_xhdl10 / width_a);    --  get the word from the a array
                  temp_wa_xhdl16(i_xhdl10 mod width_a) := temp_xhdl17;    --  set the bit we want to load
                  mem_data_xhdl15(i_xhdl10 / width_a) := temp_wa_xhdl16;    --  set the word in the a array
               END LOOP;
            ELSE
               -- to facilitate data dump below:
               read_my_memory (true, mem_data_xhdl15, mem_data_b_xhdl19);
--						read_my_memory (true, mem_data, mem_data_b);

            END IF;
         END IF;
         write_by_a_xhdl20 := 0;    
         write_by_b_xhdl21 := 0;    
         -- need to disable connection if unconnected...
         
         -- btw, I think this is bogus. the byteena_aclr should be honoured even
         
         -- if the byteena isn't connected to a signal.
         
         byteena_a_unconnected_xhdl22 := '0';    
         IF (byteena_a(0) = 'Z') THEN
            byteena_a_unconnected_xhdl22 := '1';    
         END IF;
         byteena_b_unconnected_xhdl23 := '0';    
         IF (byteena_b(0) = 'Z') THEN
            byteena_b_unconnected_xhdl23 := '1';    
         END IF;
         -- Initialize registers
         
         IF (operation_mode = "BIDIR_DUAL_PORT") THEN
           -- i_rden_reg_b <= '1';    
         ELSE
           -- i_rden_reg_b <= '0';    
         END IF;
        
         mem_data <= mem_data_xhdl15;
         mem_data_b <= mem_data_b_xhdl19;
		
			--mem_data_a_int := mem_data_xhdl15;
			--mem_data_b_int := mem_data_b_xhdl19;

         byteena_a_unconnected <= byteena_a_unconnected_xhdl22;
         byteena_b_unconnected <= byteena_b_unconnected_xhdl23;
         xhdl_initial := FALSE;
				need_init <= xhdl_initial;
			else		
		
			mem_data_a_int := mem_data;
			mem_data_b_int := mem_data_b;

			if i_wren_reg_ad'event or i_data_reg_ad'event or i_address_reg_ad'event or i_byteena_mask_reg_ad'event or (write_conflict_reg_ad'event and write_conflict_reg_ad = '0') then
			IF (operation_mode = "DUAL_PORT" OR operation_mode = "SINGLE_PORT" OR operation_mode = "BIDIR_DUAL_PORT") THEN
         IF (i_wren_reg_ad = '1') THEN
               j := conv_integer(unsigned(i_address_reg_ad)) * width_a;    
               temp_wa2_xhdl74 := (i_data_reg_ad AND i_byteena_mask_reg_ad) OR (mem_data_a_int(conv_integer(unsigned(i_address_reg_ad))) AND NOT i_byteena_mask_reg_ad);    
               mem_data_a_int (conv_integer(unsigned(i_address_reg_ad))) := temp_wa2_xhdl74;    
               write_by_a_xhdl76 := write_by_a + 1;    
         END IF;
	     END IF;
			end if;
       
     -- mem_data <= mem_data_xhdl75;
   	  write_by_a <= write_by_a_xhdl76;			
		
	if i_wren_reg_bd'event or i_data_reg_bd'event or i_address_reg_bd'event or i_byteena_mask_reg_bd'event or (write_conflict_reg_bd'event and write_conflict_reg_bd = '0') then
		IF (operation_mode = "BIDIR_DUAL_PORT") THEN
         -- wait for the appropriate edge to clock in data (MEGARAM happens right away)
         
         IF (i_wren_reg_bd = '1') THEN
            j := conv_integer(unsigned(i_address_reg_bd)) * width_b;    
            FOR i IN 0 TO (width_b - 1) LOOP
               temp_wa_xhdl78 := mem_data_a_int((j + i) / width_a);    
               temp_wa_xhdl78((j + i) mod width_a) := (i_data_reg_bd(i) AND i_byteena_mask_reg_bd(i)) OR (temp_wa_xhdl78((j + i) mod width_a) AND NOT i_byteena_mask_reg_bd(i));    
               mem_data_a_int((j + i) / width_a) := temp_wa_xhdl78;    
            END LOOP;
            write_by_b_xhdl80 := write_by_b + 1;    
         END IF;
				
      END IF;
 end if;	 
--      mem_data <= mem_data_xhdl79;
      write_by_b <= write_by_b_xhdl80;

		mem_data <= mem_data_a_int;
		mem_data_b <= mem_data_b_int;


      End if;
   END PROCESS;


   -- let's try assign statements. (assigns are good...)
   
 -- clocks:
   
   -- port a:
   i_clk_a <= clock0 ;
   temp_xhdl24 <= clock0 WHEN (outdata_reg_a = "CLOCK0") ELSE '0';
   temp_xhdl25 <= clock1 WHEN (outdata_reg_a = "CLOCK1") ELSE (temp_xhdl24);
   i_outdata_clk_a <= temp_xhdl25 ;
   temp_xhdl26 <= clock0 WHEN (indata_reg_b = "CLOCK0") ELSE '0';
   temp_xhdl27 <= clock1 WHEN (indata_reg_b = "CLOCK1") ELSE (temp_xhdl26);
   i_indata_clk_b <= temp_xhdl27 ;
   temp_xhdl28 <= clock0 WHEN (outdata_reg_b = "CLOCK0") ELSE '0';
   temp_xhdl29 <= clock1 WHEN (outdata_reg_b = "CLOCK1") ELSE (temp_xhdl28);
   i_outdata_clk_b <= temp_xhdl29 ;
   temp_xhdl30 <= clock0 WHEN (wrcontrol_wraddress_reg_b = "CLOCK0") ELSE '0';
   temp_xhdl31 <= clock1 WHEN (wrcontrol_wraddress_reg_b = "CLOCK1") ELSE (temp_xhdl30);
   i_wrcontrol_wraddress_clk_b <= temp_xhdl31 ;
   temp_xhdl32 <= clock0 WHEN (rdcontrol_reg_b = "CLOCK0") ELSE '0';
   temp_xhdl33 <= clock1 WHEN (rdcontrol_reg_b = "CLOCK1") ELSE (temp_xhdl32);
   i_rdcontrol_clk_b <= temp_xhdl33 ;
   temp_xhdl34 <= clock0 WHEN (address_reg_b = "CLOCK0") ELSE '0';
   temp_xhdl35 <= clock1 WHEN (address_reg_b = "CLOCK1") ELSE (temp_xhdl34);
   i_address_clk_b <= temp_xhdl35 ;
   temp_xhdl36 <= clock0 WHEN (byteena_reg_b = "CLOCK0") ELSE '0';
   temp_xhdl37 <= clock1 WHEN (byteena_reg_b = "CLOCK1") ELSE (temp_xhdl36);
   i_byteena_clk_b <= temp_xhdl37 ;
   -- clock enables:
   
   -- port a:
   i_clken_a <= clocken0 ;
   temp_xhdl38 <= clocken0 WHEN (outdata_reg_a = "CLOCK0") ELSE '0';
   temp_xhdl39 <= clocken1 WHEN (outdata_reg_a = "CLOCK1") ELSE (temp_xhdl38);
   i_outdata_clken_a <= temp_xhdl39 ;
   temp_xhdl40 <= clocken1 WHEN (indata_reg_b = "CLOCK1") ELSE '0';
   temp_xhdl41 <= clocken0 WHEN (indata_reg_b = "CLOCK0") ELSE (temp_xhdl40);
   i_indata_clken_b <= temp_xhdl41 ;
   temp_xhdl42 <= clocken1 WHEN (outdata_reg_b = "CLOCK1") ELSE '0';
   temp_xhdl43 <= clocken0 WHEN (outdata_reg_b = "CLOCK0") ELSE (temp_xhdl42);
   i_outdata_clken_b <= temp_xhdl43 ;
   temp_xhdl44 <= clocken1 WHEN (wrcontrol_wraddress_reg_b = "CLOCK1") ELSE '0';
   temp_xhdl45 <= clocken0 WHEN (wrcontrol_wraddress_reg_b = "CLOCK0") ELSE (temp_xhdl44);
   i_wrcontrol_wraddress_clken_b <= temp_xhdl45 ;
   temp_xhdl46 <= clocken1 WHEN (rdcontrol_reg_b = "CLOCK1") ELSE '0';
   temp_xhdl47 <= clocken0 WHEN (rdcontrol_reg_b = "CLOCK0") ELSE (temp_xhdl46);
   i_rdcontrol_clken_b <= temp_xhdl47 ;
   temp_xhdl48 <= clocken1 WHEN (address_reg_b = "CLOCK1") ELSE '0';
   temp_xhdl49 <= clocken0 WHEN (address_reg_b = "CLOCK0") ELSE (temp_xhdl48);
   i_address_clken_b <= temp_xhdl49 ;
   temp_xhdl50 <= clocken1 WHEN (byteena_reg_b = "CLOCK1") ELSE '0';
   temp_xhdl51 <= clocken0 WHEN (byteena_reg_b = "CLOCK0") ELSE (temp_xhdl50);
   i_byteena_clken_b <= temp_xhdl51 ;
   temp_xhdl52 <= i_aclr0 WHEN (indata_aclr_a = "CLEAR0") ELSE '0';
   i_indata_aclr_a <= temp_xhdl52 ;
   temp_xhdl53 <= i_aclr0 WHEN (address_aclr_a = "CLEAR0") ELSE '0';
   i_address_aclr_a <= temp_xhdl53 ;
   temp_xhdl54 <= i_aclr0 WHEN (wrcontrol_aclr_a = "CLEAR0") ELSE '0';
   i_wrcontrol_aclr_a <= temp_xhdl54 ;
   temp_xhdl55 <= i_aclr1 WHEN (byteena_aclr_a = "CLEAR1") ELSE '0';
   temp_xhdl56 <= i_aclr0 WHEN (byteena_aclr_a = "CLEAR0") ELSE (temp_xhdl55);
   i_byteena_aclr_a <= temp_xhdl56 ;
   temp_xhdl57 <= i_aclr1 WHEN (outdata_aclr_a = "CLEAR1") ELSE '0';
   temp_xhdl58 <= i_aclr0 WHEN (outdata_aclr_a = "CLEAR0") ELSE (temp_xhdl57);
   i_outdata_aclr_a <= temp_xhdl58 ;
   temp_xhdl59 <= i_aclr1 WHEN (indata_aclr_b = "CLEAR1") ELSE '0';
   temp_xhdl60 <= i_aclr0 WHEN (indata_aclr_b = "CLEAR0") ELSE (temp_xhdl59);
   i_indata_aclr_b <= temp_xhdl60 ;
   temp_xhdl61 <= i_aclr1 WHEN (address_aclr_b = "CLEAR1") ELSE '0';
   temp_xhdl62 <= i_aclr0 WHEN (address_aclr_b = "CLEAR0") ELSE (temp_xhdl61);
   i_address_aclr_b <= temp_xhdl62 ;
   temp_xhdl63 <= i_aclr1 WHEN (wrcontrol_aclr_b = "CLEAR1") ELSE '0';
   temp_xhdl64 <= i_aclr0 WHEN (wrcontrol_aclr_b = "CLEAR0") ELSE (temp_xhdl63);
   i_wrcontrol_aclr_b <= temp_xhdl64 ;
   temp_xhdl65 <= i_aclr1 WHEN (rdcontrol_aclr_b = "CLEAR1") ELSE '0';
   temp_xhdl66 <= i_aclr0 WHEN (rdcontrol_aclr_b = "CLEAR0") ELSE (temp_xhdl65);
   i_rdcontrol_aclr_b <= temp_xhdl66 ;
   temp_xhdl67 <= i_aclr1 WHEN (byteena_aclr_b = "CLEAR1") ELSE '0';
   temp_xhdl68 <= i_aclr0 WHEN (byteena_aclr_b = "CLEAR0") ELSE (temp_xhdl67);
   i_byteena_aclr_b <= temp_xhdl68 ;
   temp_xhdl69 <= i_aclr1 WHEN (outdata_aclr_b = "CLEAR1") ELSE '0';
   temp_xhdl70 <= i_aclr0 WHEN (outdata_aclr_b = "CLEAR0") ELSE (temp_xhdl69);
   i_outdata_aclr_b <= temp_xhdl70 ;

   -- registers:
   
   -- PORT A:
   
   PROCESS (i_clk_a)
      VARIABLE i_byteena_mask_reg_a_xhdl71  : std_logic_vector(width_a - 1 DOWNTO 0);
   BEGIN
      IF (i_clk_a'EVENT AND i_clk_a = '1') THEN
         IF (i_clken_a = '1') THEN
            IF (i_indata_aclr_a = '1') THEN
               i_data_reg_a <= (OTHERS => '0');    
            ELSE
               IF (NOW > 0 ns) THEN
                  i_data_reg_a <= data_a;    
               END IF;
            END IF;
            IF (i_wrcontrol_aclr_a = '1') THEN
               i_wren_reg_a <= '0';    
            ELSE
               IF (NOW > 0 ns) THEN
                  i_wren_reg_a <= i_wren_a;    
               END IF;
            END IF;

		write_conflict_reg_a <= write_conflict;

            IF (byteena_a_unconnected = '1') THEN
               i_byteena_mask_reg_a_xhdl71 := (OTHERS => '1');    
            ELSE
               IF (i_byteena_aclr_a = '1') THEN
                  i_byteena_mask_reg_a_xhdl71 := (OTHERS => '0');    
               ELSE
                  IF (NOW > 0 ns) THEN
                     -- dilate the byteena into a byteena_mask:
                     
                     FOR k IN 0 TO (width_a - 1) LOOP
                        -- non-blocking here causes read-a to see the byteena too late
                        
                        i_byteena_mask_reg_a_xhdl71(k) := byteena_a(k / byte_size);    
                     END LOOP;
                  END IF;
               END IF;
            END IF;
            IF (i_address_aclr_a = '1') THEN
               i_address_reg_a <= (OTHERS => '0');    
            ELSE
               IF (NOW > 0 ns) THEN
                  i_address_reg_a <= address_a;    
               END IF;
            END IF;
         END IF;
      END IF;
      i_byteena_mask_reg_a <= i_byteena_mask_reg_a_xhdl71;
   END PROCESS;

   -- PORT B
   
   PROCESS (i_indata_clk_b)
   BEGIN
      IF (i_indata_clk_b'EVENT AND i_indata_clk_b = '1') THEN
         IF (i_indata_clken_b = '1') THEN
            IF (i_indata_aclr_b = '1') THEN
               i_data_reg_b <= (OTHERS => '0');    
            ELSE
               IF (NOW > 0 ns) THEN
                  i_data_reg_b <= data_b;    
               END IF;
            END IF;
         END IF;
      END IF;
   END PROCESS;

   PROCESS (i_address_clk_b)
   BEGIN
      IF (i_address_clk_b'EVENT AND i_address_clk_b = '1') THEN
         IF (i_address_clken_b = '1') THEN
            IF (i_address_aclr_b = '1') THEN
               i_address_reg_b <= (OTHERS => '0');    
            ELSE
               IF (NOW > 0 ns) THEN
                  i_address_reg_b <= address_b;    
               END IF;
            END IF;
         END IF;
      END IF;
   END PROCESS;

   PROCESS (i_wrcontrol_wraddress_clk_b)
      VARIABLE i_byteena_mask_reg_b_xhdl72  : std_logic_vector(width_b - 1 DOWNTO 0);
   BEGIN
      IF (i_wrcontrol_wraddress_clk_b'EVENT AND i_wrcontrol_wraddress_clk_b = '1') THEN
         IF (i_wrcontrol_wraddress_clken_b = '1') THEN
            IF (i_wrcontrol_aclr_b = '1') THEN
               i_wren_reg_b <= '0';    
            ELSE
               IF (NOW > 0 ns) THEN
                  i_wren_reg_b <= i_wren_b;    
               END IF;
            END IF;

		write_conflict_reg_b <= write_conflict;

            IF (byteena_b_unconnected = '1') THEN
               i_byteena_mask_reg_b_xhdl72 := (OTHERS => '1');    
            ELSE
               IF (i_byteena_aclr_b = '1') THEN
                  i_byteena_mask_reg_b_xhdl72 := (OTHERS => '0');    
               ELSE
                  IF (NOW > 0 ns) THEN
                     FOR k IN 0 TO (width_b - 1) LOOP
                        i_byteena_mask_reg_b_xhdl72(k) := byteena_b(k / byte_size);    
                     END LOOP;
                  END IF;
               END IF;
            END IF;
         END IF;
      END IF;
      i_byteena_mask_reg_b <= i_byteena_mask_reg_b_xhdl72;
   END PROCESS;

   -- dilate the byteena into a byteena_mask:
   
   PROCESS (i_rdcontrol_clk_b)
   BEGIN
      IF (i_rdcontrol_clk_b'EVENT AND i_rdcontrol_clk_b = '1') THEN
         IF (i_rdcontrol_clken_b = '1') THEN
            IF (operation_mode /= "DUAL_PORT" and operation_mode /="BIDIR_DUAL_PORT" ) THEN
						if (now > 0 ns) then
               i_rden_reg_b <= '0';    
						end if;
            ELSE
               IF (i_rdcontrol_aclr_b = '1') THEN
                  i_rden_reg_b <= '0';    
               ELSE
                  IF (NOW > 0 ns) THEN
                     i_rden_reg_b <= i_rden_b;    
                  END IF;
               END IF;
            END IF;
         END IF;
      END IF;
   END PROCESS;

   PROCESS (clock0, clock1, aclr0, aclr1)
   BEGIN
			IF ( outdata_aclr_b = "CLEAR0" and aclr0 = '1') or ( outdata_aclr_b = "CLEAR1" and aclr1 = '1') THEN
         i_q_reg_b <= (OTHERS => '0');    
      ELSIF ( outdata_reg_b = "CLOCK0" and clock0'EVENT AND clocken0 = '1' and clock0 = '1') or ( outdata_reg_b = "CLOCK1" and clock1'EVENT AND clocken1 = '1' and clock1 = '1') THEN
        i_q_reg_b <= i_q_tmp_b;    
      END IF;

--      IF (i_outdata_aclr_b = '1') THEN
  --       i_q_reg_b <= (OTHERS => '0');    
    --  ELSIF (i_outdata_clk_b'EVENT AND i_outdata_clk_b = '1') THEN
      --   IF (i_outdata_clken_b  = '1') THEN
        --    i_q_reg_b <= i_q_tmp_b;    
--         END IF;
  --    END IF;
   END PROCESS;

   -- XXX is this sync or async?
   
   --=========
   
   -- Memory
   
   --=========
   
   -- count the number of input clocks. when the second clock
   
   -- gets here, then we are good to output some data
   
   PROCESS (i_clk_a)
   BEGIN
      IF (i_clk_a'EVENT AND i_clk_a = '1') THEN
         IF (i_clken_a = '1') THEN
            good_to_go_a <= '1';    
         END IF;
      END IF;
   END PROCESS;


   PROCESS (i_address_clk_b)
   BEGIN
      IF (i_address_clk_b'EVENT AND i_address_clk_b = '1') THEN
         IF (i_address_clken_b = '1') THEN
            good_to_go_b <= '1';    
         END IF;
      END IF;
   END PROCESS;

 
   PROCESS (i_clk_a)
   BEGIN
      IF (i_clk_a'EVENT AND i_clk_a = '1') THEN
	 IF (i_clken_a = '1') THEN
	         write_by_b_reg_a <= write_by_b;    
	end if;
      END IF;
   END PROCESS;

   -- port a reading:
   
   -- (legal for BDP, SP, ROM only)
   
   PROCESS (i_address_reg_a, write_by_b_reg_a, write_by_a)
     VARIABLE i_q_tmp2_a_xhdl81  : std_logic_vector(width_a - 1 DOWNTO 0);
   BEGIN
      IF (operation_mode = "BIDIR_DUAL_PORT" OR operation_mode = "SINGLE_PORT" OR operation_mode = "ROM") THEN
         i_q_tmp2_a_xhdl81 := mem_data(conv_integer(unsigned(i_address_reg_a)));    
      END IF;
      i_q_tmp2_a <= i_q_tmp2_a_xhdl81;
   END PROCESS;

	PROCESS (clock0, clock1, aclr0, aclr1)
	
	 BEGIN
      -- XXX is this sync or async?
      IF ( outdata_aclr_a = "CLEAR0" and aclr0 = '1') or ( outdata_aclr_a = "CLEAR1" and aclr1 = '1') THEN
         i_q_reg_a <= (OTHERS => '0');    
      ELSIF ( outdata_reg_a = "CLOCK0" and clock0'EVENT AND clocken0 = '1' and clock0 = '1') or ( outdata_reg_a = "CLOCK1" and clock1'EVENT AND clocken1 = '1' and clock1 = '1') THEN
			  i_q_reg_a <= i_q_tmp_a;    
      END IF;
   END PROCESS;

	i_q_tmp_wren_a <= i_q_tmp2_a when i_wren_reg_a = '0' else ((i_data_reg_a AND i_byteena_mask_reg_a) OR (i_q_tmp2_a AND NOT i_byteena_mask_reg_a));
	i_q_tmp_a <= i_q_tmp_wren_a when good_to_go_a = '1' else (OTHERS => default_val);
  
	 temp_xhdl87 <= i_q_reg_a WHEN (outdata_reg_a = "CLOCK0" OR outdata_reg_a = "CLOCK1") ELSE i_q_tmp_a;
   temp_xhdl88 <= (others => '0') WHEN (operation_mode = "DUAL_PORT") ELSE (temp_xhdl87);
   q_a <= temp_xhdl88 ;


   PROCESS (i_address_clk_b)
   BEGIN
      IF (i_address_clk_b'EVENT AND i_address_clk_b = '1') THEN
         IF (i_address_clken_b ='1' ) then --OR (ram_block_type = "MEGARAM")) THEN
            write_by_a_reg_b <= write_by_a;    
         END IF;
      END IF;
   END PROCESS;

   PROCESS (i_rden_reg_b, i_address_reg_b, write_by_a_reg_b, write_by_b)
      VARIABLE j  : integer;
      VARIABLE temp_wa2_xhdl84  : std_logic_vector(width_a - 1 DOWNTO 0);
      VARIABLE temp_wb_xhdl85  : std_logic_vector(width_b - 1 DOWNTO 0);
   BEGIN
--      IF (ram_block_type /= "MEGARAM") THEN
--         WAIT UNTIL i_rden_reg_b'EVENT OR i_address_reg_b'EVENT OR write_by_a_reg_b'EVENT OR write_by_b'EVENT;
         
  --    ELSE
    --     WAIT UNTIL i_rden_reg_b'EVENT OR i_address_reg_b'EVENT OR write_by_a'EVENT OR write_by_b'EVENT;
         
      --END IF;
      IF (operation_mode = "DUAL_PORT" OR operation_mode = "BIDIR_DUAL_PORT") THEN
         IF (i_rden_reg_b = '1' or operation_mode = "BIDIR_DUAL_PORT") THEN
            j := conv_integer(unsigned(i_address_reg_b)) * width_b;    
            FOR i IN 0 TO (width_b - 1) LOOP
               temp_wa2_xhdl84 := mem_data((j + i) / width_a);    
               temp_wb_xhdl85(i) := temp_wa2_xhdl84((j + i) mod width_a);    
            END LOOP;
            i_q_tmp2_b <= temp_wb_xhdl85;    
         END IF;
      END IF;
      
   END PROCESS;

   PROCESS (i_q_tmp2_b, good_to_go_b, i_wren_reg_b, i_data_reg_b, i_address_reg_b, i_byteena_mask_reg_b)
      VARIABLE i_q_tmp_b_xhdl86  : std_logic_vector(width_b - 1 DOWNTO 0);
   BEGIN
      IF (operation_mode = "DUAL_PORT" OR operation_mode = "BIDIR_DUAL_PORT") THEN
         IF (NOT good_to_go_b = '1') THEN
            i_q_tmp_b_xhdl86 := (OTHERS => default_val);    
         ELSE
            IF (operation_mode = "DUAL_PORT") THEN
               IF ((i_rden_b AND NOT i_rdcontrol_aclr_b) = '1') THEN
                  i_q_tmp_b_xhdl86 := i_q_tmp2_b;    
               END IF;
            ELSE
               IF (operation_mode = "BIDIR_DUAL_PORT") THEN
                  IF (i_wren_reg_b = '1') THEN
                     i_q_tmp_b_xhdl86 := (i_data_reg_b AND i_byteena_mask_reg_b) OR (i_q_tmp2_b AND NOT i_byteena_mask_reg_b);    
                  ELSE
                     i_q_tmp_b_xhdl86 := i_q_tmp2_b;    
                  END IF;
               END IF;
            END IF;
         END IF;
      END IF;
      i_q_tmp_b <= i_q_tmp_b_xhdl86;
   END PROCESS;

   temp_xhdl89 <= i_q_reg_b WHEN (outdata_reg_b = "CLOCK0" OR outdata_reg_b = "CLOCK1") ELSE i_q_tmp_b;
   temp_xhdl90 <= (others => '0') WHEN (operation_mode = "SINGLE_PORT" OR operation_mode = "ROM") ELSE (temp_xhdl89);
   q_b_xhdl2 <= temp_xhdl90 ;

END translated;

Library ieee;
use ieee.std_logic_1164.all;

Package pllpack is

	function alt_conv_integer(arg : in std_logic_vector) return integer;
        function gcd (X: integer; Y: integer) return integer;

        function lcm (A1: integer; A2: integer; A3: integer; A4: integer;
                      A5: integer; A6: integer; A7: integer;
                      A8: integer; A9: integer; A10: integer; P: integer) return integer;

        function output_counter_value (clk_divide: integer; clk_mult : integer ;
                M: integer; N: integer ) return integer;

        function counter_mode (duty_cycle: integer; output_counter_value: integer) return string;

        function counter_high (output_counter_value: integer := 1; duty_cycle: integer)
                             return integer;

        function counter_low (output_counter_value: integer; duty_cycle: integer)
                           return integer;

        function mintimedelay (t1: integer; t2: integer; t3: integer; t4: integer;
                             t5: integer; t6: integer; t7: integer; t8: integer;
                             t9: integer; t10: integer) return integer;

        function maxtimedelay (t1: integer; t2: integer; t3: integer; t4: integer;
                             t5: integer; t6: integer; t7: integer; t8: integer;
                             t9: integer; t10: integer) return integer;

        function maxnegabs (t1: integer; t2: integer; t3: integer; t4: integer;
                             t5: integer; t6: integer; t7: integer; t8: integer;
                             t9: integer; t10: integer) return integer;

        function counter_time_delay ( clk_time_delay: integer;
                                    m_time_delay: integer; n_time_delay: integer)
                                return integer;


        function counter_initial (tap_phase: integer; m: integer; n: integer)
                         return integer;

        function counter_ph (tap_phase: integer; m : integer; n: integer) return integer;

        function ph_adjust (tap_phase: integer; ph_base : integer) return integer;

        function translate_string (mode : string) return string;
	
	function str2int (s : string) return integer;

end pllpack;

package body pllpack is

function alt_conv_integer(arg : in std_logic_vector) return integer is
variable result : integer;
begin
  result := 0;
  for i in arg'range loop
     if arg(i) = '1' then
        result := result + 2**i;
     end if;
  end loop;
  return result;
end alt_conv_integer;

function gcd (X: integer; Y: integer) return integer is
variable L, S, R, G : integer;
begin
        if X < Y then -- find which is smaller.
           S := X;
           L := Y;
        else
           S := Y;
           L := X;
        end if;

        R := S;
        while ( R > 1) loop
                S := L;
                L := R;
                R := S rem L; -- divide bigger number by smaller.
                              -- remainder becomes smaller number.
        end loop;
        if R = 0 then  -- if evenly divisible then L is gcd else it is 1.
                G := L;
        else
                G := R;
        end if;
        return G;
end gcd;

function lcm (A1: integer; A2: integer; A3: integer; A4: integer;
              A5: integer; A6: integer; A7: integer;
              A8: integer; A9: integer; A10: integer; P: integer) return integer is
variable M1, M2, M3, M4, M5 , M6, M7, M8, M9: integer;
begin
        M1 := (A1 * A2)/gcd(A1, A2);
        M2 := (M1 * A3)/gcd(M1, A3);
        M3 := (M2 * A4)/gcd(M2, A4);
        M4 := (M3 * A5)/gcd(M3, A5);
        M5 := (M4 * A6)/gcd(M4, A6);
        M6 := (M5 * A7)/gcd(M5, A7);
        M7 := (M6 * A8)/gcd(M6, A8);
        M8 := (M7 * A9)/gcd(M7, A9);
        M9 := (M8 * A10)/gcd(M8, A10);
        return M9 * P/100;
end lcm;

function output_counter_value (clk_divide: integer; clk_mult: integer ;
                M: integer; N: integer ) return integer is
Variable R: integer;
begin
        R := (clk_divide * M)/(clk_mult * N);
        return R;
end output_counter_value;

function counter_mode (duty_cycle: integer; output_counter_value: integer) return string is
variable R: string (1 to 6);
variable counter_value: integer;
begin
        counter_value := (2*duty_cycle*output_counter_value)/100;
        if output_counter_value = 1 then
                R := "bypass";
        elsif (counter_value REM 2) = 0 then
                R := "  even";
        else
                R := "   odd";
        end if;
        return R;
end counter_mode;

function counter_high (output_counter_value: integer := 1; duty_cycle: integer)
                       return integer is
variable R: integer;
variable half_cycle_high : integer;
begin
        half_cycle_high := (duty_cycle * output_counter_value *2)/100 ;
	if (half_cycle_high/2 = 0) then
	 R := half_cycle_high/2 ;
	else
	 R := half_cycle_high/2 + 1;
	end if;
        return R;
end;

function counter_low (output_counter_value: integer; duty_cycle: integer)
                        return integer is
variable R, R1: integer;
variable half_cycle_high : integer;
begin
        half_cycle_high := (duty_cycle * output_counter_value*2)/100 ;
	if (half_cycle_high/2 = 0) then
	 R1 := half_cycle_high/2 ;
	else
	 R1 := half_cycle_high/2 + 1;
	end if;
	
        R :=    output_counter_value - R1;
        return R;
end;

function mintimedelay (t1: integer; t2: integer; t3: integer; t4: integer;
                       t5: integer; t6: integer; t7: integer; t8: integer;
                       t9: integer; t10: integer) return integer is
variable m1,m2,m3,m4,m5,m6,m7,m8,m9 : integer;
begin
        if t1 < t2 then m1 := t1; else m1 := t2; end if;
        if m1 < t3 then m2 := m1; else m2 := t3; end if;
        if m2 < t4 then m3 := m2; else m3 := t4; end if;
        if m3 < t5 then m4 := m3; else m4 := t5; end if;
        if m4 < t6 then m5 := m4; else m5 := t6; end if;
        if m5 < t7 then m6 := m5; else m6 := t7; end if;
        if m6 < t8 then m7 := m6; else m7 := t8; end if;
        if m7 < t9 then m8 := m7; else m8 := t9; end if;
        if m8 < t10 then m9 := m8; else m9 := t10; end if;
        return m9;
end;

function maxtimedelay (t1: integer; t2: integer; t3: integer; t4: integer;
                       t5: integer; t6: integer; t7: integer; t8: integer;
                       t9: integer; t10: integer) return integer is
variable m1,m2,m3,m4,m5,m6,m7,m8,m9 : integer;
begin
        if t1 > t2 then m1 := t1; else m1 := t2; end if;
        if m1 > t3 then m2 := m1; else m2 := t3; end if;
        if m2 > t4 then m3 := m2; else m3 := t4; end if;
        if m3 > t5 then m4 := m3; else m4 := t5; end if;
        if m4 > t6 then m5 := m4; else m5 := t6; end if;
        if m5 > t7 then m6 := m5; else m6 := t7; end if;
        if m6 > t8 then m7 := m6; else m7 := t8; end if;
        if m7 > t9 then m8 := m7; else m8 := t9; end if;
        if m8 > t10 then m9 := m8; else m9 := t10; end if;
        return m9;
end;

function maxnegabs (t1: integer; t2: integer; t3: integer; t4: integer;
                    t5: integer; t6: integer; t7: integer; t8: integer;
                    t9: integer; t10: integer) return integer is
variable m1,m2,m3,m4,m5,m6,m7,m8,m9 : integer;
begin
        if t1 < t2 then m1 := t1; else m1 := t2; end if;
        if m1 < t3 then m2 := m1; else m2 := t3; end if;
        if m2 < t4 then m3 := m2; else m3 := t4; end if;
        if m3 < t5 then m4 := m3; else m4 := t5; end if;
        if m4 < t6 then m5 := m4; else m5 := t6; end if;
        if m5 < t7 then m6 := m5; else m6 := t7; end if;
        if m6 < t8 then m7 := m6; else m7 := t8; end if;
        if m7 < t9 then m8 := m7; else m8 := t9; end if;
        if m8 < t10 then m9 := m8; else m9 := t10; end if;
	if m9 < 0 then return (0 - m9); else return 0; end if;
end;

function ph_adjust (tap_phase: integer; ph_base : integer) return integer is
begin
	return tap_phase + ph_base;
end;
	

function counter_time_delay ( clk_time_delay: integer;
                              m_time_delay: integer; n_time_delay: integer)
                              return integer is
variable R: integer;
begin
        R := clk_time_delay + m_time_delay - n_time_delay;
        return R;
end;

function counter_initial (tap_phase: integer; m: integer; n: integer)
                         return integer is
variable R: integer;
begin
        R := ((tap_phase * m)/(360 * n)) + 1;
        return R;
end;

function counter_ph (tap_phase: integer; m: integer; n: integer) return integer is
variable R: integer;
begin
        R := (2*((tap_phase * m/n) REM 360) + 45)/90;
        return R;
end;

function translate_string (mode : string) return string is
variable new_mode : string (1 to 6);
begin
     if (mode = "bypass") then
        new_mode := "bypass";
     elsif (mode = "even") then
        new_mode := "  even";
     elsif (mode = "odd") then
        new_mode := "   odd";
     end if;

     return new_mode;
       
end;

	function str2int (s : string) return integer is
	variable len : integer := s'length;
        variable newdigit : integer := 0;
        variable sign : integer := 1;
        variable digit : integer;
        begin
        for i in 1 to len loop
              case s(i) is
                 when '-' =>
                   if i = 1 then
                     sign := -1;
                   else
                    ASSERT FALSE
                    REPORT "Illegal Character "&  s(i) & "i n string parameter! "
                    SEVERITY ERROR;
                   end if;
                 when '0' =>
                       digit := 0;
                 when '1' =>
                       digit := 1;
                 when '2' =>
                       digit := 2;
                 when '3' =>
                       digit := 3;
                 when '4' =>
                       digit := 4;
                 when '5' =>
                       digit := 5;
                 when '6' =>
                       digit := 6;
                 when '7' =>
                       digit := 7;
                 when '8' =>
                       digit := 8;
                 when '9' =>
                       digit := 9;
                 when others =>
                     ASSERT FALSE
                     REPORT "Illegal Character "&  s(i) & "in string parameter! "
                     SEVERITY ERROR;
               end case;
               newdigit := newdigit * 10 + digit;
        end loop;

               return (sign*newdigit);
	end;
end pllpack;

library ieee;
use ieee.std_logic_1164.all;
entity DFFE is
	port(CLK : in std_logic;
             ENA : in std_logic := '1';
             D : in std_logic;
             CLRN : in std_logic := '1';
             PRN : in std_logic := '1';
             Q : out std_logic);

end DFFE;
architecture behave of DFFE is
begin
	process (CLK, PRN, CLRN)
        begin
	  if (PRN = '0') then Q <= '1';
	  elsif (CLRN = '0') then Q <= '0';
	  elsif (CLK'event and (ENA = '1')) then Q <= D;
	  end if;
	end process;
end behave;


--
-- MN_CNTR
--

library ieee;
use IEEE.std_logic_1164.all;

entity mn_cntr is
   port ( clk : IN std_logic;
          reset : IN std_logic;
          cout : OUT std_logic;
          initial_value : IN integer;
          modulus : IN integer;
          time_delay : IN integer;
          ph : IN integer;
          fb_delay : IN integer
   );
end mn_cntr;
architecture behave of mn_cntr is
begin
  process (clk, reset)
  variable count : integer := 1;
  variable first_rising_edge : boolean := true;
  variable tmp_cout : std_logic;
--  variable clk_delay : time := time_delay * 1 ps;
  begin
     if (reset = '1') then
        count := 1;
        tmp_cout := '0';
     elsif (clk'event and clk = '1' and first_rising_edge) then
        first_rising_edge := false;
        tmp_cout := clk;
     elsif (not first_rising_edge) then
        if (count < modulus) then
           count := count + 1;
        else
           count := 1;
           tmp_cout := not tmp_cout;
        end if;
     else
        tmp_cout := clk;
     end if;
     cout <= transport tmp_cout after (time_delay + fb_delay) * 1 ps;
  end process;
end behave;


library ieee;
use IEEE.std_logic_1164.all;

entity scale_cntr is
   port (clk : IN std_logic;
         reset : IN std_logic;
         cout : OUT std_logic;
         initial : IN integer;
         high : IN integer;
         low : IN integer;
         mode : IN string := "bypass";
         time_delay : IN integer
   );
end scale_cntr;

architecture behave of scale_cntr is
begin
   process (clk, reset)
   variable tmp_cout : std_logic := '0';
   variable count : integer := 1;
   variable output_shift_count : integer := 0;
   variable first_rising_edge : boolean := false;
--   variable clk_delay : time := time_delay * 1 ps;
   begin
      if (reset = '1') then
         count := 1;
         output_shift_count := 0;
         tmp_cout := '0';
         first_rising_edge := false;
      elsif (clk'event) then
         if (mode = "bypass") then
            tmp_cout := clk;
         elsif (not first_rising_edge) then
            if (clk = '1') then
               if (initial = 1) then
                  tmp_cout := clk;
                  first_rising_edge := true;
               else
                  output_shift_count := output_shift_count + 1;
                  if (output_shift_count = initial) then
                     tmp_cout := clk;
                     first_rising_edge := true;
                  end if;
               end if;
            end if;
         else
            count := count + 1;
            if (mode = "  even" and (count = (high*2) + 1)) then
               tmp_cout := '0';
            elsif (mode = "   odd" and (count = high*2)) then
               tmp_cout := '0';
            elsif (count = (high + low)*2 + 1) then
               tmp_cout := '1';
               count := 1;  -- reset count
            end if;
         end if;
      end if;
      cout <= transport tmp_cout after time_delay * 1 ps;
   end process;
end behave;

library ieee;
use IEEE.std_logic_1164.all;
use work.pllpack.all;

entity stratix_pll is
   generic (operation_mode : string := "normal";
            qualify_conf_done : string := "off";
            compensate_clock : string := "clk0";
            pll_type : string := "auto";   -- EGPP/HSSI/AUTO
            scan_chain : string := "long";

            clk0_multiply_by : integer := 1;
            clk0_divide_by : integer := 1;
            clk0_phase_shift : string := "0";
            clk0_time_delay : string := "0";
            clk0_duty_cycle : integer := 50;

            clk1_multiply_by : integer := 1;
            clk1_divide_by : integer := 1;
            clk1_phase_shift : string := "0";
            clk1_time_delay : string := "0";
            clk1_duty_cycle : integer := 50;

            clk2_multiply_by : integer := 1;
            clk2_divide_by : integer := 1;
            clk2_phase_shift : string := "0";
            clk2_time_delay : string := "0";
            clk2_duty_cycle : integer := 50;

            clk3_multiply_by : integer := 1;
            clk3_divide_by : integer := 1;
            clk3_phase_shift : string := "0";
            clk3_time_delay : string := "0";
            clk3_duty_cycle : integer := 50;

            clk4_multiply_by : integer := 1;
            clk4_divide_by : integer := 1;
            clk4_phase_shift : string := "0";
            clk4_time_delay : string := "0";
            clk4_duty_cycle : integer := 50;

            clk5_multiply_by : integer := 1;
            clk5_divide_by : integer := 1;
            clk5_phase_shift : string := "0";
            clk5_time_delay : string := "0";
            clk5_duty_cycle : integer := 50;

            extclk0_multiply_by : integer := 1;
            extclk0_divide_by : integer := 1;
            extclk0_phase_shift : string := "0";
            extclk0_time_delay : string := "0";
            extclk0_duty_cycle : integer := 50;

            extclk1_multiply_by : integer := 1;
            extclk1_divide_by : integer := 1;
            extclk1_phase_shift : string := "0";
            extclk1_time_delay : string := "0";
            extclk1_duty_cycle : integer := 50;

            extclk2_multiply_by : integer := 1;
            extclk2_divide_by : integer := 1;
            extclk2_phase_shift : string := "0";
            extclk2_time_delay : string := "0";
            extclk2_duty_cycle : integer := 50;

            extclk3_multiply_by : integer := 1;
            extclk3_divide_by : integer := 1;
            extclk3_phase_shift : string := "0";
            extclk3_time_delay : string := "0";
            extclk3_duty_cycle : integer := 50;

            primary_clock : string := "inclk0";
            inclk0_input_frequency : integer := 10000;
            inclk1_input_frequency : integer := 10000;
            gate_lock_signal : string := "yes";
            gate_lock_counter : integer := 1;
            lock_high : integer := 1;
            lock_low : integer := 1;
            valid_lock_multiplier : integer := 5;
            invalid_lock_multiplier : integer := 5;

-- need default values for the foll.
            switch_over_on_lossclk : string := "off";
            switch_over_on_gated_lock : string := "off";
            switch_over_counter : integer := 1;
            enable_switch_over_counter : string := "off";
            feedback_source : string := "e0";
--          bandwidth = "";
--          up_spread = "";
--          down_spread = "";

-- ADVANCED USE PARAMETERS
            m_initial : integer := 1;     -- 1-1024
            m : integer := 4;     -- 1-1024
            n: integer := 4;     -- 1-1024
            m2 : integer := 1;     -- 1-1024
            n2 : integer := 1;     -- 1-1024
            ss : integer := 1;     -- 1-32768

            l0_high : integer := 2;    -- 1-512
            l0_low : integer := 2;    -- 1-512
            l0_initial : integer := 1;    -- 1-512
            l0_mode : string := "   odd";       -- bypass,odd,even
            l0_ph : integer := 0;
            l0_time_delay : integer := 0;

            l1_high : integer := 1;
            l1_low : integer := 1;
            l1_initial : integer := 1;
            l1_mode : string := "bypass";
            l1_ph : integer := 0;
            l1_time_delay : integer := 0;

            g0_high : integer := 1;
            g0_low : integer := 1;
            g0_initial : integer := 1;
            g0_mode : string := "bypass";
            g0_ph : integer := 0;
            g0_time_delay : integer := 0;

            g1_high : integer := 1;
            g1_low : integer := 1;
            g1_initial : integer := 1;
            g1_mode : string := "bypass";
            g1_ph : integer := 0;
            g1_time_delay : integer := 0;

            g2_high : integer := 1;
            g2_low : integer := 1;
            g2_initial : integer := 1;
            g2_mode : string := "bypass";
            g2_ph : integer := 0;
            g2_time_delay : integer := 0;

            g3_high : integer := 1;
            g3_low : integer := 1;
            g3_initial : integer := 1;
            g3_mode : string := "bypass";
            g3_ph : integer := 0;
            g3_time_delay : integer := 0;

            e0_high : integer := 1;
            e0_low : integer := 1;
            e0_initial : integer := 1;
            e0_mode : string := "bypass";
            e0_ph : integer := 0;
            e0_time_delay : integer := 0;

            e1_high : integer := 1;
            e1_low : integer := 1;
            e1_initial : integer := 1;
            e1_mode : string := "bypass";
            e1_ph : integer := 0;
            e1_time_delay : integer := 0;

            e2_high : integer := 1;
            e2_low : integer := 1;
            e2_initial : integer := 1;
            e2_mode : string := "bypass";
            e2_ph : integer := 0;
            e2_time_delay : integer := 0;

            e3_high : integer := 1;
            e3_low : integer := 1;
            e3_initial : integer := 1;
            e3_mode : string := "bypass";
            e3_ph : integer := 0;
            e3_time_delay : integer := 0;

            m_ph : integer := 0;
            m_time_delay : integer := 0;
            n_time_delay : integer := 0;

            extclk0_counter : string := "e0";
            extclk1_counter : string := "e1";
            extclk2_counter : string := "e2";
            extclk3_counter : string := "e3";

            clk0_counter : string := "l0";
            clk1_counter : string := "l1";
            clk2_counter : string := "g0";
            clk3_counter : string := "g1";
            clk4_counter : string := "g2";
            clk5_counter : string := "g3";

            -- LVDS mode parameters
            enable0_counter : string := "l0";
            enable1_counter : string := "l0";

            charge_pump_current : integer := 0;

            loop_filter_r : string := "1.0";
            loop_filter_c : integer := 1;
            common_rx_tx : string := "off";

            pll_compensation_delay : integer := 0;
            simulation_type : string := "timing");

   port (inclk : IN std_logic_vector(1 downto 0);
         fbin : IN std_logic;
         ena : IN std_logic;
         clkswitch : IN std_logic;
         areset : IN std_logic;
         pfdena : IN std_logic;
         clkena : IN std_logic_vector(5 downto 0);
         extclkena : IN std_logic_vector(3 downto 0);
         scanaclr : IN std_logic;
         scandata : IN std_logic;
         scanclk : IN std_logic;
         clk : OUT std_logic_vector(5 downto 0);
         extclk : OUT std_logic_vector(3 downto 0);
         clkbad : OUT std_logic_vector(1 downto 0);
         activeclock : OUT std_logic;
         locked : OUT std_logic;
         clkloss : OUT std_logic;
-- lvds specific ports
         comparator : IN std_logic := '1';
         enable0 : OUT std_logic;
         enable1 : OUT std_logic
   );
end stratix_pll;

architecture vital_pll of stratix_pll is

-- internal advanced parameter signals
signal   i_vco_min      : natural;
signal   i_vco_max      : natural;
signal   i_vco_center   : natural;
signal   i_pfd_min      : natural;
signal   i_pfd_max      : natural;
signal   i_l0_ph        : natural;
signal   i_l1_ph        : natural;
signal   i_g0_ph        : natural;
signal   i_g1_ph        : natural;
signal   i_g2_ph        : natural;
signal   i_g3_ph        : natural;
signal   i_e0_ph        : natural;
signal   i_e1_ph        : natural;
signal   i_e2_ph        : natural;
signal   i_e3_ph        : natural;
signal   i_m_ph         : natural;
signal   i_l0_time_delay        : natural;
signal   i_l1_time_delay        : natural;
signal   i_g0_time_delay        : natural;
signal   i_g1_time_delay        : natural;
signal   i_g2_time_delay        : natural;
signal   i_g3_time_delay        : natural;
signal   i_e0_time_delay        : natural;
signal   i_e1_time_delay        : natural;
signal   i_e2_time_delay        : natural;
signal   i_e3_time_delay        : natural;
signal   i_m_time_delay         : natural;
signal   i_n_time_delay         : natural;
signal   i_extclk3_counter      : string(1 to 2) := "e3";
signal   i_extclk2_counter      : string(1 to 2) := "e2";
signal   i_extclk1_counter      : string(1 to 2) := "e1";
signal   i_extclk0_counter      : string(1 to 2) := "e0";
signal   i_clk5_counter         : string(1 to 2) := "g3";
signal   i_clk4_counter         : string(1 to 2) := "g2";
signal   i_clk3_counter         : string(1 to 2) := "g1";
signal   i_clk2_counter         : string(1 to 2) := "g0";
signal   i_clk1_counter         : string(1 to 2) := "l1";
signal   i_clk0_counter         : string(1 to 2) := "l0";
signal   i_charge_pump_current  : natural;
signal   i_loop_filter_r        : natural;

-- end internal advanced parameter signals

-- CONSTANTS
CONSTANT max_gpp_bits : integer := 192;

-- SIGNALS

SIGNAL vcc : std_logic := '1';

SIGNAL fbclk, refclk : std_logic;
SIGNAL m_times_vco_period : time := inclk0_input_frequency * n * 1 ps;
SIGNAL tmp_vco_out : std_logic := '0';
SIGNAL fbclk_tmp, fbclk_tmp1, fbclk_tmp2, fbclk_tmp3, fbclk_tmp4, fbclk_tmp5 : std_logic;
SIGNAL schedule_vco : boolean := true;
SIGNAL schedule_zero : boolean := false;
SIGNAL schedule_from_refclk : boolean := false;
SIGNAL reschedule_from_fbclk : boolean := false;

SIGNAL l0_clk, l1_clk : std_logic;
SIGNAL g0_clk, g1_clk, g2_clk, g3_clk : std_logic;
SIGNAL e0_clk, e1_clk, e2_clk, e3_clk : std_logic;

SIGNAL vco_out : std_logic_vector(7 downto 0);

-- signals to assign values to counter params
SIGNAL m_val : integer := 1;
SIGNAL n_val : integer := 1;
SIGNAL m_time_delay_val, n_time_delay_val : integer := 0;
SIGNAL m_ph_val : integer := 0;
SIGNAL m_initial_val : integer := m_initial;

SIGNAL l0_initial_val : integer := l0_initial;
SIGNAL l1_initial_val : integer := l1_initial;
SIGNAL l0_high_val : integer := l0_high;
SIGNAL l1_high_val : integer := l1_high;
SIGNAL l0_low_val : integer := l0_low;
SIGNAL l1_low_val : integer := l1_low;
SIGNAL l0_mode_val : string(1 to 6) := "bypass";
SIGNAL l1_mode_val : string(1 to 6) := "bypass";
SIGNAL l0_time_delay_val : integer := l0_time_delay;
SIGNAL l1_time_delay_val : integer := l1_time_delay;

SIGNAL g0_initial_val : integer := g0_initial;
SIGNAL g1_initial_val : integer := g1_initial;
SIGNAL g2_initial_val : integer := g2_initial;
SIGNAL g3_initial_val : integer := g3_initial;
SIGNAL g0_high_val : integer := g0_high;
SIGNAL g1_high_val : integer := g1_high;
SIGNAL g2_high_val : integer := g2_high;
SIGNAL g3_high_val : integer := g3_high;
SIGNAL g0_mode_val : string(1 to 6) := "bypass";
SIGNAL g1_mode_val : string(1 to 6) := "bypass";
SIGNAL g2_mode_val : string(1 to 6) := "bypass";
SIGNAL g3_mode_val : string(1 to 6) := "bypass";
SIGNAL g0_low_val : integer := g0_low;
SIGNAL g1_low_val : integer := g1_low;
SIGNAL g2_low_val : integer := g2_low;
SIGNAL g3_low_val : integer := g3_low;
SIGNAL g0_time_delay_val : integer := g0_time_delay;
SIGNAL g1_time_delay_val : integer := g1_time_delay;
SIGNAL g2_time_delay_val : integer := g2_time_delay;
SIGNAL g3_time_delay_val : integer := g3_time_delay;

SIGNAL e0_initial_val : integer := e0_initial;
SIGNAL e1_initial_val : integer := e1_initial;
SIGNAL e2_initial_val : integer := e2_initial;
SIGNAL e3_initial_val : integer := e3_initial;
SIGNAL e0_high_val : integer := e0_high;
SIGNAL e1_high_val : integer := e1_high;
SIGNAL e2_high_val : integer := e2_high;
SIGNAL e3_high_val : integer := e3_high;
SIGNAL e0_low_val : integer := e0_low;
SIGNAL e1_low_val : integer := e1_low;
SIGNAL e2_low_val : integer := e2_low;
SIGNAL e3_low_val : integer := e3_low;
SIGNAL e0_time_delay_val : integer := e0_time_delay;
SIGNAL e1_time_delay_val : integer := e1_time_delay;
SIGNAL e2_time_delay_val : integer := e2_time_delay;
SIGNAL e3_time_delay_val : integer := e3_time_delay;
SIGNAL e0_mode_val : string(1 to 6) := "bypass";
SIGNAL e1_mode_val : string(1 to 6) := "bypass";
SIGNAL e2_mode_val : string(1 to 6) := "bypass";
SIGNAL e3_mode_val : string(1 to 6) := "bypass";

SIGNAL m_mode_val : string(1 to 6) := "bypass";
SIGNAL n_mode_val : string(1 to 6) := "bypass";


SIGNAL transfer : std_logic := '0';

SIGNAL scan_data : std_logic_vector(0 to 288) := (OTHERS => '0');
SIGNAL ena0, ena1, ena2, ena3, ena4, ena5 : std_logic;
SIGNAL extena0, extena1, extena2, extena3 : std_logic;

SIGNAL clk0_tmp, clk1_tmp, clk2_tmp : std_logic;
SIGNAL clk3_tmp, clk4_tmp, clk5_tmp : std_logic;
SIGNAL extclk0_tmp, extclk1_tmp, extclk2_tmp, extclk3_tmp : std_logic;

SIGNAL not_clk0_tmp, not_clk1_tmp, not_clk2_tmp : std_logic;
SIGNAL not_clk3_tmp, not_clk4_tmp, not_clk5_tmp : std_logic;

SIGNAL not_extclk0_tmp, not_extclk1_tmp, not_extclk2_tmp : std_logic;
SIGNAL not_extclk3_tmp : std_logic;

SIGNAL clkin : std_logic;
SIGNAL gate_locked : std_logic := '0';
SIGNAL lock : std_logic := '0';

SIGNAL inclk_l0, inclk_l1 : std_logic;
SIGNAL inclk_g0, inclk_g1 : std_logic;
SIGNAL inclk_g2, inclk_g3 : std_logic;
SIGNAL inclk_e0, inclk_e1 : std_logic;
SIGNAL inclk_e2, inclk_e3 : std_logic;
SIGNAL inclk_m : std_logic;
SIGNAL devpor, devclrn : std_logic;

SIGNAL inclk0_ipd, inclk1_ipd : std_logic;
SIGNAL ena_ipd, pfdena_ipd, comparator_ipd : std_logic;
SIGNAL areset_ipd, fbin_ipd : std_logic;
SIGNAL clkena0_ipd, clkena1_ipd : std_logic;
SIGNAL clkena2_ipd, clkena3_ipd : std_logic;
SIGNAL clkena4_ipd, clkena5_ipd : std_logic;
SIGNAL extclkena0_ipd, extclkena1_ipd : std_logic;
SIGNAL extclkena2_ipd, extclkena3_ipd : std_logic;
SIGNAL scanclk_ipd, scanaclr_ipd, scandata_ipd : std_logic;

SIGNAL lvds_dffa_clk, lvds_dffb_clk : std_logic;
SIGNAL lvds_dffc_clk, lvds_dffd_clk : std_logic;
SIGNAL dffa_out, dffb_out, dffc_out, dffd_out : std_logic;
SIGNAL nce_temp, nce_l0, nce_l1 : std_logic := '0';

SIGNAL first_schedule : boolean := true;
SIGNAL total_sched_time : time := 0 ps;

SIGNAL sig_offset, sig_refclk_last_rising_edge : time := 0 ps;
SIGNAL schedule_again : std_logic := '0';

SIGNAL m_delay, n_delay : integer := 0;
SIGNAL dummy_event : std_logic := '0';

component mn_cntr
   port ( clk : IN std_logic;
          reset : IN std_logic;
          cout : OUT std_logic;
          initial_value : IN integer := 1;
          modulus : IN integer;
          time_delay : IN integer;
          ph : IN integer := 0;
          fb_delay : IN integer
   );
end component;

component scale_cntr
   port (clk : IN std_logic;
         reset : IN std_logic;
         cout : OUT std_logic;
         initial : IN integer := 1;
         high : IN integer := 1;
         low : IN integer := 1;
         mode : IN string := "bypass";
         time_delay : IN integer := 0
   );
end component;

component DFFE
   port(
      Q                              :  out   STD_LOGIC := '0';
      D                              :  in    STD_LOGIC := '1';
      CLRN                           :  in    STD_LOGIC := '1';
      PRN                            :  in    STD_LOGIC := '1';
      CLK                            :  in    STD_LOGIC := '0';
      ENA                            :  in    STD_LOGIC := '1');
end component;

begin

    ----------------------
    --  INPUT PATH DELAYs
    ----------------------
  WireDelay : block
  begin
    inclk0_ipd <= inclk(0);
    inclk1_ipd <= inclk(1);
    areset_ipd <= areset;
    ena_ipd <= ena;
    fbin_ipd <= fbin;
    pfdena_ipd <= pfdena;
    clkena0_ipd <= clkena(0);
    clkena1_ipd <= clkena(1);
    clkena2_ipd <= clkena(2);
    clkena3_ipd <= clkena(3);
    clkena4_ipd <= clkena(4);
    clkena5_ipd <= clkena(5);
    extclkena0_ipd <= extclkena(0);
    extclkena1_ipd <= extclkena(1);
    extclkena2_ipd <= extclkena(2);
    extclkena3_ipd <= extclkena(3);
    scanclk_ipd <= scanclk;
    scanaclr_ipd <= scanaclr;
    scandata_ipd <= scandata;
    comparator_ipd <= comparator;
  end block;

-- User to Advanced parametr conversion

        -- i_vco_min       <= vco_min;     -- default
        -- i_vco_max       <= vco_max;     -- default
        -- i_vco_center    <= vco_center;  -- default
        -- i_pfd_min       <= pfd_min;     -- default
        -- i_pfd_max       <= pfd_max;     -- default
        i_l0_time_delay <= counter_time_delay(str2int(clk0_time_delay),
                                 m_time_delay,n_time_delay) when m = 0 
                                 else l0_time_delay;
        i_l1_time_delay <= counter_time_delay(str2int(clk1_time_delay), 
                                 m_time_delay, n_time_delay) 
                                 when m = 0 else l1_time_delay;
        i_g0_time_delay <= counter_time_delay(str2int(clk2_time_delay), 
				 m_time_delay, n_time_delay) when m = 0 
                                 else g0_time_delay;
        i_g1_time_delay <= counter_time_delay(str2int(clk3_time_delay), 
                                 m_time_delay, n_time_delay) when m = 0 
                                 else g1_time_delay;
        i_g2_time_delay <= counter_time_delay(str2int(clk4_time_delay), 
                                 m_time_delay, n_time_delay) when m = 0 
                                 else g2_time_delay;
        i_g3_time_delay <= counter_time_delay(str2int(clk5_time_delay), 
                                 m_time_delay, n_time_delay) when m = 0 
                                 else g3_time_delay;
        i_e0_time_delay <= counter_time_delay(str2int(extclk0_time_delay), 
                                 m_time_delay, n_time_delay) when m = 0 
                                 else e0_time_delay;
        i_e1_time_delay <= counter_time_delay(str2int(extclk1_time_delay), 
				 m_time_delay, n_time_delay) when m = 0 
                                 else e1_time_delay;
        i_e2_time_delay <= counter_time_delay(str2int(extclk2_time_delay), 
 			 	 m_time_delay, n_time_delay) when m = 0 
				 else e2_time_delay;
        i_e3_time_delay <= counter_time_delay(str2int(extclk3_time_delay), 
				 m_time_delay, n_time_delay) when m = 0 
                                 else e3_time_delay;
        i_m_time_delay   <= mintimedelay(str2int(clk0_time_delay), 
				str2int(clk1_time_delay),
                                str2int(clk2_time_delay), 
				str2int(clk3_time_delay), 
				str2int(clk4_time_delay),
                                str2int(clk5_time_delay), 
				str2int(extclk0_time_delay),
                                str2int(extclk1_time_delay), 
				str2int(extclk2_time_delay),
                                str2int(extclk3_time_delay)) 
				when m = 0 else m_time_delay;
        i_n_time_delay   <= maxtimedelay(str2int(clk0_time_delay), 
				str2int(clk1_time_delay),
                                str2int(clk2_time_delay), 
				str2int(clk3_time_delay), 
				str2int(clk4_time_delay),
                                str2int(clk5_time_delay), 
				str2int(extclk0_time_delay),
                                str2int(extclk1_time_delay), 
				str2int(extclk2_time_delay),
                                str2int(extclk3_time_delay)) 
				when m = 0 else n_time_delay;
        i_extclk3_counter       <= "e3" when m=0 else extclk3_counter;
        i_extclk2_counter       <= "e2" when m=0 else extclk2_counter;
        i_extclk1_counter       <= "e1" when m=0 else extclk1_counter;
        i_extclk0_counter       <= "e0" when m=0 else extclk0_counter;
        i_clk5_counter          <= "g3" when m=0 else clk5_counter;
        i_clk4_counter          <= "g2" when m=0 else clk4_counter;
        i_clk3_counter          <= "g1" when m=0 else clk3_counter;
        i_clk2_counter          <= "g0" when m=0 else clk2_counter;
        i_clk1_counter          <= "l1" when m=0 else clk1_counter;
        i_clk0_counter          <= "l0" when m=0 else clk0_counter;
        -- i_charge_pump_current   <= charge_pump_current; -- default
        -- i_loop_filter_r         <= loop_filter_r;       -- default

-- end parameter conversion

inclk_m <= fbin when operation_mode = "external_feedback" else vco_out(i_m_ph);
   m1 : mn_cntr
        port map (clk => inclk_m,
                 reset => areset_ipd,
                 cout => fbclk,
                 initial_value => m_initial_val,
                 modulus => m_val,
                 time_delay => m_time_delay_val,
                 ph => i_m_ph,
                 fb_delay => m_delay);

--   clkin <= inclk when pll_type = "HSSI" else inclk(0);
   clkin <= inclk0_ipd;

   n1 : mn_cntr
        port map (clk => clkin,
                  reset => areset_ipd,
                  cout => refclk,
                  initial_value => n_val,
                  modulus => n_val,
                  time_delay => n_time_delay_val,
                  fb_delay => n_delay);

   inclk_l0 <= vco_out(i_l0_ph);
   l0 : scale_cntr
        port map (clk => inclk_l0,
                  reset => areset_ipd,
                  cout => l0_clk,
                  initial => l0_initial_val,
                  high => l0_high_val,
                  low => l0_low_val,
                  mode => l0_mode_val,
                  time_delay => l0_time_delay_val);

   inclk_l1 <= vco_out(i_l1_ph);
   l1 : scale_cntr
        port map (clk => inclk_l1,
                  reset => areset_ipd,
                  cout => l1_clk,
                  initial => l1_initial_val,
                  high => l1_high_val,
                  low => l1_low_val,
                  mode => l1_mode_val,
                  time_delay => l1_time_delay_val);

   inclk_g0 <= vco_out(i_g0_ph);
   g0 : scale_cntr
        port map (clk => inclk_g0,
                  reset => areset_ipd,
                  cout => g0_clk,
                  initial => g0_initial_val,
                  high => g0_high_val,
                  low => g0_low_val,
                  mode => g0_mode_val,
                  time_delay => g0_time_delay_val);

   lvds_dffa : dffe
             port map(D => comparator_ipd,
                      CLRN => vcc,
                      PRN => vcc,
                      ENA => vcc,
                      CLK => g0_clk,
                      Q => dffa_out);

   lvds_dffb : dffe
             port map(D => dffa_out,
                      CLRN => vcc,
                      PRN => vcc,
                      ENA => vcc,
                      CLK => lvds_dffb_clk,
                      Q => dffb_out);

   lvds_dffb_clk <= l0_clk when enable0_counter = "l0" else
                    l1_clk when enable0_counter = "l1" else
                    '0';

   lvds_dffc : dffe
             port map(D => dffb_out,
                      CLRN => vcc,
                      PRN => vcc,
                      ENA => vcc,
                      CLK => lvds_dffc_clk,
                      Q => dffc_out);

   lvds_dffc_clk <= l0_clk when enable0_counter = "l0" else
                    l1_clk when enable0_counter = "l1" else
                    '0';

   nce_temp <= (not dffc_out) and dffb_out;

   lvds_dffd : dffe
             port map(D => nce_temp,
                      CLRN => vcc,
                      PRN => vcc,
                      ENA => vcc,
                      CLK => lvds_dffd_clk,
                      Q => dffd_out);

   lvds_dffd_clk <= l0_clk when enable0_counter = "l0" else
                    l1_clk when enable0_counter = "l1" else
                    '0';

   nce_l0 <= dffd_out when enable0_counter = "l0" else '0';
   nce_l1 <= dffd_out when enable0_counter = "l1" else '0';

   inclk_g1 <= vco_out(i_g1_ph);
   g1 : scale_cntr
        port map (clk => inclk_g1,
                  reset => areset_ipd,
                  cout => g1_clk,
                  initial => g1_initial_val,
                  high => g1_high_val,
                  low => g1_low_val,
                  mode => g1_mode_val,
                  time_delay => g1_time_delay_val);

   inclk_g2 <= vco_out(i_g2_ph);
   g2 : scale_cntr
        port map (clk => inclk_g2,
                  reset => areset_ipd,
                  cout => g2_clk,
                  initial => g2_initial_val,
                  high => g2_high_val,
                  low => g2_low_val,
                  mode => g2_mode_val,
                  time_delay => g2_time_delay_val);

   inclk_g3 <= vco_out(i_g3_ph);
   g3 : scale_cntr
        port map (clk => inclk_g3,
                  reset => areset_ipd,
                  cout => g3_clk,
                  initial => g3_initial_val,
                  high => g3_high_val,
                  low => g3_low_val,
                  mode => g3_mode_val,
                  time_delay => g3_time_delay_val);

   inclk_e0 <= vco_out(i_e0_ph);
   e0 : scale_cntr
        port map (clk => inclk_e0,
                  reset => areset_ipd,
                  cout => e0_clk,
                  initial => e0_initial_val,
                  high => e0_high_val,
                  low => e0_low_val,
                  mode => e0_mode_val,
                  time_delay => e0_time_delay_val);

   inclk_e1 <= vco_out(i_e1_ph);
   e1 : scale_cntr
        port map (clk => inclk_e1,
                  reset => areset_ipd,
                  cout => e1_clk,
                  initial => e1_initial_val,
                  high => e1_high_val,
                  low => e1_low_val,
                  mode => e1_mode_val,
                  time_delay => e1_time_delay_val);

   inclk_e2 <= vco_out(i_e2_ph);
   e2 : scale_cntr
        port map (clk => inclk_e2,
                  reset => areset_ipd,
                  cout => e2_clk,
                  initial => e2_initial_val,
                  high => e2_high_val,
                  low => e2_low_val,
                  mode => e2_mode_val,
                  time_delay => e2_time_delay_val);

   inclk_e3 <= vco_out(i_e3_ph);
   e3 : scale_cntr
        port map (clk => inclk_e3,
                  reset => areset_ipd,
                  cout => e3_clk,
                  initial => e3_initial_val,
                  high => e3_high_val,
                  low => e3_low_val,
                  mode => e3_mode_val,
                  time_delay => e3_time_delay_val);

   process(inclk_l0, inclk_l1)
   variable l0_got_first_rising_edge : boolean := false;
   variable l0_count : integer := 0;
   variable l0_tmp, l1_tmp : std_logic := '0';
   variable l1_got_first_rising_edge : boolean := false;
   variable l1_count : integer := 0;
   begin
      if (nce_l0 = '0') then
         if (not l0_got_first_rising_edge) then
            if (inclk_l0'event and inclk_l0 = '1') then
               l0_got_first_rising_edge := true;
            end if;
         elsif (inclk_l0'event) then
            l0_count := l0_count + 1;
            if (l0_count = (l0_high_val + l0_low_val) * 2) then
               l0_count := 1;
            end if;
         end if;
      end if;
      if (inclk_l0'event and inclk_l0 = '0') then
         if (l0_count = 1) then
            l0_tmp := '1';
            l0_got_first_rising_edge := false;
         else
            l0_tmp := '0';
         end if;
      end if;

      if (nce_l1 = '0') then
         if (not l1_got_first_rising_edge) then
            if (inclk_l1'event and inclk_l1 = '1') then
               l1_got_first_rising_edge := true;
            end if;
         elsif (inclk_l1'event) then
            l1_count := l1_count + 1;
            if (l1_count = (l1_high_val + l1_low_val) * 2) then
               l1_count := 1;
            end if;
         end if;
      end if;
      if (inclk_l1'event and inclk_l1 = '0') then
         if (l1_count = 1) then
            l1_tmp := '1';
            l1_got_first_rising_edge := false;
         else
            l1_tmp := '0';
         end if;
      end if;

      if (enable0_counter = "l0") then
          enable0 <= l0_tmp;
      elsif (enable0_counter = "l1") then
          enable0 <= l1_tmp;
      else
          enable0 <= '0';
      end if;

      if (enable1_counter = "l0") then
          enable1 <= l0_tmp;
      elsif (enable1_counter = "l1") then
          enable1 <= l1_tmp;
      else
          enable1 <= '0';
      end if;

   end process;

   glocked_cntr : process(inclk0_ipd, ena_ipd, areset_ipd)
   variable count : integer := 0;
   variable output : std_logic := '0';
   begin
      if (areset_ipd = '1') then
         count := 0;
         output := '0';
      elsif (inclk0_ipd'event and inclk0_ipd = '1') then
         if (ena_ipd = '1') then
            count := count + 1;
            if (count = gate_lock_counter) then
               output := '1';
            end if;
         end if;
      end if;
      gate_locked <= output;
   end process;

   locked <= gate_locked and lock when gate_lock_signal = "yes" else
             lock;

   process (transfer)
   variable init : boolean := true;
   variable low, high : std_logic_vector(0 to 8);
   variable delay_chain : std_logic_vector(0 to 3);
   variable mn_delay_chain : std_logic_vector(0 to 3);
   variable mode : string(1 to 6) := "bypass";
   variable egpp_offset, j : integer := 0;

-- user to advanced variables

   variable   i_m_initial    : natural;
   variable   i_m            : integer := 1;
   variable   i_n            : natural := 1;
   variable   i_m2           : natural;
   variable   i_n2           : natural;
   variable   i_ss           : natural;
   variable   i_l0_high      : natural;
   variable   i_l1_high      : natural;
   variable   i_g0_high      : natural;
   variable   i_g1_high      : natural;
   variable   i_g2_high      : natural;
   variable   i_g3_high      : natural;
   variable   i_e0_high      : natural;
   variable   i_e1_high      : natural;
   variable   i_e2_high      : natural;
   variable   i_e3_high      : natural;
   variable   i_l0_low       : natural;
   variable   i_l1_low       : natural;
   variable   i_g0_low       : natural;
   variable   i_g1_low       : natural;
   variable   i_g2_low       : natural;
   variable   i_g3_low       : natural;
   variable   i_e0_low       : natural;
   variable   i_e1_low       : natural;
   variable   i_e2_low       : natural;
   variable   i_e3_low       : natural;
   variable   i_l0_initial   : natural;
   variable   i_l1_initial   : natural;
   variable   i_g0_initial   : natural;
   variable   i_g1_initial   : natural;
   variable   i_g2_initial   : natural;
   variable   i_g3_initial   : natural;
   variable   i_e0_initial   : natural;
   variable   i_e1_initial   : natural;
   variable   i_e2_initial   : natural;
   variable   i_e3_initial   : natural;
   variable   i_l0_mode      : string(1 to 6);
   variable   i_l1_mode      : string(1 to 6);
   variable   i_g0_mode      : string(1 to 6);
   variable   i_g1_mode      : string(1 to 6);
   variable   i_g2_mode      : string(1 to 6);
   variable   i_g3_mode      : string(1 to 6);
   variable   i_e0_mode      : string(1 to 6);
   variable   i_e1_mode      : string(1 to 6);
   variable   i_e2_mode      : string(1 to 6);
   variable   i_e3_mode      : string(1 to 6);
   variable   max_neg_abs    : integer := 0;

   begin
      if (init) then
       if (m = 0) then  -- convert user parameters to advanced
        i_n := 1;
        i_m := lcm (clk0_multiply_by, clk1_multiply_by,
               clk2_multiply_by, clk3_multiply_by,
               clk4_multiply_by, clk5_multiply_by,
               extclk0_multiply_by,
               extclk1_multiply_by, extclk2_multiply_by,
               extclk3_multiply_by, inclk0_input_frequency);
	max_neg_abs := maxnegabs(str2int(clk0_phase_shift), 
                                 str2int(clk1_phase_shift),
                                 str2int(clk2_phase_shift),
                                 str2int(clk3_phase_shift),
                                 str2int(clk4_phase_shift),
                                 str2int(clk5_phase_shift),
                                 str2int(extclk0_phase_shift),
                                 str2int(extclk1_phase_shift),
                                 str2int(extclk2_phase_shift),
                                 str2int(extclk3_phase_shift));
        i_m_ph  <= counter_ph(max_neg_abs, i_m, i_n); 
        i_l0_ph <= counter_ph(ph_adjust((str2int(clk0_phase_shift)*360/inclk0_input_frequency),max_neg_abs), i_m, i_n);
        i_l1_ph <= counter_ph(ph_adjust((str2int(clk1_phase_shift)*360/inclk0_input_frequency),max_neg_abs), i_m, i_n);
        i_g0_ph <= counter_ph(ph_adjust((str2int(clk2_phase_shift)*360/inclk0_input_frequency),max_neg_abs), i_m, i_n);
        i_g1_ph <= counter_ph(ph_adjust((str2int(clk3_phase_shift)*360/inclk0_input_frequency),max_neg_abs), i_m, i_n);
        i_g2_ph <= counter_ph(ph_adjust((str2int(clk4_phase_shift)*360/inclk0_input_frequency),max_neg_abs), i_m, i_n);
        i_g3_ph <= counter_ph(ph_adjust((str2int(clk5_phase_shift)*360/inclk0_input_frequency),max_neg_abs), i_m, i_n);
        i_e0_ph <= counter_ph(ph_adjust((str2int(extclk0_phase_shift)*360/inclk0_input_frequency),max_neg_abs), i_m, i_n);
        i_e1_ph <= counter_ph(ph_adjust((str2int(extclk1_phase_shift)*360/inclk0_input_frequency),max_neg_abs), i_m, i_n);
        i_e2_ph <= counter_ph(ph_adjust((str2int(extclk2_phase_shift)*360/inclk0_input_frequency),max_neg_abs), i_m, i_n);
        i_e3_ph <= counter_ph(ph_adjust((str2int(extclk3_phase_shift)*360/inclk0_input_frequency),max_neg_abs), i_m, i_n);
        i_l0_high := counter_high(output_counter_value(clk0_divide_by,
                     clk0_multiply_by, i_m, i_n), clk0_duty_cycle);
        i_l1_high := counter_high(output_counter_value(clk1_divide_by,
                     clk1_multiply_by, i_m, i_n), clk1_duty_cycle);
        i_g0_high := counter_high(output_counter_value(clk2_divide_by,
                     clk2_multiply_by, i_m, i_n), clk2_duty_cycle);
        i_g1_high := counter_high(output_counter_value(clk3_divide_by,
                     clk3_multiply_by, i_m, i_n), clk3_duty_cycle);
        i_g2_high := counter_high(output_counter_value(clk4_divide_by,
                     clk4_multiply_by,  i_m, i_n), clk4_duty_cycle);
        i_g3_high := counter_high(output_counter_value(clk5_divide_by,
                     clk5_multiply_by,  i_m, i_n), clk5_duty_cycle);
        i_e0_high := counter_high(output_counter_value(extclk0_divide_by,
                     extclk0_multiply_by,  i_m, i_n), extclk0_duty_cycle);
        i_e1_high := counter_high(output_counter_value(extclk1_divide_by,
                           extclk1_multiply_by,  i_m, i_n), extclk1_duty_cycle);
        i_e2_high := counter_high(output_counter_value(extclk2_divide_by,
                           extclk2_multiply_by,  i_m, i_n), extclk2_duty_cycle);
        i_e3_high := counter_high(output_counter_value(extclk3_divide_by,
                          extclk3_multiply_by,  i_m, i_n), extclk3_duty_cycle);
        i_l0_low  := counter_low(output_counter_value(clk0_divide_by,
                               clk0_multiply_by,  i_m, i_n), clk0_duty_cycle);
        i_l1_low  := counter_low(output_counter_value(clk1_divide_by,
                               clk1_multiply_by,  i_m, i_n), clk1_duty_cycle);
        i_g0_low  := counter_low(output_counter_value(clk2_divide_by,
                               clk2_multiply_by,  i_m, i_n), clk2_duty_cycle);
        i_g1_low  := counter_low(output_counter_value(clk3_divide_by,
                               clk3_multiply_by,  i_m, i_n), clk3_duty_cycle);
        i_g2_low  := counter_low(output_counter_value(clk4_divide_by,
                               clk4_multiply_by,  i_m, i_n), clk4_duty_cycle);
        i_g3_low  := counter_low(output_counter_value(clk5_divide_by,
                               clk5_multiply_by,  i_m, i_n), clk5_duty_cycle);
        i_e0_low  := counter_low(output_counter_value(extclk0_divide_by,
                         extclk0_multiply_by,  i_m, i_n), extclk0_duty_cycle);
        i_e1_low  := counter_low(output_counter_value(extclk1_divide_by,
                         extclk1_multiply_by,  i_m, i_n), extclk1_duty_cycle);
        i_e2_low  := counter_low(output_counter_value(extclk2_divide_by,
                          extclk2_multiply_by,  i_m, i_n), extclk2_duty_cycle);
        i_e3_low  := counter_low(output_counter_value(extclk3_divide_by,
                          extclk3_multiply_by,  i_m, i_n), extclk3_duty_cycle);
        i_m_initial := counter_initial(max_neg_abs, i_m, i_n);
        i_l0_initial := counter_initial((str2int(clk0_phase_shift)*360/inclk0_input_frequency), i_m, i_n);
        i_l1_initial := counter_initial((str2int(clk1_phase_shift)*360/inclk0_input_frequency), i_m, i_n);
        i_g0_initial := counter_initial((str2int(clk2_phase_shift)*360/inclk0_input_frequency), i_m, i_n);
        i_g1_initial := counter_initial((str2int(clk3_phase_shift)*360/inclk0_input_frequency), i_m, i_n);
        i_g2_initial := counter_initial((str2int(clk4_phase_shift)*360/inclk0_input_frequency), i_m, i_n);
        i_g3_initial := counter_initial((str2int(clk5_phase_shift)*360/inclk0_input_frequency), i_m, i_n);
        i_e0_initial := counter_initial((str2int(extclk0_phase_shift)*360/inclk0_input_frequency), i_m, i_n);
        i_e1_initial := counter_initial((str2int(extclk1_phase_shift)*360/inclk0_input_frequency), i_m, i_n);
        i_e2_initial := counter_initial((str2int(extclk2_phase_shift)*360/inclk0_input_frequency), i_m, i_n);
        i_e3_initial := counter_initial((str2int(extclk3_phase_shift)*360/inclk0_input_frequency), i_m, i_n);
        i_l0_mode := counter_mode(clk0_duty_cycle, output_counter_value(clk0_divide_by, clk0_multiply_by,  i_m, i_n));
        i_l1_mode := counter_mode(clk1_duty_cycle, output_counter_value(clk1_divide_by, clk1_multiply_by,  i_m, i_n));
        i_g0_mode := counter_mode(clk2_duty_cycle, output_counter_value(clk2_divide_by, clk2_multiply_by,  i_m, i_n));
        i_g1_mode := counter_mode(clk3_duty_cycle, output_counter_value(clk3_divide_by, clk3_multiply_by,  i_m, i_n));
        i_g2_mode := counter_mode(clk4_duty_cycle, output_counter_value(clk4_divide_by, clk4_multiply_by,  i_m, i_n));
        i_g3_mode := counter_mode(clk5_duty_cycle, output_counter_value(clk5_divide_by, clk5_multiply_by,  i_m, i_n));
        i_e0_mode := counter_mode(extclk0_duty_cycle, output_counter_value(extclk0_divide_by, extclk0_multiply_by,  i_m, i_n));
        i_e1_mode := counter_mode(extclk1_duty_cycle, output_counter_value(extclk1_divide_by, extclk1_multiply_by,  i_m, i_n));
        i_e2_mode := counter_mode(extclk2_duty_cycle, output_counter_value(extclk2_divide_by, extclk2_multiply_by,  i_m, i_n));
        i_e3_mode := counter_mode(extclk3_duty_cycle, output_counter_value(extclk3_divide_by, extclk3_multiply_by,  i_m, i_n));
 
     else -- m \= 0

        i_n := n;
        i_m := m;
	i_m_initial := m_initial;
	i_m_ph <= m_ph;
        i_l0_ph <= l0_ph;
        i_l1_ph <= l1_ph;
        i_g0_ph <= g0_ph;
        i_g1_ph <= g1_ph;
        i_g2_ph <= g2_ph;
        i_g3_ph <= g3_ph;
        i_e0_ph <= e0_ph;
        i_e1_ph <= e1_ph;
        i_e2_ph <= e2_ph;
        i_e3_ph <= e3_ph;
        i_l0_high := l0_high;
        i_l1_high := l1_high;
        i_g0_high := g0_high;
        i_g1_high := g1_high;
        i_g2_high := g2_high;
        i_g3_high := g3_high;
        i_e0_high := e0_high;
        i_e1_high := e1_high;
        i_e2_high := e2_high;
        i_e3_high := e3_high;
        i_l0_low  := l0_low;
        i_l1_low  := l1_low;
        i_g0_low  := g0_low;
        i_g1_low  := g1_low;
        i_g2_low  := g2_low;
        i_g3_low  := g3_low;
        i_e0_low  := e0_low;
        i_e1_low  := e1_low;
        i_e2_low  := e2_low;
        i_e3_low  := e3_low;
        i_l0_initial := l0_initial;
        i_l1_initial := l1_initial;
        i_g0_initial := g0_initial;
        i_g1_initial := g1_initial;
        i_g2_initial := g2_initial;
        i_g3_initial := g3_initial;
        i_e0_initial := e0_initial;
        i_e1_initial := e1_initial;
        i_e2_initial := e2_initial;
        i_e3_initial := e3_initial;
        i_l0_mode := translate_string(l0_mode);
        i_l1_mode := translate_string(l1_mode);
        i_g0_mode := translate_string(g0_mode);
        i_g1_mode := translate_string(g1_mode);
        i_g2_mode := translate_string(g2_mode);
        i_g3_mode := translate_string(g3_mode);
        i_e0_mode := translate_string(e0_mode);
        i_e1_mode := translate_string(e1_mode);
        i_e2_mode := translate_string(e2_mode);
        i_e3_mode := translate_string(e3_mode);

     end if; -- user to advanced conversion.

          m_initial_val <= i_m_initial;
          n_val <= i_n;
          m_val <= i_m;

          m_time_delay_val <= i_m_time_delay;
          n_time_delay_val <= i_n_time_delay;

         l0_initial_val <= i_l0_initial;
         l0_high_val <= i_l0_high;
         l0_low_val <= i_l0_low;
         l0_mode_val <= i_l0_mode;
         l0_time_delay_val <= i_l0_time_delay;

         l1_initial_val <= i_l1_initial;
         l1_high_val <= i_l1_high;
         l1_low_val <= i_l1_low;
         l1_mode_val <= i_l1_mode;
         l1_time_delay_val <= i_l1_time_delay;

         g0_initial_val <= i_g0_initial;
         g0_high_val <= i_g0_high;
         g0_low_val <= i_g0_low;
         g0_mode_val <= i_g0_mode;
         g0_time_delay_val <= i_g0_time_delay;

         g1_initial_val <= i_g1_initial;
         g1_high_val <= i_g1_high;
         g1_low_val <= i_g1_low;
         g1_mode_val <= i_g1_mode;
         g1_time_delay_val <= i_g1_time_delay;

         g2_initial_val <= i_g2_initial;
         g2_high_val <= i_g2_high;
         g2_low_val <= i_g2_low;
         g2_mode_val <= i_g2_mode;
         g2_time_delay_val <= i_g2_time_delay;

         g3_initial_val <= i_g3_initial;
         g3_high_val <= i_g3_high;
         g3_low_val <= i_g3_low;
         g3_mode_val <= i_g3_mode;
         g3_time_delay_val <= i_g3_time_delay;

         if (scan_chain = "long") then
            e0_initial_val <= i_e0_initial;
            e0_high_val <= i_e0_high;
            e0_low_val <= i_e0_low;
            e0_mode_val <= i_e0_mode;
            e0_time_delay_val <= i_e0_time_delay;

            e1_initial_val <= i_e1_initial;
            e1_high_val <= i_e1_high;
            e1_low_val <= i_e1_low;
            e1_mode_val <= i_e1_mode;
            e1_time_delay_val <= i_e1_time_delay;

            e2_initial_val <= i_e2_initial;
            e2_high_val <= i_e2_high;
            e2_low_val <= i_e2_low;
            e2_mode_val <= i_e2_mode;
            e2_time_delay_val <= i_e2_time_delay;

            e3_initial_val <= i_e3_initial;
            e3_high_val <= i_e3_high;
            e3_low_val <= i_e3_low;
            e3_mode_val <= i_e3_mode;
            e3_time_delay_val <= i_e3_time_delay;
         end if;
         init := false;
      elsif (transfer'event and transfer = '1') then
         if (scan_chain = "long") then
            -- cntr e3
            delay_chain := scan_data(0 to 3);
            if (scan_data(14) = '1') then
               e3_mode_val <= "bypass";
            elsif (scan_data(4) = '1') then
               e3_mode_val <= "   odd";
            else 
               e3_mode_val <= "  even";
            end if;
            j := 8;
            for i in 0 to 8 loop
               low(j) := scan_data(i+5);
               high(j) := scan_data(i+15);
               j := j - 1;
            end loop;
            e3_low_val <= alt_conv_integer(low);
            e3_high_val <= alt_conv_integer(high);
            e3_time_delay_val <= alt_conv_integer(delay_chain);
            -- cntr e2
            delay_chain := scan_data(24 to 27);
            if (scan_data(38) = '1') then
               e2_mode_val <= "bypass";
            elsif (scan_data(28) = '1') then
               e2_mode_val <= "   odd";
            else 
               e2_mode_val <= "  even";
            end if;
            j := 8;
            for i in 0 to 8 loop
               low(j) := scan_data(i+29);
               high(j) := scan_data(i+39);
               j := j - 1;
            end loop;
            e2_low_val <= alt_conv_integer(low);
            e2_high_val <= alt_conv_integer(high);
            e2_time_delay_val <= alt_conv_integer(delay_chain);
            -- cntr e1
            delay_chain := scan_data(48 to 51);
            if (scan_data(62) = '1') then
               e1_mode_val <= "bypass";
            elsif (scan_data(52) = '1') then
               e1_mode_val <= "   odd";
            else 
               e1_mode_val <= "  even";
            end if;
            j := 8;
            for i in 0 to 8 loop
               low(j) := scan_data(i+53);
               high(j) := scan_data(i+63);
               j := j - 1;
            end loop;
            e1_low_val <= alt_conv_integer(low);
            e1_high_val <= alt_conv_integer(high);
            e1_time_delay_val <= alt_conv_integer(delay_chain);
            -- cntr e0
            delay_chain := scan_data(72 to 75);
            if (scan_data(86) = '1') then
               e0_mode_val <= "bypass";
            elsif (scan_data(76) = '1') then
               e0_mode_val <= "   odd";
            else 
               e0_mode_val <= "  even";
            end if;
            j := 8;
            for i in 0 to 8 loop
               low(j) := scan_data(i+77);
               high(j) := scan_data(i+87);
               j := j - 1;
            end loop;
            e0_low_val <= alt_conv_integer(low);
            e0_high_val <= alt_conv_integer(high);
            e0_time_delay_val <= alt_conv_integer(delay_chain);

            egpp_offset := 96;
         elsif (scan_chain = "short") then
            egpp_offset := 0;
         end if;
            -- cntr l1
            delay_chain := scan_data(egpp_offset to (egpp_offset+3));
            if (scan_data(egpp_offset+14) = '1') then
               l1_mode_val <= "bypass";
            elsif (scan_data(egpp_offset+4) = '1') then
               l1_mode_val <= "   odd";
            else 
               l1_mode_val <= "  even";
            end if;
            j := 8;
            for i in 0 to 8 loop
               low(j) := scan_data(i+egpp_offset+5);
               high(j) := scan_data(i+egpp_offset+15);
               j := j - 1;
            end loop;
            l1_low_val <= alt_conv_integer(low);
            l1_high_val <= alt_conv_integer(high);
            l1_time_delay_val <= alt_conv_integer(delay_chain);

            -- cntr l0
            delay_chain := scan_data((egpp_offset+24) to (egpp_offset+27));
            if (scan_data(egpp_offset+38) = '1') then
               l0_mode_val <= "bypass";
            elsif (scan_data(egpp_offset+28) = '1') then
               l0_mode_val <= "   odd";
            else 
               l0_mode_val <= "  even";
            end if;
            j := 8;
            for i in 0 to 8 loop
               low(j) := scan_data(i+egpp_offset+29);
               high(j) := scan_data(i+egpp_offset+39);
               j := j - 1;
            end loop;
            l0_low_val <= alt_conv_integer(low);
            l0_high_val <= alt_conv_integer(high);
            l0_time_delay_val <= alt_conv_integer(delay_chain);

            -- cntr g3
            delay_chain := scan_data((egpp_offset+48) to (egpp_offset+51));
            if (scan_data(egpp_offset+62) = '1') then
               g3_mode_val <= "bypass";
            elsif (scan_data(egpp_offset+52) = '1') then
               g3_mode_val <= "   odd";
            else 
               g3_mode_val <= "  even";
            end if;
            j := 8;
            for i in 0 to 8 loop
               low(j) := scan_data(i+egpp_offset+53);
               high(j) := scan_data(i+egpp_offset+63);
               j := j - 1;
            end loop;
            g3_low_val <= alt_conv_integer(low);
            g3_high_val <= alt_conv_integer(high);
            g3_time_delay_val <= alt_conv_integer(delay_chain);

            -- cntr g2
            delay_chain := scan_data((egpp_offset+72) to (egpp_offset+75));
            if (scan_data(egpp_offset+86) = '1') then
               g2_mode_val <= "bypass";
            elsif (scan_data(egpp_offset+76) = '1') then
               g2_mode_val <= "   odd";
            else 
               g2_mode_val <= "  even";
            end if;
            j := 8;
            for i in 0 to 8 loop
               low(j) := scan_data(i+egpp_offset+77);
               high(j) := scan_data(i+egpp_offset+87);
               j := j - 1;
            end loop;
            g2_low_val <= alt_conv_integer(low);
            g2_high_val <= alt_conv_integer(high);
            g2_time_delay_val <= alt_conv_integer(delay_chain);

            -- cntr g1
            delay_chain := scan_data((egpp_offset+96) to (egpp_offset+99));
            if (scan_data(egpp_offset+110) = '1') then
               g1_mode_val <= "bypass";
            elsif (scan_data(egpp_offset+100) = '1') then
               g1_mode_val <= "   odd";
            else 
               g1_mode_val <= "  even";
            end if;
            j := 8;
            for i in 0 to 8 loop
               low(j) := scan_data(i+egpp_offset+101);
               high(j) := scan_data(i+egpp_offset+111);
               j := j - 1;
            end loop;
            g1_low_val <= alt_conv_integer(low);
            g1_high_val <= alt_conv_integer(high);
            g1_time_delay_val <= alt_conv_integer(delay_chain);

            -- cntr g0
            delay_chain := scan_data((egpp_offset+120) to (egpp_offset+123));
            if (scan_data(egpp_offset+134) = '1') then
               g0_mode_val <= "bypass";
            elsif (scan_data(egpp_offset+124) = '1') then
               g0_mode_val <= "   odd";
            else 
               g0_mode_val <= "  even";
            end if;
            j := 8;
            for i in 0 to 8 loop
               low(j) := scan_data(i+egpp_offset+125);
               high(j) := scan_data(i+egpp_offset+135);
               j := j - 1;
            end loop;
            g0_low_val <= alt_conv_integer(low);
            g0_high_val <= alt_conv_integer(high);
            g0_time_delay_val <= alt_conv_integer(delay_chain);

            -- cntr M
            j := 3;
            for i in 0 to 3 loop
               delay_chain(i) := scan_data(j+egpp_offset+144);
               j := j - 1;
            end loop;
            if (scan_data(egpp_offset+158) = '1') then
               m_mode_val <= "bypass";
            end if;
            -- 'low' contains modulus for m_cntr(spread_spectrum disabled)
            j := 8;
            for i in 0 to 8 loop
               low(j) := scan_data(i+egpp_offset+159);
--               high(j) := scan_data(i+egpp_offset+167);
               j := j - 1;
            end loop;
--            low := scan_data((egpp_offset+159) to (egpp_offset+167));
            m_val <= alt_conv_integer(low);
            m_time_delay_val <= alt_conv_integer(delay_chain);

            -- cntr N
            j := 3;
            for i in 0 to 3 loop
               delay_chain(i) := scan_data(j+egpp_offset+168);
               j := j - 1;
            end loop;
            if (scan_data(egpp_offset+182) = '1') then
               n_mode_val <= "bypass";
            end if;
            -- 'low' contains modulus for n_cntr(spread_spectrum disabled)
            j := 8;
            for i in 0 to 8 loop
               low(j) := scan_data(i+egpp_offset+183);
               j := j - 1;
            end loop;
            n_val <= alt_conv_integer(low);
            n_time_delay_val <= alt_conv_integer(delay_chain);

            -- reset array to all 0's
            for i in scan_data'range loop
                scan_data(i) <= '0';
            end loop;
            transfer <= '0';
      end if;
   end process;

   process (schedule_again)
   begin
      if (now > 0 ns) then
         dummy_event <= '1';
      end if;
   end process;

-- add delta delays to fbclk path to detect condition when refclk and fbclk
-- are in phase
fbclk_tmp <= fbclk;
fbclk_tmp1 <= fbclk_tmp;
fbclk_tmp2 <= fbclk_tmp1;
fbclk_tmp3 <= fbclk_tmp2;
fbclk_tmp4 <= fbclk_tmp3;
fbclk_tmp5 <= fbclk_tmp4;

   process(refclk, fbclk_tmp5, ena_ipd, dummy_event, schedule_again)
   variable got_refclk_posedge : boolean := false;
   variable got_fbclk_posedge : boolean := false;
   variable got_refclk_rising_edge : boolean := false;
   variable got_fbclk_rising_edge : boolean := false;

   variable refclk_got_first_rising_edge : boolean := false;
   variable fbclk_got_first_rising_edge : boolean := false;
   variable fbclk_got_second_rising_edge : boolean := false;

--   variable fbclk_period : time := m_times_vco_period;
   variable fbclk_period : time;
   variable refclk_period : time := (inclk0_input_frequency * 1 ps) * n;

   variable refclk_last_rising_edge : time := 0 ps;
   variable fbclk_last_rising_edge : time := 0 ps;
   variable refclk_rising_edge : time := 0 ps;
   variable fbclk_rising_edge : time := 0 ps;

   variable cycles_to_lock : integer := 0;
   variable cycles_to_unlock : integer := 0;
   variable pll_lock : std_logic := '0';

   variable offset, temp_offset : time := 0 ps;
   variable reset_vco : boolean := false;
   variable fbclk_is_in_phase_with_refclk: boolean := false;

   begin
      if (ena_ipd'event and ena_ipd = '1') then
         schedule_vco <= true;
         first_schedule <= true;
      elsif (ena_ipd = '0') then
         pll_lock := '0';
         cycles_to_lock := 0;
      else
      schedule_vco <= false;
      schedule_zero <= false;
      schedule_from_refclk <= false;
      reschedule_from_fbclk <= false;
      first_schedule <= false;
      if (schedule_again = '1' and dummy_event = '1') then
         schedule_again <= '0';
         m_times_vco_period <= refclk_period;
         offset := temp_offset;
         schedule_vco <= true;
      end if;
--      if (ena_ipd'event and ena_ipd = '1') then
--         schedule_vco <= true;
--      elsif (ena_ipd = '0') then
--         pll_lock := '0';
--         cycles_to_lock := 0;
--      else
         if (refclk'event and refclk = '1') then
            got_refclk_posedge := true;
            got_refclk_rising_edge := true;
            if (not refclk_got_first_rising_edge) then
               refclk_got_first_rising_edge := true;
               if (refclk_last_rising_edge /= 0 ps) then
                  refclk_period := now - refclk_last_rising_edge;
               end if;
               refclk_last_rising_edge := now;
            else
               refclk_period := now - refclk_last_rising_edge;
            end if;
            refclk_rising_edge := now;
         else
            got_refclk_posedge := false;
         end if;

--         if (fbclk_tmp1'event and fbclk_tmp1 = '1') then
         if (fbclk_tmp5'event and fbclk_tmp5 = '1') then
            got_fbclk_posedge := true;
            got_fbclk_rising_edge := true;
            if (not fbclk_got_first_rising_edge) then
               fbclk_got_first_rising_edge := true;
               first_schedule <= false;
               if (fbclk_last_rising_edge /= 0 ps) then
                  fbclk_period := now - fbclk_last_rising_edge;
               end if;
               fbclk_last_rising_edge := now;
            else
               fbclk_got_second_rising_edge := true;
               fbclk_period := now - fbclk_last_rising_edge;
            end if;
            fbclk_rising_edge := now;
         else
            got_fbclk_posedge := false;
         end if;

         if (pfdena_ipd = '1') then 
            if (got_refclk_posedge and got_fbclk_posedge) then
--               if (not fbclk_is_in_phase_with_refclk) then
--                  fbclk_is_in_phase_with_refclk := true;
--                  reset_vco := true;
--               end if;
--               if (cycles_to_lock = valid_lock_multiplier) then
--                  pll_lock := '1';
--               end if;
--               cycles_to_lock := cycles_to_lock + 1;
--
--               if (simulation_type = "timing") then
--                  if (reset_vco) then
--                     offset := refclk_period - (pll_compensation_delay * 1 ps);
--                     while (offset < 0 ps) loop
--                        offset := offset + refclk_period;
--                     end loop;
--                     temp_offset := offset;
--                     offset := 0 ps;
--                     m_times_vco_period <= temp_offset;
--                     m_delay <= pll_compensation_delay;
--                     schedule_vco <= true;
--                     schedule_again <= true;
--                  else
--                     schedule_vco <= true;
--                  end if;
--               elsif (simulation_type = "functional") then
--                  m_times_vco_period <= refclk_period;
--                  schedule_vco <= true;
--               end if;
--               m_times_vco_period <= refclk_period;
--               refclk_got_first_rising_edge := false;
--               fbclk_got_first_rising_edge := false;
--               got_refclk_rising_edge := false;
--               got_fbclk_rising_edge := false;

            elsif (got_fbclk_posedge) then
               if (got_refclk_rising_edge) then
                  got_refclk_rising_edge := false;
                  got_fbclk_rising_edge := false;
                  if (now = refclk_last_rising_edge) then
                     if (not fbclk_is_in_phase_with_refclk) then
                        fbclk_is_in_phase_with_refclk := true;
                        reset_vco := true;
                     end if;
                     if (cycles_to_lock = valid_lock_multiplier) then
                        pll_lock := '1';
                     end if;
                     cycles_to_lock := cycles_to_lock + 1;

                     if (simulation_type = "timing") then
                        if (reset_vco) then
                           offset := refclk_period - (pll_compensation_delay * 1 ps);
                           while (offset < 0 ps) loop
                              offset := offset + refclk_period;
                           end loop;
                           temp_offset := offset;
                           offset := 0 ps;
                           m_times_vco_period <= temp_offset;
                           m_delay <= pll_compensation_delay;
                           schedule_vco <= true;
                           schedule_again <= '1';
                           reset_vco := false;
                        else
                           schedule_vco <= true;
                        end if;
                     elsif (simulation_type = "functional") then
                        m_times_vco_period <= refclk_period;
                        schedule_vco <= true;
                     end if;
--                     m_times_vco_period <= refclk_period;
                     refclk_got_first_rising_edge := false;
                     fbclk_got_first_rising_edge := false;
                  elsif (refclk_period = fbclk_period) then
                     fbclk_is_in_phase_with_refclk := false;
                     offset := now - refclk_last_rising_edge;
                     m_times_vco_period <= m_times_vco_period - offset;
                     offset := 0 ps;
                     cycles_to_lock := 0;
                     schedule_vco <= true;
                  else
                     fbclk_is_in_phase_with_refclk := false;
                     m_times_vco_period <= refclk_period;
                     offset := 0 ps;
                     if (pll_lock = '1') then
                        cycles_to_unlock := cycles_to_unlock + 1;
                        if (cycles_to_unlock = invalid_lock_multiplier) then
                           pll_lock := '0';
                        end if;
                        cycles_to_lock := 0;
                     end if;
                     schedule_vco <= true;
                  end if;
               elsif ((now - refclk_last_rising_edge) > 5 * refclk_period) then
                  -- do not schedule, so VCO stops
                  got_fbclk_rising_edge := false;
                  fbclk_got_first_rising_edge := false;
                  first_schedule <= true;
                  pll_lock := '0';
                  -- schedule '0', so VCO o/p drops
                  schedule_zero <= true;
               elsif (not refclk_got_first_rising_edge and fbclk_got_second_rising_edge) then
--               elsif (not refclk_got_first_rising_edge) then
--                  reschedule_from_fbclk <= true;
                  if (ena_ipd = '1') then
                     offset := 0 ps;
                     schedule_vco <= true;
                  end if;
               end if;
            elsif (got_refclk_posedge) then
               if (got_fbclk_rising_edge) then
                  got_refclk_rising_edge := false;
                  got_fbclk_rising_edge := false;
                  if (now /= total_sched_time) then
                     m_times_vco_period <= now - fbclk_last_rising_edge;
                     if (ena_ipd = '1') then
                        schedule_vco <= true;
                        schedule_from_refclk <= true;
                     end if;
                  else
                     got_refclk_rising_edge := true;
                  end if;
               elsif (not fbclk_got_first_rising_edge and first_schedule) then
                  -- start VCO
                  if (ena_ipd = '1') then
                     schedule_vco <= true;
                  end if;
               end if;
            end if;
         elsif (pfdena_ipd = '0') then -- keep VCO running at vco_period
            if (got_fbclk_posedge) then
               schedule_vco <= true;
            end if;
         end if;
      end if;
      refclk_last_rising_edge := refclk_rising_edge;
      fbclk_last_rising_edge := fbclk_rising_edge;
     lock <= pll_lock;
--   locked <= pll_lock;
     sig_offset <= offset;
     sig_refclk_last_rising_edge <= refclk_last_rising_edge;
   end process;

   process (schedule_vco, schedule_zero)
   variable vco_val, vco_val_last_value : std_logic := '0';
   variable vco_per : time;
   variable high_time : time;
   variable low_time : time;
   variable sched_time : time;
   variable tmp_vco_per : integer;
   variable temp, tmp_rem, my_rem : integer;
   begin
      if (schedule_vco and ena_ipd = '1') then
         if (schedule_from_refclk) then
            sched_time := total_sched_time - sig_refclk_last_rising_edge;
         else
            sched_time := sig_offset;
         end if;
         vco_val := vco_val_last_value;
         temp := m_times_vco_period/1 ps;
         my_rem := temp rem m_val;
         tmp_rem := my_rem;
         for i in 1 to m_val loop
            tmp_vco_per := temp/m_val;            -- integer value
            if (my_rem /= 0) then
               if (m_val rem 2 = 0) then   -- m is even
                  if (my_rem <= m_val/2) then
                     if (i rem 2 = 1 and tmp_rem > 0) then 
                        -- add 1 ps to odd cycles
                        tmp_vco_per := tmp_vco_per + 1;
                        tmp_rem := tmp_rem - 1;
                     end if;
                  elsif (tmp_rem > 0) then
                     -- add 1 ps to every cycle till tmp_rem = 0
                     tmp_vco_per := tmp_vco_per + 1;
                     tmp_rem := tmp_rem - 1;
                  end if;
               else  -- m is odd
                  if (my_rem <= m_val/2+1) then
                     -- add 1 ps to odd cycles
                     if (i rem 2 = 1 and tmp_rem > 0) then
                        tmp_vco_per := tmp_vco_per + 1;
                        tmp_rem := tmp_rem - 1;
                     end if;
                  elsif (tmp_rem > 0) then
                     -- add 1 ps to every cycle till tmp_rem = 0
                     tmp_vco_per := tmp_vco_per + 1;
                     tmp_rem := tmp_rem - 1;
                  end if;
               end if;
            end if;
            vco_per := tmp_vco_per * 1 ps;
            high_time := (tmp_vco_per/2) * 1 ps;
            if (tmp_vco_per rem 2 /= 0) then
               high_time := high_time + 1 ps;
            end if;
            low_time := vco_per - high_time;
            for j in 1 to 2 loop
               vco_val := not vco_val;
               if (vco_val = '0') then
                  sched_time := sched_time + high_time;
               elsif (vco_val = '1') then
                  sched_time := sched_time + low_time;
               end if;
               -- schedule the phase taps
               vco_out(0) <= transport vco_val after sched_time;
               vco_out(1) <= transport vco_val after (sched_time + vco_per/8);
               vco_out(2) <= transport vco_val after (sched_time + vco_per/4);
               vco_out(3) <= transport vco_val after (sched_time + 3*vco_per/8);
               vco_out(4) <= transport not vco_val after sched_time;
               vco_out(5) <= transport vco_val after (sched_time + 5*vco_per/8);
               vco_out(6) <= transport vco_val after (sched_time + 3*vco_per/4);
               vco_out(7) <= transport vco_val after (sched_time + 7*vco_per/8);
            end loop;
         end loop;
         -- schedule once more
         if (first_schedule) then
            vco_val := not vco_val;
            if (vco_val = '0') then
               sched_time := sched_time + high_time;
            elsif (vco_val = '1') then
               sched_time := sched_time + low_time;
            end if;
            tmp_vco_out <= transport vco_val after sched_time;
            -- schedule the phase taps
            vco_out(0) <= transport vco_val after sched_time;
            vco_out(1) <= transport vco_val after (sched_time + vco_per/8);
            vco_out(2) <= transport vco_val after (sched_time + vco_per/4);
            vco_out(3) <= transport vco_val after (sched_time + 3*vco_per/8);
            vco_out(4) <= transport not vco_val after sched_time;
            vco_out(5) <= transport vco_val after (sched_time + 5*vco_per/8);
            vco_out(6) <= transport vco_val after (sched_time + 3*vco_per/4);
            vco_out(7) <= transport vco_val after (sched_time + 7*vco_per/8);
         end if;

         vco_val_last_value := vco_val;
         if (not reschedule_from_fbclk) then
            if (first_schedule) then
               total_sched_time <= now + sched_time;
            else
               total_sched_time <= total_sched_time + m_times_vco_period;
            end if;
         end if;
      elsif (schedule_zero) then
            vco_out(0) <= transport '0' after high_time;
            vco_out(1) <= '0';
            vco_out(2) <= '0';
            vco_out(3) <= '0';
            vco_out(4) <= '0';
            vco_out(5) <= '0';
            vco_out(6) <= '0';
            vco_out(7) <= '0';
      end if;
   end process;

   process (scanclk, scanaclr)
      variable j : integer := 0;
      variable transfer_enable : boolean := false;
   begin
      if (scanaclr = '1') then
         for i in scan_data'range loop
            scan_data(i) <= '0';
         end loop;
      elsif (scanclk'event and scanclk = '1') then
         if (j = 0 and (not transfer_enable)) then
            if (scandata = '1') then
               transfer_enable := true;
            end if;
         elsif (scan_chain = "long") then
            if (j < scan_data'high) then
               scan_data(j) <= scandata;
               j := j + 1;
            else
               scan_data(j) <= scandata;
               -- this was last bit : generate transfer signal
               transfer <= '1';
               j := 0;
               transfer_enable := false;
            end if;
         elsif (scan_chain = "short") then
            if (j < max_gpp_bits) then
               scan_data(j) <= scandata;
               j := j + 1;
            else
               scan_data(j) <= scandata;
               -- this was last bit : generate transfer signal
               transfer <= '1';
               j := 0;
               transfer_enable := false;
            end if;
         end if;
      end if;
   end process;

   clk0_tmp <= l0_clk when i_clk0_counter = "l0" else
               l1_clk when i_clk0_counter = "l1" else
               g0_clk when i_clk0_counter = "g0" else
               g1_clk when i_clk0_counter = "g1" else
               g2_clk when i_clk0_counter = "g2" else
               g3_clk when i_clk0_counter = "g3" else
               '0';
   not_clk0_tmp <= not clk0_tmp;
   ena0_reg : dffe
             port map(D => clkena(0),
                      CLRN => vcc,
                      PRN => vcc,
                      ENA => vcc,
                      CLK => not_clk0_tmp,
                      Q => ena0);
   clk(0) <= ena0 and clk0_tmp;

   clk1_tmp <= l0_clk when i_clk1_counter = "l0" else
               l1_clk when i_clk1_counter = "l1" else
               g0_clk when i_clk1_counter = "g0" else
               g1_clk when i_clk1_counter = "g1" else
               g2_clk when i_clk1_counter = "g2" else
               g3_clk when i_clk1_counter = "g3" else
               '0';
   not_clk1_tmp <= not clk1_tmp;
   ena1_reg : dffe
             port map(D => clkena(1),
                      CLRN => vcc,
                      PRN => vcc,
                      ENA => vcc,
                      CLK => not_clk1_tmp,
                      Q => ena1);
   clk(1) <= ena1 and clk1_tmp;

   clk2_tmp <= l0_clk when i_clk2_counter = "l0" else
               l1_clk when i_clk2_counter = "l1" else
               g0_clk when i_clk2_counter = "g0" else
               g1_clk when i_clk2_counter = "g1" else
               g2_clk when i_clk2_counter = "g2" else
               g3_clk when i_clk2_counter = "g3" else
               '0';
   not_clk2_tmp <= not clk2_tmp;
   ena2_reg : dffe
             port map(D => clkena(2),
                      CLRN => vcc,
                      PRN => vcc,
                      ENA => vcc,
                      CLK => not_clk2_tmp,
                      Q => ena2);
   clk(2) <= ena2 and clk2_tmp;

   clk3_tmp <= l0_clk when i_clk3_counter = "l0" else
               l1_clk when i_clk3_counter = "l1" else
               g0_clk when i_clk3_counter = "g0" else
               g1_clk when i_clk3_counter = "g1" else
               g2_clk when i_clk3_counter = "g2" else
               g3_clk when i_clk3_counter = "g3" else
               '0';
   not_clk3_tmp <= not clk3_tmp;
   ena3_reg : dffe
             port map(D => clkena(3),
                      CLRN => vcc,
                      PRN => vcc,
                      ENA => vcc,
                      CLK => not_clk3_tmp,
                      Q => ena3);
   clk(3) <= ena3 and clk3_tmp;

   clk4_tmp <= l0_clk when i_clk4_counter = "l0" else
               l1_clk when i_clk4_counter = "l1" else
               g0_clk when i_clk4_counter = "g0" else
               g1_clk when i_clk4_counter = "g1" else
               g2_clk when i_clk4_counter = "g2" else
               g3_clk when i_clk4_counter = "g3" else
               '0';
   not_clk4_tmp <= not clk4_tmp;
   ena4_reg : dffe
             port map(D => clkena(4),
                      CLRN => vcc,
                      PRN => vcc,
                      ENA => vcc,
                      CLK => not_clk4_tmp,
                      Q => ena4);
   clk(4) <= ena4 and clk4_tmp;

   clk5_tmp <= l0_clk when i_clk5_counter = "l0" else
               l1_clk when i_clk5_counter = "l1" else
               g0_clk when i_clk5_counter = "g0" else
               g1_clk when i_clk5_counter = "g1" else
               g2_clk when i_clk5_counter = "g2" else
               g3_clk when i_clk5_counter = "g3" else
               '0';
   not_clk5_tmp <= not clk5_tmp;
   ena5_reg : dffe
             port map(D => clkena(5),
                      CLRN => vcc,
                      PRN => vcc,
                      ENA => vcc,
                      CLK => not_clk5_tmp,
                      Q => ena5);
   clk(5) <= ena5 and clk5_tmp;

   extclk0_tmp <= e0_clk when i_extclk0_counter = "e0" else
                  e1_clk when i_extclk0_counter = "e1" else
                  e2_clk when i_extclk0_counter = "e2" else
                  e3_clk when i_extclk0_counter = "e3" else
                  '0';
   not_extclk0_tmp <= not extclk0_tmp;
   extena0_reg : dffe
             port map(D => extclkena(0),
                      CLRN => vcc,
                      PRN => vcc,
                      ENA => vcc,
                      CLK => not_extclk0_tmp,
                      Q => extena0);
   extclk(0) <= extena0 and extclk0_tmp;

   extclk1_tmp <= e0_clk when i_extclk1_counter = "e0" else
                  e1_clk when i_extclk1_counter = "e1" else
                  e2_clk when i_extclk1_counter = "e2" else
                  e3_clk when i_extclk1_counter = "e3" else
                  '0';
   not_extclk1_tmp <= not extclk1_tmp;
   extena1_reg : dffe
             port map(D => extclkena(1),
                      CLRN => vcc,
                      PRN => vcc,
                      ENA => vcc,
                      CLK => not_extclk1_tmp,
                      Q => extena1);
   extclk(1) <= extena1 and extclk1_tmp;

   extclk2_tmp <= e0_clk when i_extclk2_counter = "e0" else
                  e1_clk when i_extclk2_counter = "e1" else
                  e2_clk when i_extclk2_counter = "e2" else
                  e3_clk when i_extclk2_counter = "e3" else
                  '0';
   not_extclk2_tmp <= not extclk2_tmp;
   extena2_reg : dffe
             port map(D => extclkena(2),
                      CLRN => vcc,
                      PRN => vcc,
                      ENA => vcc,
                      CLK => not_extclk2_tmp,
                      Q => extena2);
   extclk(2) <= extena2 and extclk2_tmp;

   extclk3_tmp <= e0_clk when i_extclk3_counter = "e0" else
                  e1_clk when i_extclk3_counter = "e1" else
                  e2_clk when i_extclk3_counter = "e2" else
                  e3_clk when i_extclk3_counter = "e3" else
                  '0';
   not_extclk3_tmp <= not extclk3_tmp;
   extena3_reg : dffe
             port map(D => extclkena(3),
                      CLRN => vcc,
                      PRN => vcc,
                      ENA => vcc,
                      CLK => not_extclk3_tmp,
                      Q => extena3);
   extclk(3) <= extena3 and extclk3_tmp;

end vital_pll;


library ieee;
use ieee.std_logic_1164.all;

entity altpll is
	generic (	
		device_family		: string := "stratix" ;
		operation_mode     	: string := "NORMAL" ;
		pll_type     		: string := "TYPE_AUTO" ;
		implementation_type     : string := "TYPE_EGPP" ;
		qualify_conf_done       : string := "OFF" ;
		compensate_clock        : string := "CLK0" ;
                scan_chain              : string := "long";
                primary_clock           : string := "INCLK0" ;
                inclk0_input_frequency  : positive ;
                inclk1_input_frequency  : natural := 1;
                gate_lock_signal        : string := "NO";
                gate_lock_counter       : integer := 1;
                lock_high               : natural := 1;
                lock_low                : natural := 1;
                valid_lock_multiplier   : natural := 5;
                invalid_lock_multiplier : natural := 5;
                switch_over_on_lossclk  : string := "OFF" ;
                switch_over_on_gated_lock  : string := "OFF" ;
                switch_over_counter     : natural := 0;
                enable_switch_over_counter     : string := "OFF";
                feedback_source         : string := "EXT_CLK0" ;
                bandwidth               : natural := 0;
                bandwidth_type          : string := "UNUSED";
                spread_frequency        : natural := 0;
                down_spread             : natural := 0;
		simulation_type		: string := "functional";
		-- internal clock specifications
		clk5_multiply_by     	: positive := 1;
		clk4_multiply_by     	: positive := 1;
		clk3_multiply_by     	: positive := 1;
		clk2_multiply_by     	: positive := 1;
		clk1_multiply_by     	: positive := 1;
		clk0_multiply_by     	: positive := 1;
		clk5_divide_by     	: positive := 1;
		clk4_divide_by     	: positive := 1;
		clk3_divide_by     	: positive := 1;
		clk2_divide_by     	: positive := 1;
		clk1_divide_by     	: positive := 1;
		clk0_divide_by     	: positive := 1;
		clk5_phase_shift     	: string := "0";
		clk4_phase_shift     	: string := "0";
		clk3_phase_shift     	: string := "0";
		clk2_phase_shift     	: string := "0";
		clk1_phase_shift     	: string := "0";
		clk0_phase_shift     	: string := "0";
		clk5_time_delay     	: string := "0";
		clk4_time_delay     	: string := "0";
		clk3_time_delay     	: string := "0";
		clk2_time_delay     	: string := "0";
		clk1_time_delay     	: string := "0";
		clk0_time_delay     	: string := "0";
		clk5_duty_cycle     	: natural := 50;
		clk4_duty_cycle     	: natural := 50;
		clk3_duty_cycle     	: natural := 50;
		clk2_duty_cycle     	: natural := 50;
		clk1_duty_cycle     	: natural := 50;
		clk0_duty_cycle     	: natural := 50;
		-- external clock specifications
		extclk3_multiply_by     : positive := 1;
		extclk2_multiply_by     : positive := 1;
		extclk1_multiply_by     : positive := 1;
		extclk0_multiply_by     : positive := 1;
		extclk3_divide_by     	: positive := 1;
		extclk2_divide_by     	: positive := 1;
		extclk1_divide_by     	: positive := 1;
		extclk0_divide_by     	: positive := 1;
		extclk3_phase_shift     : string := "0";
		extclk2_phase_shift     : string := "0";
		extclk1_phase_shift     : string := "0";
		extclk0_phase_shift     : string := "0";
		extclk3_time_delay     	: string := "0";
		extclk2_time_delay     	: string := "0";
		extclk1_time_delay     	: string := "0";
		extclk0_time_delay     	: string := "0";
		extclk3_duty_cycle     	: natural := 50;
		extclk2_duty_cycle     	: natural := 50;
		extclk1_duty_cycle     	: natural := 50;
		extclk0_duty_cycle     	: natural := 50;
		-- advanced use parameters
		vco_min     		: natural := 0;
		vco_max     		: natural := 0;
		vco_center     		: natural := 0;
		pfd_min     		: natural := 0;
		pfd_max    		: natural := 0;
		m_initial     		: natural := 1;
		m     			: natural := 0;
		n     			: natural := 1;
		m2     			: natural := 1;
		n2     			: natural := 1;
		ss     			: natural := 1;
		l0_high     		: natural := 1;
		l1_high     		: natural := 1;
		g0_high     		: natural := 1;
		g1_high     		: natural := 1;
		g2_high    		: natural := 1;
		g3_high     		: natural := 1;
		e0_high     		: natural := 1;
		e1_high    		: natural := 1;
		e2_high     		: natural := 1;
		e3_high     		: natural := 1;
		l0_low     		: natural := 1;
		l1_low     		: natural := 1;
		g0_low     		: natural := 1;
		g1_low     		: natural := 1;
		g2_low     		: natural := 1;
		g3_low     		: natural := 1;
		e0_low     		: natural := 1;
		e1_low     		: natural := 1;
		e2_low     		: natural := 1;
		e3_low     		: natural := 1;
		l0_initial     		: natural := 1;
		l1_initial     		: natural := 1;
		g0_initial     		: natural := 1;
		g1_initial     		: natural := 1;
		g2_initial     		: natural := 1;
		g3_initial     		: natural := 1;
		e0_initial     		: natural := 1;
		e1_initial     		: natural := 1;
		e2_initial     		: natural := 1;
		e3_initial     		: natural := 1;
		l0_mode     		: string := "bypass" ;
		l1_mode     		: string := "bypass" ;
		g0_mode     		: string := "bypass" ;
		g1_mode     		: string := "bypass" ;
		g2_mode     		: string := "bypass" ;
		g3_mode     		: string := "bypass" ;
		e0_mode     		: string := "bypass" ;
		e1_mode     		: string := "bypass" ;
		e2_mode     		: string := "bypass" ;
		e3_mode     		: string := "bypass" ;
		l0_ph     		: natural := 0;
		l1_ph     		: natural := 0;
		g0_ph     		: natural := 0;
		g1_ph     		: natural := 0;
		g2_ph     		: natural := 0;
		g3_ph     		: natural := 0;
		e0_ph     		: natural := 0;
		e1_ph     		: natural := 0;
		e2_ph     		: natural := 0;
		e3_ph     		: natural := 0;
		m_ph     		: natural := 0;
		l0_time_delay     	: natural := 0;
		l1_time_delay     	: natural := 0;
		g0_time_delay     	: natural := 0;
		g1_time_delay     	: natural := 0;
		g2_time_delay     	: natural := 0;
		g3_time_delay     	: natural := 0;
		e0_time_delay     	: natural := 0;
		e1_time_delay     	: natural := 0;
		e2_time_delay     	: natural := 0;
		e3_time_delay     	: natural := 0;
		m_time_delay     	: natural := 0;
		n_time_delay     	: natural := 0;
		extclk3_counter     	: string := "e3" ;
		extclk2_counter     	: string := "e2" ;
		extclk1_counter     	: string := "e1" ;
		extclk0_counter     	: string := "e0" ;
		clk5_counter    	: string := "g3" ;
		clk4_counter     	: string := "g2" ;
		clk3_counter     	: string := "g1" ;
		clk2_counter     	: string := "g0" ;
		clk1_counter     	: string := "l1" ;
		clk0_counter     	: string := "l0" ;
		charge_pump_current     : natural := 2;
		loop_filter_r     	: string := "1.0";
		loop_filter_c		: natural := 5
	);
	port (
		inclk	   : in std_logic_vector(1 downto 0) := (others => '0');
		fbin     	: in std_logic := '1';
		pllena     	: in std_logic := '1';
		clkswitch       : in std_logic := '0';
		areset     	: in std_logic := '0';
		pfdena     	: in std_logic := '1';
		clkena     : in std_logic_vector(5 downto 0) := (others => '1');
		extclkena  : in std_logic_vector(3 downto 0) := (others => '1');
		scanclk     	: in std_logic := '1';
		scanaclr     	: in std_logic := '0';
		scandata	: in std_logic := '1';
		clk     	: out std_logic_vector(5 downto 0);
		extclk     	: out std_logic_vector(3 downto 0);
		clkbad     	: out std_logic_vector(1 downto 0);
		activeclock     : out std_logic;
		clkloss     	: out std_logic;
		locked		: out std_logic
	);
end altpll;

architecture behavior of altpll is

component stratix_pll
   generic (operation_mode : string := "normal";
            qualify_conf_done : string := "off";
            compensate_clock : string := "clk0";
            pll_type : string := "auto";
            scan_chain : string := "long";
	    simulation_type : string := "functional";

            clk0_multiply_by : integer := 1;
            clk0_divide_by : integer := 1;
            clk0_phase_shift : string := "0";
            clk0_time_delay : string := "0";
            clk0_duty_cycle : integer := 50;

            clk1_multiply_by : integer := 1;
            clk1_divide_by : integer := 1;
            clk1_phase_shift : string := "0";
            clk1_time_delay : string := "0";
            clk1_duty_cycle : integer := 50;

            clk2_multiply_by : integer := 1;
            clk2_divide_by : integer := 1;
            clk2_phase_shift : string := "0";
            clk2_time_delay : string := "0";
            clk2_duty_cycle : integer := 50;

            clk3_multiply_by : integer := 1;
            clk3_divide_by : integer := 1;
            clk3_phase_shift : string := "0";
            clk3_time_delay : string := "0";
            clk3_duty_cycle : integer := 50;

            clk4_multiply_by : integer := 1;
            clk4_divide_by : integer := 1;
            clk4_phase_shift : string := "0";
            clk4_time_delay : string := "0";
            clk4_duty_cycle : integer := 50;

            clk5_multiply_by : integer := 1;
            clk5_divide_by : integer := 1;
            clk5_phase_shift : string := "0";
            clk5_time_delay : string := "0";
            clk5_duty_cycle : integer := 50;

            extclk0_multiply_by : integer := 1;
            extclk0_divide_by : integer := 1;
            extclk0_phase_shift : string := "0";
            extclk0_time_delay : string := "0";
            extclk0_duty_cycle : integer := 50;

            extclk1_multiply_by : integer := 1;
            extclk1_divide_by : integer := 1;
            extclk1_phase_shift : string := "0";
            extclk1_time_delay : string := "0";
            extclk1_duty_cycle : integer := 50;

            extclk2_multiply_by : integer := 1;
            extclk2_divide_by : integer := 1;
            extclk2_phase_shift : string := "0";
            extclk2_time_delay : string := "0";
            extclk2_duty_cycle : integer := 50;

            extclk3_multiply_by : integer := 1;
            extclk3_divide_by : integer := 1;
            extclk3_phase_shift : string := "0";
            extclk3_time_delay : string := "0";
            extclk3_duty_cycle : integer := 50;

            primary_clock : string := "inclk0";
            inclk0_input_frequency : integer := 10000;
            inclk1_input_frequency : integer := 10000;
            gate_lock_signal : string := "yes";
            gate_lock_counter : integer := 1;
            lock_high : integer := 1;
            lock_low : integer := 1;
            valid_lock_multiplier : integer := 5;
            invalid_lock_multiplier : integer := 5;
--          switch_over_on_lossclk = "";
--          switch_over_counter : integer := "";
--          feedback_source = "";
--          bandwidth = "";
--          up_spread = "";
--          down_spread = "";

-- ADVANCED USE PARAMETERS
            m_initial : integer := 1;     -- 1-1024
            m : integer := 1;     -- 1-1024
            n: integer := 1;     -- 1-1024
            m2 : integer := 1;     -- 1-1024
            n2 : integer := 1;     -- 1-1024
            ss : integer := 1;     -- 1-32768

            l0_high : integer := 1;    -- 1-512
            l0_low : integer := 1;    -- 1-512
            l0_initial : integer := 1;    -- 1-512
            l0_mode : string := "bypass";       -- bypass,odd,even
            l0_ph : integer := 0;
            l0_time_delay : integer := 0;

            l1_high : integer := 1;
            l1_low : integer := 1;
            l1_initial : integer := 1;
            l1_mode : string := "bypass";
            l1_ph : integer := 0;
            l1_time_delay : integer := 0;

            g0_high : integer := 1;
            g0_low : integer := 1;
            g0_initial : integer := 1;
            g0_mode : string := "bypass";
            g0_ph : integer := 0;
            g0_time_delay : integer := 0;

            g1_high : integer := 1;
            g1_low : integer := 1;
            g1_initial : integer := 1;
            g1_mode : string := "bypass";
            g1_ph : integer := 0;
            g1_time_delay : integer := 0;

            g2_high : integer := 1;
            g2_low : integer := 1;
            g2_initial : integer := 1;
            g2_mode : string := "bypass";
            g2_ph : integer := 0;
            g2_time_delay : integer := 0;

          g3_high : integer := 1;
          g3_low : integer := 1;
          g3_initial : integer := 1;
          g3_mode : string := "bypass";
          g3_ph : integer := 0;
          g3_time_delay : integer := 0;

          e0_high : integer := 1;
          e0_low : integer := 1;
          e0_initial : integer := 1;
          e0_mode : string := "bypass";
          e0_ph : integer := 0;
          e0_time_delay : integer := 0;

          e1_high : integer := 1;
          e1_low : integer := 1;
          e1_initial : integer := 1;
          e1_mode : string := "bypass";
          e1_ph : integer := 0;
          e1_time_delay : integer := 0;

          e2_high : integer := 1;
          e2_low : integer := 1;
          e2_initial : integer := 1;
          e2_mode : string := "bypass";
          e2_ph : integer := 0;
          e2_time_delay : integer := 0;

          e3_high : integer := 1;
          e3_low : integer := 1;
          e3_initial : integer := 1;
          e3_mode : string := "bypass";
          e3_ph : integer := 0;
          e3_time_delay : integer := 0;

          m_ph : integer := 0;
          m_time_delay : integer := 0;

          extclk0_counter : string := "e0";
          extclk1_counter : string := "e1";
          extclk2_counter : string := "e2";
          extclk3_counter : string := "e3";

          clk0_counter : string := "g0";
          clk1_counter : string := "g1";
          clk2_counter : string := "g2";
          clk3_counter : string := "g3";
          clk4_counter : string := "l0";
          clk5_counter : string := "l1";

          enable0_counter : string := "l0";
          enable1_counter : string := "l1";

          charge_pump_current : integer := 2;

          loop_filter_r : string := "1.0";
          loop_filter_c : natural := 1

   );
   port (inclk : IN std_logic_vector(1 downto 0);
         fbin : IN std_logic;
         ena : IN std_logic := '1';
         clkswitch : IN std_logic;
         areset : IN std_logic;
         pfdena : IN std_logic;
         clkena : IN std_logic_vector(5 downto 0);
         extclkena : IN std_logic_vector(3 downto 0);
         scanaclr : IN std_logic;
         scandata : IN std_logic;
         scanclk : IN std_logic;
         clk : OUT std_logic_vector(5 downto 0);
         extclk : OUT std_logic_vector(3 downto 0);
         clkbad : OUT std_logic_vector(1 downto 0);
         activeclock : OUT std_logic;
         locked : OUT std_logic;
         clkloss : OUT std_logic;
-- lvds specific ports
         comparator : IN std_logic := '1';
         enable0 : OUT std_logic;
         enable1 : OUT std_logic
   );
end component;

begin

m1:	stratix_pll

	generic map(
		operation_mode    => operation_mode, 
		pll_type     		=> pll_type,		
		-- implementation_type     			?
		-- qualify_conf_done      	=> quailfy_conf_done, 
		compensate_clock       	=> compensate_clock, 
		-- scan_chain 					?
		simulation_type => simulation_type,

		-- internal clock specifications

		clk5_multiply_by     	=> clk5_multiply_by,
		clk4_multiply_by     	=> clk4_multiply_by,	
		clk3_multiply_by     	=> clk3_multiply_by,	
		clk2_multiply_by     	=> clk2_multiply_by,	
		clk1_multiply_by     	=> clk1_multiply_by,	
		clk0_multiply_by     	=> clk0_multiply_by,	
		clk5_divide_by     	=> clk5_divide_by,	
		clk4_divide_by    	=> clk4_divide_by, 	
		clk3_divide_by     	=> clk3_divide_by,
		clk2_divide_by     	=> clk2_divide_by,
		clk1_divide_by     	=> clk1_divide_by,
		clk0_divide_by     	=> clk0_divide_by,
		clk5_phase_shift     	=> clk5_phase_shift,
		clk4_phase_shift     	=> clk4_phase_shift,
		clk3_phase_shift     	=> clk3_phase_shift,
		clk2_phase_shift     	=> clk2_phase_shift,
		clk1_phase_shift     	=> clk1_phase_shift,
		clk0_phase_shift     	=> clk0_phase_shift,
		clk5_time_delay     	=> clk5_time_delay,
		clk4_time_delay     	=> clk4_time_delay,
		clk3_time_delay     	=> clk3_time_delay,
		clk2_time_delay     	=> clk2_time_delay,
		clk1_time_delay     	=> clk1_time_delay,
		clk0_time_delay     	=> clk0_time_delay,
		clk5_duty_cycle     	=> clk5_duty_cycle,
		clk4_duty_cycle     	=> clk4_duty_cycle,
		clk3_duty_cycle     	=> clk3_duty_cycle,
		clk2_duty_cycle     	=> clk2_duty_cycle,
		clk1_duty_cycle     	=> clk1_duty_cycle,
		clk0_duty_cycle     	=> clk0_duty_cycle,

		-- external clock specifications

		extclk3_multiply_by    	=> extclk3_multiply_by,	
		extclk2_multiply_by    	=> extclk2_multiply_by,	
		extclk1_multiply_by    	=> extclk1_multiply_by,	
		extclk0_multiply_by    	=> extclk0_multiply_by,	
		extclk3_divide_by     	=> extclk3_divide_by,
		extclk2_divide_by     	=> extclk2_divide_by,
		extclk1_divide_by     	=> extclk1_divide_by,
		extclk0_divide_by     	=> extclk0_divide_by,
		extclk3_phase_shift    	=> extclk3_phase_shift,
		extclk2_phase_shift    	=> extclk2_phase_shift,
		extclk1_phase_shift    	=> extclk1_phase_shift,
		extclk0_phase_shift    	=> extclk0_phase_shift,
		extclk3_time_delay     	=> extclk3_time_delay,
		extclk2_time_delay     	=> extclk2_time_delay,
		extclk1_time_delay     	=> extclk1_time_delay,
		extclk0_time_delay     	=> extclk0_time_delay,
		extclk3_duty_cycle     	=> extclk3_duty_cycle,
		extclk2_duty_cycle     	=> extclk2_duty_cycle,
		extclk1_duty_cycle     	=> extclk1_duty_cycle,
		extclk0_duty_cycle     	=> extclk0_duty_cycle,
		primary_clock     	=> primary_clock,
		inclk0_input_frequency  => inclk0_input_frequency,
		-- inclk1_input_frequency  => inclk1_input_frequency,
		-- glocked_Signal       => gate_lock_signal,        ?
		-- glocked_counter     	=> gate_lock_counter,
		lock_high     		=> lock_high,
		lock_low     		=> lock_low,
		valid_lock_multiplier   => valid_lock_multiplier,
		invalid_lock_multiplier => invalid_lock_multiplier,
		-- switch_over_on_lossclk  => switch_over_on_lossclk,
		-- switch_over_counter     => switch_over_counter,
		-- feedback_source         => feedback_source,
		-- bandwidth               => badwidth, 
		-- spread_frequency        => up_spread,
		-- down_spread             => down_spread,

		-- advanced use parameters

		-- vco_min     	=> ?
		-- vco_max     	?	
		-- vco_center     	?	
		-- pfd_min     	?	
		-- pfd_max    	?	
		m_initial     		=> m_initial, 
		m     			=> m,
		n     			=> n,
		m2     			=> m2,
		n2     			=> n2,
		ss     			=> ss,	
		l0_high     		=> l0_high,
		l1_high     		=> l1_high,
		g0_high     		=> g0_high,
		g1_high     		=> g1_high,
		g2_high    		=> g2_high,
		g3_high     		=> g3_high,
		e0_high     		=> e0_high,
		e1_high    		=> e1_high,
		e2_high     		=> e2_high,
		e3_high     		=> e3_high,
		l0_low     		=> l0_low,
		l1_low     		=> l1_low,
		g0_low     		=> g0_low,
		g1_low     		=> g1_low,
		g2_low     		=> g2_low,
		g3_low     		=> g3_low,
		e0_low     		=> e0_low,
		e1_low     		=> e1_low,
		e2_low     		=> e2_low,
		e3_low     		=> e3_low,
		l0_initial     		=> l0_initial,
		l1_initial     		=> l1_initial,
		g0_initial     		=> g0_initial,
		g1_initial     		=> g1_initial,
		g2_initial     		=> g2_initial,
		g3_initial     		=> g3_initial,	
		e0_initial     		=> e0_initial,
		e1_initial     		=> e1_initial,
		e2_initial     		=> e2_initial,
		e3_initial     		=> e3_initial,
		l0_mode     		=> l0_mode,
		l1_mode     		=> l1_mode,
		g0_mode     		=> g0_mode,
		g1_mode     		=> g1_mode,
		g2_mode     		=> g2_mode,
		g3_mode     		=> g3_mode,
		e0_mode     		=> e0_mode,
		e1_mode     		=> e1_mode,
		e2_mode     		=> e2_mode,
		e3_mode     		=> e3_mode,
		l0_ph     		=> l0_ph,
		l1_ph     		=> l1_ph,
		g0_ph     		=> g0_ph,
		g1_ph     		=> g1_ph,
		g2_ph     		=> g2_ph,
		g3_ph     		=> g3_ph,
		e0_ph     		=> e0_ph,
		e1_ph     		=> e1_ph,
		e2_ph     		=> e2_ph,
		e3_ph     		=> e3_ph,
		m_ph     		=> m_ph,	
		l0_time_delay     	=> l0_time_delay,	
		l1_time_delay     	=> l1_time_delay,
		g0_time_delay     	=> g0_time_delay,
		g1_time_delay     	=> g1_time_delay,
		g2_time_delay     	=> g2_time_delay,
		g3_time_delay     	=> g3_time_delay,
		e0_time_delay     	=> e0_time_delay,
		e1_time_delay     	=> e1_time_delay,
		e2_time_delay     	=> e2_time_delay,
		e3_time_delay     	=> e3_time_delay,
		m_time_delay     	=> m_time_delay,
		-- n_time_delay     	=> n_time_delay,
		extclk3_counter     	=> extclk3_counter,
		extclk2_counter     	=> extclk2_counter,
		extclk1_counter     	=> extclk1_counter,
		extclk0_counter     	=> extclk0_counter,
		clk5_counter    	=> clk5_counter,
		clk4_counter     	=> clk4_counter,
		clk3_counter     	=> clk3_counter,
		clk2_counter     	=> clk2_counter,
		clk1_counter     	=> clk1_counter,
		clk0_counter     	=> clk0_counter,
		charge_pump_current     => charge_pump_current,
		loop_filter_r     	=> loop_filter_r	
	)

	port map(
		inclk => inclk,
		-- inclk0 => inclk(0),       --?
		fbin => fbin,
		ena => pllena,
		clkswitch => clkswitch,
		areset  => areset, 
		pfdena => pfdena, 
		clkena => clkena,
		extclkena => extclkena,
		scanclk  => scanclk,
		scanaclr => scanaclr,
		scandata => scandata,
		clk   => clk, 
		extclk => extclk,
		clkbad => clkbad,
		activeclock => activeclock,
		clkloss  => clkloss, 
		locked=> locked	
	);
end behavior;

----------------------------------------------------------------------------
-- lvds receiver megafunction
----------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;


entity altlvds_rx is
    generic
      ( number_of_channels     : natural;
        deserialization_factor : natural;
	inclock_boost	       : natural := 0;
        registered_output      : string := "ON";
        inclock_period         : natural;
        clock_setting          : string := "UNUSED";
        cds_mode               : string := "UNUSED";
        intended_device_family : string := "APEX20KE";
		input_data_rate: natural := 0;
		inclock_data_alignment:string:= "EDGE_ALIGNED";
		registered_data_align_input:string:="ON";
		common_rx_tx_pll:string:="ON");
    port
      ( rx_in           : in std_logic_vector(number_of_channels-1 downto 0);
        rx_inclock      : in std_logic;
        rx_deskew       : in std_logic := '0';
        rx_pll_enable   : in std_logic := '1';
	rx_data_align   : in std_logic := '0';
        rx_out          : out std_logic_vector(deserialization_factor*number_of_channels -1 downto 0);
        rx_outclock     : out std_logic;
        rx_locked       : out std_logic );


function clock_boost_calc (constant i_input_data_rate, i_inclock_period, i_deserialization_factor, i_inclock_boost : in natural) return natural is
variable i_input_clock_boost: natural;
begin
if (i_input_data_rate /= 0 and i_inclock_period /= 0) then
			i_input_clock_boost := (i_input_data_rate * 100) / i_inclock_period;
		else
			if inclock_boost =0 then
				i_input_clock_boost := i_deserialization_factor;
			else
				i_input_clock_boost := i_inclock_boost;
			end if;
		end if;
return i_input_clock_boost;

end clock_boost_calc; 

function get_phase_delay (constant i_phase_delay : in string) return string is
variable my_phase : string (1 to 11);
begin
-- returns the delay in radians 
if i_phase_delay = "EDGE_ALIGNED" then
	my_phase := "0.000000000";
elsif i_phase_delay = "CENTER_ALIGNED" then
	my_phase := "1.570796326";
elsif i_phase_delay = "45_DEGREES" then
	my_phase := "0.785398163";
elsif i_phase_delay = "135_DEGREES" then
	my_phase := "2.356194490";
elsif i_phase_delay = "180_DEGREES" then
	my_phase := "3.141592653";
elsif i_phase_delay = "225_DEGREES" then
	my_phase := "3.926990816";
elsif i_phase_delay = "270_DEGREES" then
	my_phase := "4.712388980";
elsif i_phase_delay = "315_DEGREES" then
	my_phase := "5.497787143";
end if;

return my_phase;

end get_phase_delay;



end altlvds_rx;

architecture behavior of altlvds_rx is

type channel_cnt is array (number_of_channels-1 downto 0) of integer;

signal rx_out_rgd : std_logic_vector(deserialization_factor*number_of_channels -1 downto 0) := (others => '0');
signal rx_hold_rgd : std_logic_vector(deserialization_factor*number_of_channels -1 downto 0) := (others => '0');
signal rx_out_int : std_logic_vector(deserialization_factor*number_of_channels -1 downto 0);
signal data_out : std_logic_vector(deserialization_factor*number_of_channels -1 downto 0);
signal rx_clock0_int : std_logic;
signal rx_clock1_int : std_logic;
signal rx_pll_clk0 : std_logic;
signal rx_pll_clk1 : std_logic;
signal apex20ke_locked_int: std_logic;
signal apex20ke_pll_clk0: std_logic; 
signal apex20ke_pll_clk1: std_logic; 
signal mercury_locked_int_deser: std_logic;
signal mercury_pll_clk0_deser: std_logic; 
signal mercury_pll_clk1_deser: std_logic; 
signal mercury_locked_int_boost: std_logic;
signal mercury_pll_clk0_boost: std_logic; 
signal mercury_pll_clk1_boost: std_logic; 
signal mercury_locked_int: std_logic;
signal mercury_pll_clk0: std_logic; 
signal mercury_pll_clk1: std_logic; 
signal rx_mercury_slow_clock : std_logic;
signal rx_locked_int : std_logic;
signal deskew_done: std_logic_vector(number_of_channels-1 downto 0) := (others => '1');
signal calibrate: std_logic_vector(number_of_channels-1 downto 0) := (others => '0');
signal apex20ke_en: std_logic;
signal mercury_en: std_logic;
signal mercury_boost_en: std_logic;
signal first_load : std_logic := '0';
signal temp_zero : std_logic := '0';
signal temp_high : std_logic_vector (1 to 5)  := (others => '1');
signal temp_clk : std_logic_vector  (1 to 4):= (others => '0');

 --signal input_clock_boost: natural:=0;
signal deser_boost:natural:=0;
signal rx_data_align_reg: std_logic := '0';


component altclklock
    generic
      ( inclock_period       : natural := 10000;
        clock0_boost         : natural := 1;
        clock1_boost         : natural := 1;
        clock1_divide        : natural := 1;
        valid_lock_cycles    : natural := 1;
        intended_device_family  : string := "APEX20KE";
			outclock_phase_shift : natural :=0 );
    port
      ( inclock   : in std_logic;
        inclocken : in std_logic;
        clock0    : out std_logic;
        clock1    : out std_logic;
        locked    : out std_logic );
end component;

component altpll 
	generic (	
                inclk0_input_frequency  : positive ;
                clk1_multiply_by     : positive := 1;
					    clk0_multiply_by     : positive := 1;
						  clk1_divide_by     	: positive := 1;
		           clk0_phase_shift     : string := "0";
		           device_family : string := "Stratix"
	);
	port (
		inclk	   : in std_logic_vector(1 downto 0) := (others => '0');
		clkena     : in std_logic_vector(5 downto 0) := (others => '1');
		clk     	: out std_logic_vector(5 downto 0);
		locked		: out std_logic
	);
end component;

begin

	process (rx_clock0_int, rx_data_align)
	begin

		if (rx_clock0_int='1' and rx_clock0_int'event) or registered_data_align_input /= "ON" then
			rx_data_align_reg <= rx_data_align;
		end if;
	end process;


    rx_out <= rx_out_rgd when registered_output = "ON" else rx_out_int;
    rx_out_int <= rx_hold_rgd when (deserialization_factor > 2 and deserialization_factor < 7) else data_out;
    rx_clock0_int <= rx_pll_clk0 when deserialization_factor > 1 else rx_inclock;
    rx_clock1_int <= rx_pll_clk1 when deserialization_factor > 1 else rx_inclock;
    rx_outclock <= rx_clock1_int;
    rx_locked <= rx_locked_int when deserialization_factor > 1 else '1';
    mercury_pll_clk0 <= mercury_pll_clk0_deser when inclock_boost = 0 else mercury_pll_clk0_boost;
    mercury_pll_clk1 <= mercury_pll_clk1_deser when inclock_boost = 0 else mercury_pll_clk1_boost;
    rx_pll_clk0 <= mercury_pll_clk0 when (intended_device_family = "MERCURY" or intended_device_family = "STRATIX") else apex20ke_pll_clk0;
    rx_pll_clk1 <= rx_mercury_slow_clock when (intended_device_family = "MERCURY" or intended_device_family = "STRATIX") else apex20ke_pll_clk1;
    mercury_locked_int <= mercury_locked_int_deser when inclock_boost = 0 else mercury_locked_int_boost;
    rx_locked_int <= mercury_locked_int when (intended_device_family = "MERCURY" or intended_device_family = "STRATIX") else apex20ke_locked_int;
    apex20ke_en <= rx_pll_enable when ( intended_device_family = "APEX20KE" or intended_device_family = "APEX20KC" or
                                (intended_device_family = "APEXII" or intended_device_family = "APEX II" or intended_device_family = "STRATIX") or
                                intended_device_family = "EXCALIBUR_ARM" or intended_device_family = "EXCALIBUR_MIPS") else '0';
    mercury_en <= rx_pll_enable when ((intended_device_family = "MERCURY" or intended_device_family = "STRATIX") and inclock_boost = 0) else '0';
    mercury_boost_en <= rx_pll_enable when ((intended_device_family = "MERCURY" or intended_device_family = "STRATIX") and inclock_boost /= 0) else '0';

		
		deser_boost <= deserialization_factor when inclock_boost = 0 else inclock_boost ;
		--input_clock_boost := deser_boost;

    U0: altclklock -- APEX20KE PLL
            generic map
              ( inclock_period => inclock_period,
                clock0_boost => deserialization_factor, clock1_boost => 1,
                valid_lock_cycles => 5,
                intended_device_family => intended_device_family)
            port map
              ( inclock => rx_inclock, inclocken => apex20ke_en,
                clock0 => apex20ke_pll_clk0, clock1 => apex20ke_pll_clk1,
                locked => apex20ke_locked_int );

    U1: altclklock -- MERCURY PLL without inclock boost 
            generic map
              ( inclock_period => inclock_period,
                clock0_boost => deserialization_factor, clock1_boost => 1,
                valid_lock_cycles => 3,
                intended_device_family => intended_device_family)
            port map
              ( inclock => rx_inclock, inclocken => mercury_en,
                clock0 => mercury_pll_clk0_deser, clock1 => mercury_pll_clk1_deser,
                locked => mercury_locked_int_deser );

	YEAGER_PLL:
	if intended_device_family = "STRATIX" generate
	U2: altpll -- STRATIX PLL 
            generic map
              ( inclk0_input_frequency => inclock_period,
               
		clk0_multiply_by => clock_boost_calc (input_data_rate, inclock_period, deserialization_factor, inclock_boost), 
		clk1_multiply_by => clock_boost_calc (input_data_rate, inclock_period, deserialization_factor, inclock_boost),
                clk1_divide_by => deserialization_factor,
               
		clk0_phase_shift => get_phase_delay (inclock_data_alignment),
                device_family => intended_device_family)
            port map
              ( inclk(0) => rx_inclock, inclk (1) => temp_zero, clkena(0) => mercury_boost_en, clkena(1 to 5) => temp_high,
                clk(0) => mercury_pll_clk0_boost, clk(1) => mercury_pll_clk1_boost, clk (2 to 5) => temp_clk,
                locked => mercury_locked_int_boost );

	end generate YEAGER_PLL;

MERCURY_PLL:
if intended_device_family /= "Stratix" generate
	U2: altclklock -- MERCURY PLL with inclock_boost 
            generic map
              ( inclock_period => inclock_period,
                clock0_boost => inclock_boost, clock1_boost => inclock_boost,
                clock1_divide => deserialization_factor,
                valid_lock_cycles => 3,
                intended_device_family => intended_device_family)
            port map
              ( inclock => rx_inclock, inclocken => mercury_boost_en,
                clock0 => mercury_pll_clk0_boost, clock1 => mercury_pll_clk1_boost,
                locked => mercury_locked_int_boost );
	end generate MERCURY_PLL;



    msg: process  
    begin
        if (intended_device_family = "APEX20KE" or intended_device_family = "APEX20KC" or
             intended_device_family = "EXCALIBUR_ARM" or intended_device_family = "EXCALIBUR_MIPS") and
             (deserialization_factor /= 4) and (deserialization_factor /= 7) and (deserialization_factor /= 8) then
                ASSERT FALSE
                REPORT "APEX20KE does not support the specified deserialization factor!"
                SEVERITY ERROR;
        elsif (intended_device_family = "MERCURY") and
              (((deserialization_factor > 12) and (deserialization_factor /= 14)
                and (deserialization_factor /= 16) and (deserialization_factor /= 18)
                and (deserialization_factor /= 20)) or (deserialization_factor<3)) then
                ASSERT FALSE
                REPORT "MERCURY does not support the specified deserialization factor!"
                SEVERITY ERROR;
        elsif ((intended_device_family = "APEXII" or intended_device_family = "APEX II")) and
              ((deserialization_factor > 10) or (deserialization_factor < 4)) then
                ASSERT FALSE
                REPORT "APEXII does not support the specified deserialization factor!"
                SEVERITY ERROR;
        end if;
        wait;
    end process;

    hssi_clkout: process (rx_clock0_int, mercury_pll_clk1)
    variable posedge_count: integer := 0;
    begin
        if deserialization_factor rem 2 = 0 then
            rx_mercury_slow_clock <= mercury_pll_clk1;
        else
            if mercury_pll_clk1'event and mercury_pll_clk1 = '1' then
                posedge_count := 0;
                rx_mercury_slow_clock <= mercury_pll_clk1;
            end if;
            if rx_clock0_int'event and rx_clock0_int = '1' then
                posedge_count := posedge_count+1;
            end if;
            if rx_clock0_int'event and (rx_clock0_int = '1') and (posedge_count=(deserialization_factor+1)/2+1) then
                rx_mercury_slow_clock <= NOT rx_mercury_slow_clock;
            end if;
        end if;
    end process;

    load_data: process(rx_clock0_int, rx_clock1_int, rx_deskew)
    variable edge_count: integer := 0;
    variable rxin_cnt: integer := 0;
    variable count: channel_cnt := (others => 0);
    variable sample: integer;
    variable start_data : integer := 0;
    variable init_deskew_pattern : boolean := true;
    variable check_deskew_pattern : boolean := false;
    variable deskew_pattern : std_logic_vector(deserialization_factor-1 downto 0);
    variable pattern : std_logic_vector(deserialization_factor*number_of_channels -1 downto 0);
    variable data_int : std_logic_vector(deserialization_factor*number_of_channels -1 downto 0);
    begin
	if rx_data_align_reg = '0' then
        if deserialization_factor > 1 then
            -- At start up deskew is done due to inability to detect unconnected
            -- rx_deskew pin.
            if (rx_deskew'event and rx_deskew = '1' and ( intended_device_family = "APEX20KE" or
                intended_device_family = "APEX20KC" or intended_device_family = "EXCALIBUR_ARM" or
                intended_device_family = "EXCALIBUR_MIPS" or
                (intended_device_family = "APEXII" or intended_device_family = "APEX II"  or intended_device_family="Stratix")) ) then
                deskew_done <= (others => '0');
                calibrate <= (others => '1');
            end if;
            if rx_clock1_int'event and rx_clock1_int = '1' then
                if rx_deskew = '0' then
                    calibrate <= (others => '0');
                end if;
                edge_count := 0;
                -- Deskewing is only for APEX20KE/APEXII LVDS mode.
                -- Initialise calibration pattern variables.
                if init_deskew_pattern = true then
                    init_deskew_pattern := false;
                    if  (((intended_device_family = "APEK20KE") or (intended_device_family = "APEX20KC") or 
                        (intended_device_family = "EXCALIBUR_ARM") or (intended_device_family = "EXCALIBUR_MIPS")) and
                        ((deserialization_factor = 4) or (deserialization_factor = 7) or (deserialization_factor = 8))) then
                        check_deskew_pattern := true;
                        case deserialization_factor is
                            when 8 => deskew_pattern := "00111100";
                            when 7 => deskew_pattern := "0011100";
                            when 4 => deskew_pattern := "1100";
                            when others =>
                                    ASSERT FALSE
                                    REPORT "APEX20KE does not support the specified deserialization factor!"
                                    SEVERITY ERROR;
                        end case;
                    elsif (((intended_device_family = "APEXII" or intended_device_family = "APEX II" or intended_device_family="Stratix" )) and 
                            (deserialization_factor <= 10) and (deserialization_factor >= 4)) then
                            check_deskew_pattern := true;
                            if (cds_mode = "SINGLE_BIT") then
                                case deserialization_factor is
                                when 10 => deskew_pattern := "0001111100";
                                when 9 => deskew_pattern := "000111100";
                                when 8 => deskew_pattern := "00111100";
                                when 7 => deskew_pattern := "0011100";
                                when 6 => deskew_pattern := "011100";
                                when 5 => deskew_pattern := "01100";
                                when 4 => deskew_pattern := "1100";
                                when others =>
                                    ASSERT FALSE
                                    REPORT "APEXII does not support the specified deserialization factor!"
                                    SEVERITY ERROR;
                                end case;
                            else 
                                case deserialization_factor is
                                when 10 => deskew_pattern := "0101010101";
                                when 9 => deskew_pattern := "010101010";
                                when 8 => deskew_pattern := "01010101";
                                when 7 => deskew_pattern := "0101010";
                                when 6 => deskew_pattern := "010101";
                                when 5 => deskew_pattern := "01010";
                                when 4 => deskew_pattern := "0101";
                                when others =>
                                    ASSERT FALSE
                                    REPORT "APEXII does not support the specified deserialization factor!"
                                    SEVERITY ERROR;
                                end case;
                            end if;
                    else
                        check_deskew_pattern := false; 
                    end if;
                end if;
                if (check_deskew_pattern = true)  then
                    for i in 0 to number_of_channels-1 loop
                        if calibrate(i) = '1' then
                            if ((pattern(((deserialization_factor*(i+1))-1) downto deserialization_factor*i) = deskew_pattern) or
                                ((pattern(((deserialization_factor*(i+1))-1) downto deserialization_factor*i) = not deskew_pattern) and 
                                ((intended_device_family = "APEXII" or intended_device_family = "APEX II"  or intended_device_family="Stratix" )) and (cds_mode = "MULTIPLE_BIT"))) then
                                count(i) := count(i) + 1;
                            else
                                count(i) := 0;
                            end if;
                            if count(i) >= 3 and rx_deskew = '0' then
                                deskew_done(i) <= '1' after ((inclock_period/deserialization_factor)*2 ps);
                            end if;
                        end if;
                    end loop;
                else
                    deskew_done <= (others => '1');
                end if;
            end if;
            if rx_clock0_int'event and rx_clock0_int = '0' then
                edge_count := edge_count + 1;
                if edge_count = 3 then
                    data_out <= data_int;
                    start_data := 1;
                    rxin_cnt := 0;
                end if;
                if start_data = 1 then
                    sample := rxin_cnt rem deserialization_factor;
                    rxin_cnt := rxin_cnt + 1;
                    for i in 0 to number_of_channels -1 loop
                        if deskew_done(i) = '1' then
                            -- Data gets shifted into MSB first.
                            data_int(((i+1)*deserialization_factor)-sample-1) := rx_in(i);
                        else
                            pattern(((i+1)*deserialization_factor)-sample-1) := rx_in(i);
                            data_int(((i+1)*deserialization_factor)-sample-1) := 'X';
                        end if;
                    end loop;
                end if;
            end if;
        else
            if rx_clock1_int'event and rx_clock1_int = '1' then
                data_out <= rx_in;
            end if;
       end if;
		end if;
    end process;

    parallel_reg: process(rx_clock1_int)
    begin
        if deserialization_factor > 1 then
            if rx_clock1_int = '1' then
                rx_out_rgd <= rx_out_int;
            elsif (rx_clock1_int = '0') and ((deserialization_factor > 2) and (deserialization_factor < 7)) then
                    rx_hold_rgd <= data_out;
            end if;
        else
            if rx_clock1_int = '1' then
                rx_out_rgd <= rx_in;
            end if;
        end if;
    end process parallel_reg;

end behavior;


----------------------------------------------------------------------------
-- lvds transmitter megafunction 
----------------------------------------------------------------------------
    
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

entity altlvds_tx is
    generic
      ( number_of_channels     : natural;
        deserialization_factor : natural;
        inclock_boost 	       : natural := 0;
        outclock_divide_by     : positive := 1;
        registered_input       : string := "ON";
        multi_clock            : string := "OFF";
        inclock_period         : natural;
        clock_setting          : string := "UNUSED";
        center_align_msb       : string := "UNUSED";
        intended_device_family : string := "APEX20KE";

	output_data_rate	: natural := 0;
	common_rx_tx_pll	: string := "ON";
	inclock_data_alignment  : string := "EDGE_ALIGEND";
	outclock_alignment : string := "EDGE_ALIGNED"
	);
    port
      ( tx_in           : in std_logic_vector(deserialization_factor*number_of_channels -1 downto 0);
        tx_inclock      : in std_logic;
        sync_inclock    : in std_logic := '0';
        tx_pll_enable   : in std_logic := '1';
        tx_out          : out std_logic_vector(number_of_channels-1 downto 0);
        tx_outclock     : out std_logic;
        tx_coreclock    : out std_logic;
        tx_locked       : out std_logic );

function clock_boost_calc (constant i_input_data_rate, i_inclock_period, i_deserialization_factor, i_inclock_boost : in natural) return natural is
variable i_input_clock_boost: natural;
begin
if (i_input_data_rate /= 0 and i_inclock_period /= 0) then
			i_input_clock_boost := (i_input_data_rate * 100) / i_inclock_period;
		else
			if inclock_boost =0 then
				i_input_clock_boost := i_deserialization_factor;
			else
				i_input_clock_boost := i_inclock_boost;
			end if;
		end if;
return i_input_clock_boost;

end clock_boost_calc; 

function get_phase_delay (constant i_phase_delay : in string) return string is
variable my_phase : string (1 to 11);
begin
-- returns the delay in radians 
if i_phase_delay = "EDGE_ALIGNED" then
	my_phase := "0.000000000";
elsif i_phase_delay = "CENTER_ALIGNED" then
	my_phase := "1.570796326";
elsif i_phase_delay = "45_DEGREES" then
	my_phase := "0.785398163";
elsif i_phase_delay = "135_DEGREES" then
	my_phase := "2.356194490";
elsif i_phase_delay = "180_DEGREES" then
	my_phase := "3.141592653";
elsif i_phase_delay = "225_DEGREES" then
	my_phase := "3.926990816";
elsif i_phase_delay = "270_DEGREES" then
	my_phase := "4.712388980";
elsif i_phase_delay = "315_DEGREES" then
	my_phase := "5.497787143";
end if;

return my_phase;

end get_phase_delay;

	function int_to_str( value : integer ) return string is
	variable ivalue,index : integer;
	variable digit : integer;
    variable line_no: string(8 downto 1) := "        ";  
	begin
		ivalue := value;
		index := 1;
		while (ivalue > 0) loop
			digit := ivalue MOD 10;
			ivalue := ivalue/10;
			case digit is
				when 0 =>
					line_no(index) := '0';
				when 1 =>
					line_no(index) := '1';
				when 2 =>
					line_no(index) := '2';
				when 3 =>
					line_no(index) := '3';
				when 4 =>
					line_no(index) := '4';
				when 5 =>
					line_no(index) := '5';
				when 6 =>
					line_no(index) := '6';
				when 7 =>
					line_no(index) := '7';
				when 8 =>
					line_no(index) := '8';
				when 9 =>
					line_no(index) := '9';
				when others =>
					ASSERT FALSE
					REPORT "Illegal number!"
					SEVERITY ERROR;
			end case;
			index := index + 1;
		end loop;
		return line_no;
	end;



end altlvds_tx;

architecture behavior of altlvds_tx is

signal tx_hold_rgd : std_logic_vector(deserialization_factor*number_of_channels -1 downto 0) := (others => '0');
signal tx_rgd : std_logic_vector(deserialization_factor*number_of_channels -1 downto 0) := (others => '0');
signal tx_in_int, data_int : std_logic_vector(deserialization_factor*number_of_channels -1 downto 0);
signal tx_clock0_int: std_logic;
signal tx_clock1_int: std_logic;
signal tx_pll_clk0: std_logic; 
signal tx_pll_clk1: std_logic; 
signal tx_pll_clk2: std_logic; 
signal tx_pll_clk2_deser: std_logic; 
signal tx_pll_clk2_boost: std_logic; 
signal apex20ke_locked_int: std_logic;
signal apex20ke_pll_clk0: std_logic; 
signal apex20ke_pll_clk1: std_logic; 
signal mercury_locked_int_boost: std_logic;
signal mercury_pll_clk0_boost: std_logic; 
signal mercury_pll_clk1_boost: std_logic; 
signal mercury_locked_int_deser: std_logic;
signal mercury_pll_clk0_deser: std_logic; 
signal mercury_pll_clk1_deser: std_logic; 
signal mercury_locked_int: std_logic;
signal mercury_pll_clk0: std_logic; 
signal mercury_pll_clk1: std_logic; 
signal tx_mercury_core_clock : std_logic;
signal tx_rgd_clk: std_logic; 
signal tx_locked_int: std_logic; 
signal apex20ke_en: std_logic;
signal mercury_en: std_logic;
signal mercury_boost_en: std_logic;
signal phase_inclock : integer;
signal phase_outclock : integer;
signal int_clock_boost : integer;
signal temp_zero : std_logic := '0';
signal temp_high : std_logic_vector (1 to 5)  := (others => '1');
signal temp_clk : std_logic_vector  (1 to 3):= (others => '0');


component altclklock
    generic
      ( inclock_period       : natural := 10000;
        clock0_boost         : natural := 1;
        clock1_boost         : natural := 1;
        clock1_divide        : natural := 1;
        clock2_boost         : natural := 1;
        clock2_divide        : natural := 1;
        valid_lock_cycles    : natural := 5;
        intended_device_family  : string := "APEX20KE";
			clock0_time_delay : string := "0";
			clock1_time_delay : string := "0" );
    port
      ( inclock   : in std_logic;
        inclocken : in std_logic;
        clock0    : out std_logic;
        clock1    : out std_logic;
        clock2    : out std_logic;
        locked    : out std_logic );
end component;

component altpll 
	generic (	
                inclk0_input_frequency  : positive ;
                clk1_multiply_by     : positive := 1;
					    clk0_multiply_by     : positive := 1;
						  clk2_multiply_by     : positive := 1;
							clk1_divide_by     	: positive := 1;
						  clk2_divide_by     	: positive := 1;
		           clk0_phase_shift     : string := "0";
						  clk1_phase_shift     : string := "0";
		           device_family : string := "Stratix"
	);
	port (
		inclk	   : in std_logic_vector(1 downto 0) := (others => '0');
		clkena     : in std_logic_vector(5 downto 0) := (others => '1');
		clk     	: out std_logic_vector(5 downto 0);
		locked		: out std_logic
	);
end component;

begin

    tx_in_int  <= tx_rgd when registered_input = "ON" else tx_in;
    tx_clock0_int <= tx_pll_clk0 when deserialization_factor > 1 else tx_inclock;
    tx_clock1_int <= tx_pll_clk1 when deserialization_factor > 1 else tx_inclock;
    tx_rgd_clk <= sync_inclock when multi_clock = "ON" else tx_inclock;
    tx_pll_clk2 <= tx_pll_clk2_deser when inclock_boost = 0 else tx_pll_clk2_boost;
    tx_outclock <= tx_pll_clk2 when intended_device_family = "MERCURY" else tx_clock1_int;
    tx_coreclock <= tx_mercury_core_clock when (((deserialization_factor rem 2)/=0) and (intended_device_family = "MERCURY")) else tx_clock1_int;
    tx_locked <= tx_locked_int when deserialization_factor > 1 else '1';
    mercury_pll_clk0 <= mercury_pll_clk0_deser when inclock_boost = 0 else mercury_pll_clk0_boost;
    mercury_pll_clk1 <= mercury_pll_clk1_deser when inclock_boost = 0 else mercury_pll_clk1_boost;
    tx_pll_clk0 <= mercury_pll_clk0 when intended_device_family = "MERCURY" or intended_device_family = "YEAGER" else apex20ke_pll_clk0;
    tx_pll_clk1 <= mercury_pll_clk1 when intended_device_family = "MERCURY" or intended_device_family = "YEAGER" else apex20ke_pll_clk1;
    mercury_locked_int <= mercury_locked_int_deser when inclock_boost = 0 else mercury_locked_int_boost;
    tx_locked_int <= mercury_locked_int when (intended_device_family = "MERCURY" or intended_device_family = "YEAGER") else apex20ke_locked_int;
    apex20ke_en <= tx_pll_enable when ( intended_device_family = "APEX20KE" or intended_device_family = "APEX20KC" or
                              (intended_device_family = "APEXII" or intended_device_family = "APEX II") or
                              intended_device_family = "EXCALIBUR_ARM" or intended_device_family = "EXCALIBUR_MIPS") else '0';
    mercury_en <= tx_pll_enable when ((intended_device_family = "MERCURY" or intended_device_family = "YEAGER") and inclock_boost = 0) else '0';
    mercury_boost_en <= tx_pll_enable when ((intended_device_family = "MERCURY" or intended_device_family = "YEAGER") and inclock_boost /= 0) else '0';


			int_clock_boost <= inclock_boost;


    U0: altclklock -- APEX20KE PLL
            generic map
              ( inclock_period => inclock_period,
                clock0_boost => deserialization_factor, clock1_boost => 1,
                valid_lock_cycles => 5,
                intended_device_family => intended_device_family)
            port map
              ( inclock => tx_inclock, inclocken => apex20ke_en,
                clock0 => apex20ke_pll_clk0, clock1 => apex20ke_pll_clk1,
                locked => apex20ke_locked_int );

    U1: altclklock -- MERCURY PLL with inclock boost = 0
            generic map
              ( inclock_period => inclock_period,
                clock0_boost => deserialization_factor, clock1_boost => 1,
                clock2_boost => deserialization_factor, clock2_divide => outclock_divide_by,
                valid_lock_cycles => 3,
                intended_device_family => intended_device_family)
            port map
              ( inclock => tx_inclock, inclocken => mercury_en,
                clock0 => mercury_pll_clk0_deser, clock1 => mercury_pll_clk1_deser,
                clock2 => tx_pll_clk2_deser, locked => mercury_locked_int_deser );


	YEAGER_PLL:
	if intended_device_family = "STRATIX" generate
	U2: altpll -- STRATIX PLL 
            generic map
              ( inclk0_input_frequency => inclock_period,
               
		clk0_multiply_by => clock_boost_calc (output_data_rate, inclock_period, deserialization_factor, inclock_boost), 
		clk1_multiply_by => clock_boost_calc (output_data_rate, inclock_period, deserialization_factor, inclock_boost),
                clk1_divide_by => deserialization_factor,
		clk2_multiply_by => clock_boost_calc (output_data_rate, inclock_period, deserialization_factor, inclock_boost),
		clk2_divide_by => outclock_divide_by,
               
		clk0_phase_shift => get_phase_delay (inclock_data_alignment),
		clk1_phase_shift => get_phase_delay (outclock_alignment),
                device_family => intended_device_family)
            port map
              ( inclk(0) => tx_inclock, inclk (1) => temp_zero, clkena(0) => mercury_boost_en, clkena(1 to 5) => temp_high,
                clk(0) => mercury_pll_clk0_boost, clk(1) => mercury_pll_clk1_boost, clk(2) => tx_pll_clk2_boost, clk (3 to 5) => temp_clk ,
                locked => mercury_locked_int_boost );

	end generate YEAGER_PLL;

	MERCURY_PLL:
	if intended_device_family /= "STRATIX" generate
	    U2: altclklock -- MERCURY PLL with inclock boost
            generic map
              ( inclock_period => inclock_period,
                clock0_boost => inclock_boost,
                clock1_boost => inclock_boost, clock1_divide => deserialization_factor,
                clock2_boost => inclock_boost, clock2_divide => outclock_divide_by,
                valid_lock_cycles => 3,
                intended_device_family => intended_device_family)
            port map
              ( inclock => tx_inclock, inclocken => mercury_boost_en,
                clock0 => mercury_pll_clk0_boost, clock1 => mercury_pll_clk1_boost,
                clock2 => tx_pll_clk2_boost, locked => mercury_locked_int_boost );
	end generate MERCURY_PLL;


    msg: process 
    begin
        if (intended_device_family = "APEX20KE" or intended_device_family = "APEX20KC" or
             intended_device_family = "EXCALIBUR_ARM" or intended_device_family = "EXCALIBUR_MIPS") and
             (deserialization_factor /= 4) and (deserialization_factor /= 7) and (deserialization_factor /= 8) then
                ASSERT FALSE
                REPORT "APEX20KE does not support the specified deserialization factor!"
                SEVERITY ERROR;
        elsif (intended_device_family = "MERCURY") and
              (((deserialization_factor > 12) and (deserialization_factor /= 14)
                and (deserialization_factor /= 16) and (deserialization_factor /= 18)
                and (deserialization_factor /= 20)) or (deserialization_factor<3)) then
                ASSERT FALSE
                REPORT "MERCURY does not support the specified deserialization factor!"
                SEVERITY ERROR;
        elsif (((intended_device_family = "APEXII" or intended_device_family = "APEX II")) and
              ((deserialization_factor > 10) or (deserialization_factor < 4))) then
                ASSERT FALSE
                REPORT "APEXII does not support the specified deserialization factor!"
                SEVERITY ERROR;
        end if;
        wait;
    end process;

    serialize: process(tx_clock0_int, tx_clock1_int)
    variable posedge_count: integer := 0;
    variable negedge_count: integer := 0;
    variable shift_data : std_logic := '0';
    variable count: integer := 0;
    variable sample: integer;
    variable tx_in2: std_logic_vector(deserialization_factor*number_of_channels -1 downto 0);
    begin
        if deserialization_factor > 1 then
            if tx_clock1_int'event and tx_clock1_int = '1' then
                posedge_count := 0;
                negedge_count := 0;
            end if;
            if (tx_clock0_int'event and tx_clock0_int = '0') then
                negedge_count := negedge_count + 1;
                if negedge_count = 3 then
                    if ((deserialization_factor < 3) or (deserialization_factor > 6)) then
                        data_int <= tx_hold_rgd; 
                    else 
                        data_int <= tx_in_int;
                    end if;
                end if;
            end if;
            if (tx_clock0_int'event and tx_clock0_int = '1') then
                posedge_count := posedge_count + 1;
                if posedge_count = 3 then
                    tx_in2 := data_int; -- register the incoming data on the third falling edge
                    count := 0;
                    shift_data := '1'; -- third rising edge
                end if;
                if shift_data = '1' then
                    count := count + 1;
                    for i in 0 to number_of_channels-1 loop
                        -- Data in MSB gets shifted out first.
                        -- NB: This happens 1/2clk cycle later for APEXII (MSB
                        -- only) when center_align_msb is ON.
                        if ((i = number_of_channels-1) and (count = 1) and ((intended_device_family = "APEXII" or intended_device_family = "APEX II")) and
                            (center_align_msb = "ON")) then
                            tx_out(i) <= tx_in2((i+1)*deserialization_factor - count) after (inclock_period/(deserialization_factor*2) * 1 ps);
                        else
                            tx_out(i) <= tx_in2((i+1)*deserialization_factor - count);
                        end if;
                    end loop;
                end if;
            end if;
            -- Update asymmetrical outclock for MERCURY for odd deserial
            -- factors.
            if deserialization_factor rem 2 /= 0 then
                if tx_clock1_int'event and tx_clock1_int = '1' then
                    tx_mercury_core_clock <= tx_clock1_int;
                end if;
                if tx_clock0_int'event and (tx_clock0_int = '1') and (posedge_count=(deserialization_factor+1)/2+1) then
                    tx_mercury_core_clock <= NOT tx_mercury_core_clock;
                end if;
            end if;
        else
            if tx_clock1_int'event and tx_clock1_int = '1' then
                tx_out <= tx_in;
            end if;
        end if;
    end process;

    sync: process(tx_rgd_clk)
    begin
        if deserialization_factor > 1 then
            if (tx_rgd_clk = '0') and  ((deserialization_factor < 3) or (deserialization_factor > 6)) then
                tx_hold_rgd <= tx_in_int;
            elsif tx_rgd_clk = '1' then
                tx_rgd <= tx_in;
            end if;
        end if;
    end process sync;

end behavior;
