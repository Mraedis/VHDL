##############################################################################################################################
###    Author:    Joren Guillaume
###
###    Changelog
###    2014.08.11 - Formatting, filled in code for functions
###    2014.08.17 - More filling in code
###    2014.08.21 - Added logwrite()
###    2014.08.25 - Editing functions, use of global variables
###    2014.09.16 - Bugfixing, overhaul, extended log function
###    2014.09.17 - Overhaul of setup() and logwrite(), filling of parse_source() to actually parse a source
###                 Program now saves every sourcefile seperately, every test in it's unique architecture
###                 Files are executed seperately now, output is captured
###    2014.09.23 - Started expanding format()
###    2014.09.25 - Expanded format() further, preliminary version finished. Started on xmlwrite()
###    2014.09.26 - Prelimenary working version. Produces actual xml report with (all passed) test reports
###    2014.10.01 - Introduced format and xmlwrite from previous version "test_parser".
###                 Testbenches are now required to use the vhdlUnit.vhd library for reporting (reportback() function)
###                 vhdlUnit.vhd is expected to be in the same directory as testbench_parser.py
###                 Small error fixing in handling multiple testbenches named the same (or actually being the same)
###    2014.10.07 - Small all-around changes, including the commandline options, file handling, file locating et
###
##############################################################################################################################
###
###    ToDo:
###        - Use of Try/Except for error catching
###        - Expand commentary to full descriptions
###        - (Actually detect whether tests pass!!)
###
############  FUNCTIONS  #####################################################################################################
#  setup()               ## Set up files, set global vars, process commandline arguments with argparse
#  logwrite()            ## Write things to the logbook, errors, completed jobs etc.
#  get_path(path)        ## Return absolute path if not already absolute path
#  setup_parser()        ## Prepare the parser to accept correct commandline arguments
#  make_tempdir()        ## Create the temporary working directory
#  parse_source()        ## Grab sourcefile, extract needed code, arrange functions & procedures
#  test_format()         ## Arrange found functions & procedures in their own executable files
#  parse_tests           ## Grab processed source/files, execute & capture output
#  format()             ## Grab output, format output
#  xmlwrite()           ## Grab processed output, convert to JUnit compatible XML file
#  get_time()            ## Extract the passed time from a ModelSim time notice
#  cleanup()             ## Remove temporary files & directories
##############    END    #####################################################################################################

import sys
import os
import shutil
import tempfile
import string
import random
import argparse
import time
from junit_xml import TestSuite, TestCase

## Sets up (temporary) files, sets global vars, processes cmdline arguments with argparse
##
# args is the arguments part of an argparse.ArgumentParser
#     The possible expected arguments are c,l,f,d,s; for further explanation see setup_parser()
##
# Tempname_t is the random name used throughout the script, it has no constraints for being different
##
# Foldername_t is the path to the folder that will contain all permanent logs and results
##
def setup(args, tempname_t=None, foldername_t=None):
    if (tempname_t == None):                                                        # If tempname was not given with the function, use global name
        tempname_t = tempname
    if (foldername_t == None):                                                      # If foldername was not given with the function, use global name
        foldername_t = foldername
    
    if not args.cmdline:
        folderpath_t = os.path.join(os.getcwd(), 'VHDL_TDD_Parser')
        if (not os.path.isdir(folderpath_t)):                                           # Make the folder that contains all non-deleted files from all time, if non-existant
            os.makedirs(folderpath_t)
            logbuffer('n','Created working directory \'VHDL_TDD_Parser\' for the first time.')
        outputdir = time.strftime('%Y.%m.%d - %H.%M - ') + tempname_t
        foldername_t = os.path.join(folderpath_t, outputdir)
        if (os.path.isdir(outputdir)):                                                  # Make the folder for the current run    
            logbuffer('n','Output directory \'' + tempname_t + '\' already existed.')
        else:
            os.makedirs(foldername_t)
    else:
        folderpath_t = os.path.join(os.environ['USERPROFILE'], 'VHDL_TDD_Parser')
        if (not os.path.isdir(folderpath_t)):                                           # Make the folder that contains all non-deleted files from all time, if non-existant
            os.makedirs(folderpath_t)
            logbuffer('n','Created working directory \'VHDL_TDD_Parser\' for the first time.')
        outputdir = time.strftime('%Y.%m.%d - %H.%M - ') + tempname_t
        foldername_t = os.path.join(folderpath_t, outputdir)
        if (os.path.isdir(outputdir)):                                                  # Make the folder for the current run    
            logbuffer('n','Output directory \'' + tempname_t + '\' already existed.')
        else:
            os.makedirs(foldername_t)
    return foldername_t

## Stores log reports in a buffer until the logfile is made
##
# level is the criticality of the logwrite. It can be notice, warning, severe, critical or unknown
#     Some levels are not (yet) used and may be obsolete
##
# message is the actual logmessage, with no timestamps or level
##
# tempname_t is the unique identifier for the testbench run
##
# args_t is the argument parser object generated in setup_parser()
##
def logbuffer(level='u',message ='No message given.', tempname_t=None, args_t=None):
    global logs_buffer                                                                              # Uses the global variables logs_buffer and logstarted
    if logstarted:                                                                                  # This function cannot work with no such variables
        logwrite(level, message, tempname_t, args_t)
    else:
        logs_buffer.append([level, message, time.strftime("%Y.%m.%d - %H:%M:%S") ])

## Writes things to the logbook: errors, completed jobs etc.
## Logwrite is fully explained above near logbuffer
## Logwrite cannot function without global variables logstarted and logs_buffer
def logwrite(level='u',message ='No message given.', tempname_t=None, args_t=None, foldername_t=None):
    if (tempname_t == None):
        tempname_t = tempname
    if (args_t == None):
        args_t = args
    if (foldername_t == None):
        foldername_t = foldername
    dest = ''
    if (args_t.log):
        dest = get_path(args_t.log)
    else:
        dest = foldername_t
        
    #FORMAT: Time stamp \tab Level \tab Message
    levels = {'n': 'notice', 'w' : 'warning', 's': 'severe', 'c': 'critical', 'u' : 'unknown'};
    logfile = open(dest + os.sep + 'logfile.txt', 'a+')
    
    logfile.seek(0)
    first_char = logfile.read(1)
    if first_char != '#':                                                                           # Check for any file contents and create header if none
        logfile.write('##################################################################\n')
        logfile.write('######################                      ######################\n')
        logfile.write('###################### LOGFILE FOR ' + tempname_t + ' ######################\n')
        logfile.write('######################                      ######################\n')
        logfile.write('##################################################################\n\n')
        logfile.write('YYYY.MM.DD - hh:mm:ss  -   LEVEL    -  MESSAGE\n')
    else:
        logfile.seek(2)
        
    global logstarted
    if (not logstarted):
        logstarted = True
    # if logs_buffer: -> Needed for error logging when the logfile doesn't work
    # Adding the check here prevents eternal looping of logging errors writing to log
        for command in logs_buffer:
            logfile.write(command[2] + '  -  ' + (levels[command[0]] + '\t').expandtabs(5) + '-  ' + command[1] + '\n')
                                                                                                    # Print the message with information on time and level
    logfile.write(time.strftime('%Y.%m.%d - %H:%M:%S') + '  -  ' + (levels[level] + '\t').expandtabs(5) + '-  ' + message + '\n')
    logfile.close()

## Returns absolute path if not already absolute path, otherwise return argument unchanged
def get_path(path):
    if (not os.path.isabs(path)):
        path = os.path.abspath(path)
    return path

## Prepares the parser to accept correct commandline arguments
def setup_parser():
    parser = argparse.ArgumentParser(  description='VHDL testbench to TDD parser'
                                     , formatter_class=argparse.ArgumentDefaultsHelpFormatter)
        # The -c/--cmd argument is to specify the script being called from the commandline.
        # The flag is stored in 'cmd', default value is 'False'
    parser.add_argument('-c', '--cmd'
                         , help='specifies output to be displayed on the command line'
                         , action = 'store_true', dest='cmdline', default=False)
        # The -d/--dest argument is to specify a custom folderpath for the log.
        # The flag is stored in 'log', default value is 'None'
    parser.add_argument('-d', '--dest'
                        , help='specifies a custom path for the log'
                        , action = 'store', dest='log', default=None)
        # The -f/--file argument is to specify that the -l/--list is a file/files containing a list of .vhd files.
        # The flag is stored in 'file', default value is 'False'
    parser.add_argument('-f', '--file'
                        , help='specifies -l/--list is a file with a list'
                        , action = 'store_true', default=False) 
        # The -l/--list argument is to specify the list of .vhd files to be processed.
        # The flag is stored in 'list', no default value considering there NEEDS to be at least 1 .vhd file
    parser.add_argument('-l', '--list'
                        , help='specifies the list of .vhd files to be processed -- note: ONLY .vhd files'
                        , action = 'store'
                        , nargs = '+', required=True, metavar='path')
        # The -m/--method argument is to specify which method of writing was used in testbench creation.
        # The flag is stored in 'list', no default value considering there NEEDS to be at least 1 .vhd file
    parser.add_argument('-m', '--method'
                        , help='specifies what method was used to write the testbench. '
                        , action = 'store'
                        , default='line', choices=['line', 'startstop', 'partitioned'])
        # The -p/--precompiled argument is to locate a precompiled library that will be needed during simulation.
        # The flag is stored in 'list', no default value considering there NEEDS to be at least 1 .vhd file
    parser.add_argument('-p', '--precompiled'
                        , help='specifies the location of the precompiled dependencies, requires a full path.'
                        , action = 'store'
                        , dest='prepath', metavar='path', default=None)
        # The -r/--runops argument is to specify custom commands during simulation.
        # The flag is stored in 'runops', Default value: -do "run all; exit"
    parser.add_argument('-r', '--runops'
                        , help='specifies custom arguments for simulation start, default: -do "run -all;exit"'
                        , action = 'store'
                        , dest='runops', metavar='string', default='-do "run -all;exit"')
        # The -v/--version argument is to compile in a different VHDL version other than 2008.
        # The flag is stored in 'version', default value is 2008
    parser.add_argument('-v', '--version'
                        , help='specifies a different VHDL version, default is 2008'
                        , action = 'store'
                        , default='2008' , metavar='version')
#        # The -s/--script argument is to specify the testbench(es) use the scriptstart/scriptend comments
#        # The flag is stored in 'script', default value is 'False'
#    parser.add_argument('-s', '--script'
#                        , help='specifies the testbenches use the ScriptStart and ScriptEnd comments'
#                        , action = 'store_true', default=False)
    
    return parser

## Creates the temporary working directory
def make_tempdir():
    char_set = string.ascii_uppercase + string.digits           # Generates the character set used: letters & numbers, lower & uppercase
    tempname_t = ''.join(random.sample(char_set*8,8))           # Save temporary name for deletion later
    tempdir_t = tempfile.gettempdir() + os.sep + tempname_t
    
    while(os.path.isdir(tempdir_t)):                            # Make sure directory doesn't exist yet
         tempname_t = ''.join(random.sample(char_set*8,8))
         tempdir_t = tempfile.gettempdir() + os.sep + tempname_t
    
    os.makedirs(tempdir_t)                                      # Create temporary working directory
    return tempdir_t, tempname_t

    
## grabs sourcefile, extracts needed code, arranges functions & procedures
def parse_source(path, method=None):
    
    if (method==None):
        method=args.method
    archstart = False
    
    try:
        source = open(path)                                     # Open the file
    except:
        logbuffer('c','Could not open file ' + str(path) + ', ignoring.')
        return ['ERROR', path]
    line = source.readline()
    line_lower = line.lower()                                   # VHDL is case sensitive, but using lowercase is easier to target identifiers such as 'architecture', 'function' etc.
    words = line_lower.split(' ')
    header, archheader, body, footer = '','','',''
    entname, archname = '',''
    templist = []
    tests = 0                                                   # Count the tests in the body
    
    while not archstart:                                        # Find the entity and architecture name, used in executing the testbenches
        if (line == ''):
                logwrite('c','Reached end of source file without an architecture in file ' + str(path) + '.')
                return ['ERROR', path]
        if words:    
            if (words[0] == 'entity'):
                entname = words[1]
            if (words[0] == 'architecture'):
                archstart = True
                archname = words[1]
                archheader += line
            else:
                header += line
        line = source.readline()
        line_lower = line.lower().strip()
        words = line_lower.split(' ')
    
    if (method=='startstop'):                                                   # Check whether scriptstart/end is being used with 'script'   
        scriptstart, scriptend = False, False
        while (not scriptstart):
            if (line == ''):
                logwrite('c','Reached end of source file without encountering \'--scriptstart\' despite -s flag specified in file ' + str(path) + '.')
                return ['ERROR', path]
            archheader += line
            if (line_lower.strip() == '--scriptstart'):                         # Find the start of the script, add all lines before the start to the header
                scriptstart = True
            line = source.readline()
            line_lower = line.lower()
        
        while (not scriptend):                                                  # All lines between scriptstart and scriptend are placed in the body
            if (line == ''):
                logwrite('c','Reached end of source file without encountering \'--scriptstop\' despite -s flag specified in file ' + str(path) + '.')
                return ['ERROR', path]
            elif (line_lower.strip() == '--scriptend'):
                scriptend = True
                footer += line
            else:
                body += line
            line = source.readline()
            line_lower = line.lower()
            
        footer += line                                                          # All remaining lines are placed in the footer
        for line in source:
            footer += line
        source.close()
        
    elif (method =='line'):
        archbody, archend = False, False
        beginwords = ['function','procedure','for','while','if','process','component']
        depth = 0
        
        while not archbody:
            if (line == ''):
                logwrite('c','Reached end of source file with incorrect architecture body (found no begin) in file ' + str(path) + '.')
                return ['ERROR', path]
            if words:
                if depth == 0 and words[0] == 'begin':                          # If the 'begin' is the begin of the architecture body
                    archbody = True
                elif words[0] in beginwords:
                    depth += 1
                elif words[0] == 'end':                         
                    depth -= 1
            archheader += line
            line = source.readline()
            line_lower = line.lower().strip()
            words = line_lower.split(' ')
        
        depth = 0
        process_start, process_begin = False, False
        
        while not archend:
            if (line == ''):
                logwrite('c','Reached end of source file with incorrect architecture body (found no end) in file ' + str(path) + '.')
                return ['ERROR', path]
            if not process_begin:
                if words:
                    if not process_start:
                        if 'process' in words:                                  # Finding the process that envelopes the functions
                            process_start = True
                    else:
                        if depth == 0 and words[0] == 'begin':                  # If the 'begin' is the begin of the process
                            process_begin = True
                        elif words[0] in beginwords:                            # Else, check for 'depth' changing words
                            depth += 1
                        elif words[0] == 'end':                         
                            depth -= 1
                archheader += line
            else:
                if words:
                    if (line_lower in ['end architecture;', 'end ' + archname + ';','end architecture' + archname + ';']):
                        archend = True
                        footer += line
                    elif (words[0] in ['end','wait;']):                         # 'wait;' and 'end process;' don't count as tests
                        footer += line
                    else:
                        body += line
            line = source.readline()
            line_lower = line.lower().strip()
            words = line_lower.split(' ')
            
        for line in source:                                                     # All remaining lines, if any, are placed in the footer
            footer += line
        source.close()
        
    elif(method=='partitioned'):
        in_test = False
        temp_lines = ''
        
        while (line != ''):
            if (in_test):
                if (len(line) > 4):
                    if (line_lower[0:5] == '--end'):                            # Find the start of the script, add all lines before the start to the header
                        in_test = False
                        templist.append(temp_lines)
                        temp_lines = ''
                    else:
                        temp_lines += line
                else:
                    temp_lines += line
            elif (len(line) > 5):
                if (line_lower[0:6] == '--test'):
                    in_test = True
                    tests += 1
                else:
                    if (tests == 0):
                        archheader += line
                    else:
                        temp_lines += line
                
            line = source.readline()
            line_lower = line.lower().strip()
            words = line_lower.split(' ')
            
        if (tests == 0):
            logwrite('c','Reached end of source file prematurely in file ' + str(path) + '.')
        footer = temp_lines

        source.close()
    
    if (method != 'partitioned'):
        bodylines = body.split('\n')
        for line in bodylines:                                                  # Lines in the body are all single tests/functions (or should be)
            if line.strip():
                tests += 1
                templist.append(line)                                           # [META] Improving detection here can be a major asset
    else:
        tests = len(templist)
    if (tests == 1):
        logwrite('n','Successfully parsed ' + entname + '.' + archname + ' with ' + str(tests) + ' testsuite found.')
    else:
        logwrite('n','Successfully parsed ' + entname + '.' + archname + ' with ' + str(tests) + ' testsuites found.')
    return [archname, entname, header, archheader, footer, templist]
    
## arranges found functions & procedures in their own executable files
def test_format(parsedlist_t = None, tempdir_t = None):
    if parsedlist_t == None:
        parsedlist_t = parsedlist
    if (tempdir_t == None):
        tempdir_t = tempdir
    ## Format: parsedlist is a tuple of tuples: [archname, entname, header, archheader, footer, list_of_tests]
        
    testcount = {}
    
    da = 0                                                                      # Duplicate Avoider: Multiple instances of the same tb otherwise go wrong here
    for parsedfile in parsedlist_t:                                             # Assign correct variables for making the files
        if parsedfile[0] != 'ERROR':
            archname = parsedfile[0]
            entname = parsedfile[1]
            header = parsedfile[2]
            archheader = parsedfile[3]
            footer = parsedfile[4]
            tests = parsedfile[5]
            
            localcount = 0
            localname = entname + '.' + archname + '.' + str(da) + '.vhd'       # Name for use in dictionary and testing
            testfile_path = tempdir_t + os.sep + localname
            testfile = open(testfile_path,'w+')
            testfile.write('library TDD;\nuse TDD.vhdlUnit.all;\n')
            testfile.write(header)
            
            for test in tests:                                                  # Place every test in the same file, under a different architecture
                testfile.write(archheader.replace(archname, archname + str(localcount)))
                #testfile.write('assert false report \"test started\" severity note;\n')
                testfile.write(test + '\n')
                #testfile.write('assert false report \"test ended\" severity note;\n')
                testfile.write(footer.replace(archname, archname + str(localcount)) + '\n\n')
                localcount += 1
            testfile.close()
            logwrite('n','Parsed test with name: ' + localname)
            testcount[localname] = localcount                                   # Keep a dictionary of the tests in each file
        else:
            logwrite('n','Ignoring test ' + str(len(testcount)) + ', file: ' + str(parsedfile[1]))
        da += 1
    return testcount
    
## grabs processed source/files, executes & captures output
## testcount_t is a dictionary of key: entityname.architecturename.vhd, with value the number of tests inside
def parse_tests(testcount_t = None, tempdir_t = None, foldername_t = None, args_t = None, currentdir_t = None):
    if (testcount_t == None):                                                   # Use global values if arguments not filled
        testcount_t = testcount
    if (tempdir_t == None):
        tempdir_t = tempdir
    if (foldername_t == None):
        foldername_t = foldername
    if (args_t == None):
        args_t = args
    if (currentdir_t == None):
        currentdir_t = currentdir
        
    args_t.prepath = get_path(args_t.prepath)
    
    os.chdir(tempdir_t)                                                         # Change working directory to the temporary folder
    testcount = 0
    
    
    if (args_t.prepath != None):
        source = open(args_t.prepath,'r+')
        for line in source:
            os.system(line.strip())
        #shutil.copytree(get_path(prepath), tempdir_t + os.sep + prepath.split(os.sep)[-1])
    os.system('vlib TDD')
    os.system('vcom -' + args_t.version + ' -quiet -work TDD ' + currentdir_t + os.sep + 'vhdlUnit.vhd')
    for key,value in testcount_t.iteritems():                                   # Run across the dictionary, executing every test in each file
                                                                                # Make use of the vhdlUnit 'reportback' function
        commands = 'vlib work' + str(testcount) +'\n' + 'vcom -' + args_t.version + ' -quiet -work work' + str(testcount) + ' ' + key
        
        for line in commands.split('\n'):                                       # Form a work directory per file
            os.system(line)
        
        words = str(key).split('.')
        entname = words[0]                                                      # Get entity and architecture name from the key
        archname = words[1]
        
        outputname = words[0] + '.' + words[1] + '.' + words[2]
        outputfile = open(os.getcwd() + os.sep + outputname + '_cmd_output.txt','w+')
        logwrite('n','Starting execution of tests in ' + str(key))
        for test in range(0, value):
            testname = (str(testcount) + '.' + entname + '(' + archname + str(test) + ')" '
                               + args_t.runops.replace(entname, 'work' + str(testcount) + '.' + entname + '(' + archname + str(test)))
            logwrite('n','Executing in: work' + testname)
            readcmd = os.popen('vsim -c "work' + testname).read()
            outputfile.write(readcmd)
        outputfile.close()
        logwrite('n','Finished execution of tests in ' + str(key))
        shutil.copy(os.getcwd() + os.sep + outputname + '_cmd_output.txt', foldername_t)
        testcount += 1
    os.chdir(currentdir_t)
    
def format(tempdir_t=None, foldername_t=None):
    if (tempdir_t == None):
        tempdir_t = tempdir
    if (foldername_t == None):
        foldername_t = foldername
    testname = tempdir_t.split(os.sep)[-1]
    currentdir = os.getcwd()
    os.chdir(tempdir_t)
    
    failedtests, passedtests, othernotes, totaltests = 0, 0, 0, 0
    failedlines, passedlines, otherlines, everyline = [], [], [], []
    
    for file in os.listdir(tempdir_t):                              # Get every file that is a commandline output file
        if file.endswith('_cmd_output.txt'):
            failedtests_l, passedtests_l, othernotes_l, totaltests_l = 0, 0, 0, 0
            failedlines_l, passedlines_l, otherlines_l, everyline_l = '', '', '', ''
            source = open(file,'r+')
      
            ReadNote = False
            lastline = ''
            for line in source:
                words = line.split(' ')
                if ReadNote == True:
                    if (len(words) > 5):
                        to_add = ' - time: ' + words[5] + ' ' + words[6] + '\n'
                    else:
                        to_add = line
                    if lastline == 'success':
                        passedlines_l += to_add
                    elif lastline == 'failed':
                        failedlines_l += to_add
                    elif lastline == 'other':
                        otherlines_l += to_add
                    everyline_l += to_add
                    ReadNote = False
                    
                elif (len(words) > 2):
                    if(words[2] == 'Note:'):
                        ReadNote = True
                        if(len(words) > 4):
                            if(words[4] == 'failed\tname:'):
                                failedtests_l += 1
                                totaltests_l += 1
                                failedlines_l += str(failedtests_l).zfill(4) + ' - ' + line[11:-1]
                                lastline = 'failed'
                            elif(words[4] == 'success\tname:'):
                                passedtests_l += 1
                                totaltests_l += 1
                                passedlines_l += str(passedtests_l).zfill(4) + ' - ' + line[11:-1].split('-')[0]
                                lastline = 'success'
                            else:
                                othernotes_l += 1
                                totaltests_l += 1
                                otherlines_l += str(othernotes_l).zfill(4) + ' - ' + line[11:-1]
                                lastline = 'other'
                        else:
                            othernotes_l += 1
                            totaltests_l += 1
                            otherlines_l += str(othernotes_l).zfill(4) + ' - ' + line[11:-1]
                            lastline = 'other'
                        everyline_l += str(totaltests_l).zfill(4) + ' - ' + line[11:-1]
                        
            if ReadNote == True:
                to_add = ' - time: ' + words[5] + ' ' + words[6]
                if lastline == 'succes':
                    passedlines_l += to_add[1:-1]
                elif lastline == 'failed':
                    failedlines_l += to_add
                elif lastline == 'other':
                    otherlines_l += to_add
                everyline_l += to_add
            
            failedtests += failedtests_l
            passedtests += passedtests_l
            othernotes += othernotes_l
            totaltests += totaltests_l
            failedlines.append(failedlines_l)
            passedlines.append(passedlines_l)
            otherlines.append(otherlines_l)
            everyline.append(everyline_l)
            
            source.close()
            
                #Write plain .txt file with testresults
    targetpath = foldername_t + os.sep + testname + '_testresults.txt'
    targetfile = open(targetpath,'w+')
    targetfile.write('total tests: '      + str(totaltests)
                     + '\ntests passed: ' + str(passedtests)
                     +'\ntests failed: '  + str(failedtests)
                     + '\nother notes: '  + str(othernotes))
    output_failed, output_passed, output_other, output_every = '', '', '', ''
    for x in range (0, len(failedlines)):
        output_failed += failedlines[x]
        output_passed += passedlines[x]
        output_other += otherlines[x]
        output_every += everyline[x]
    targetfile.write('\n\n\nPassed tests reports:\n' + output_passed
                     + '\n\nFailed tests reports:\n' + output_failed
                     + '\n\nOther notes:\n'          + output_other
                     + '\n\n\nAll test results:\n'   + output_every)
    
    print( 'total tests: '      + str(totaltests)
           + '\ntests passed: ' + str(passedtests)
           +'\ntests failed: '  + str(failedtests)
           + '\nother notes: '  + str(othernotes))
    #Print left out - optional command line output
    
    targetfile.close()
    os.chdir(currentdir)
    return everyline
    
#Write .xml file in JUnit format (For use in Eclipe)
#Uses JUnit-xml package by Brian Beyer
#source at https://github.com/kyrus/python-junit-xml
#Requires setuptools to install at https://pypi.python.org/pypi/setuptools
def xmlwrite(everyline, foldername_t=None):
    if (foldername_t == None):
        foldername_t = foldername
        
    testname = foldername_t.split(os.sep)[-1]
        
    xmltargetpath = foldername_t + os.sep + foldername_t.split(os.sep)[-1] + '_testresults.xml'
    xmltargetfile = open(xmltargetpath, 'w+')
    
    test_suites = []
    suite_count = 1
    for lines in everyline:
        test_cases = []
        for line in lines.split('\n'):
            words = line.split(' ')
            if line.find('success') != -1:
                time_taken = get_time(line.split('-')[-1][7:])
                name = words[0] + ' - ' + line.split('-')[1].split('name:')[1].strip()
                test_cases.append(TestCase(name, testname, time_taken, None))
            elif line.find('failed') != -1:
                time_taken = get_time(line.split('-')[-1][7:])
                name = words[0] + ' - ' + line.split('-')[1].split('name:')[1].strip()
                message = ("-".join(line.split(' - ')[2:-1])).strip()
                tc = TestCase(name, testname, time_taken, None)
                tc.add_failure_info(None, message)
                test_cases.append(tc)
        test_suites.append(TestSuite('TestSuite number: ' + str(suite_count), test_cases))
        suite_count += 1
        
    xmltargetfile.write(TestSuite.to_xml_string(test_suites))
    xmltargetfile.close()
    
# extracts the passed time from a modelsim time notice
def get_time(timestring):
    words = timestring.split(' ')
    new_time = float(words[0])
    multiplier = 1
    if words[1] == 'ps':
        multiplier = 10**(-12)
    elif words[1] == 'ns':
        multiplier = 10**(-9)
    elif words[1] == 'us':
        multiplier = 10**(-6)
    elif words[1] == 'ms':
        multiplier = 10**(-3)
    return new_time * float(multiplier)
    
## removes temporary files & directories
def cleanup():
    logwrite('n','Cleanup started at ' + str(time.time()))
    shutil.rmtree(tempdir)


##########################################
######## MAIN PROGRAM STARTS HERE ########
##########################################

#Allows the code to be used as a module
if __name__ == "__main__":
    
    parsedlist, logs_buffer = [], []                                                # A buffer is needed for loglines from before the logdirectory was created
    logstarted = False
    tempdir, tempname, foldername = '', '', ''                                      # Future variables that will be used througout the script
    parser = None                                                                   # Assigning name for the parser
    
    systemtime = time.time()                                                        # Marks the starting time of the script
    logbuffer('n','Saving current working directory')
    currentdir = os.getcwd()                                                        # CWD is changed during simulation
    
    logbuffer('n','Setting up argument parser')
    parser = setup_parser()                                                         # Creates an argument parser for the commandline arguments
    args, unknown = parser.parse_known_args()                                       # Parses commandline arguments, stores unknown arguments in 'unknown'
    
    logbuffer('n','Creating temporary directories')
    tempdir, tempname = make_tempdir()                                              # Creates a unique (name for the) temporary directory in the systems temp dir
    foldername = setup(args, tempname)                                              # Grab all functions and procedures to be processed, returns output folder
    
    logwrite('n','Started script at ' + str(systemtime))
    if (unknown):                                                                   # If there are unknown arguments, write error to log
        logwrite('w', 'Found unusuable arguments: ' + ', '.join(unknown))

    if (args.file):                                                                 # If the files to be processed are in a file
        logwrite('n','Beginning source file parsing')
        for file in args.list:                                                      # Get all filenames
            listfile = open(get_path(file))                                         # Open the files one by one
            sourcelist = [line.strip() for line in open(listfile)]                  # Get all .vhd files listed within
            for line in sourcelist:                                                 # Parse each .vhd file
                parsedlist.append(parse_source(get_path(line), args.method))
    else:                                                                           # If arguments are not lists, they are the files themselves
        logwrite('n','Beginning source file parsing')
        for file in args.list:
            parsedlist.append(parse_source(get_path(file), args.method))
            
    logwrite('n','Beginning test formatting')
    testcount = test_format(parsedlist)                                             # Get list of number of tests
    if args.cmdline:                                                                # Location of vhdlUnit.vhd may vary on how the script is called
        parse_tests(testcount, tempdir, foldername, args)
    else:
        parse_tests(testcount, tempdir, foldername, args, os.path.dirname(os.path.realpath(__file__)))
    
    logwrite('n','Beginning result formatting')
    testresults = format(tempdir)                                                  # Format the testresults to humanly readable words
    xmlwrite(testresults)                                                          # Format the above format into a JUnit-XML format
    
    logwrite('n','Cleaning up files')
    #cleanup()                                                                       # Remove the temporary working directory
    logwrite('n','Stopped script at ' + str(time.time()))                           # Note the time of ending