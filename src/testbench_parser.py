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
###
##############################################################################################################################
###
###    ToDo:
###        - Use of Try/Except for error catching
###
############  FUNCTIONS  #####################################################################################################
#  setup()               ## Sets up (temporary) files, sets global vars, processes commandline arguments with argparse
#  logwrite()            ## Writes things to the logbook, errors, completed jobs etc.
#  get_path(path)        ## Returns absolute path if not already absolute path, otherwise return argument unchanged
#  setup_parser()        ## Prepares the parser to accept correct commandline arguments
#  make_tempdir()        ## Creates the temporary working directory
#  parse_source()        ## grabs sourcefile, extracts needed code, arranges functions & procedures
#  test_format()         ## arranges found functions & procedures in their own executable files
#  parse_tests           ## grabs processed source/files, executes & captures output
#  format()              ## grabs output, formats output
#  xmlwrite()            ## grabs processed output, converts to JUnit compatible XML file
#  cleanup()             ## removes temporary files & directories
##############    END    #####################################################################################################

import sys
import os
import shutil
import tempfile
import string
import random
import argparse
import time

## Sets up (temporary) files, sets global vars, processes cmdline arguments with argparse
def setup(args, tempname_t=None, folderpath_t=None, foldername_t=None):
    if (tempname_t == None):                                                        # If tempname was not given with the function, use global name
        tempname_t = tempname
    if (folderpath_t == None):                                                      # If folderpath was not given with the function, use global name
        folderpath_t = folderpath
    if (foldername_t == None):                                                      # If foldername was not given with the function, use global name
        foldername_t = foldername
        
    folderpath_t = os.path.join(os.environ['USERPROFILE'], 'VHDL_TDD_Parser')
    if (not os.path.isdir(folderpath_t)):                                           # Make the folder that contains all non-deleted files from all time, if non-existant
        os.makedirs(folderpath_t)
        logbuffer('n','Created working directory \'VHDL_TDD_Parser\' for the first time.')
    outputdir = time.strftime('%Y.%M.%d - %H.%M - ') + tempname_t
    foldername_t = os.path.join(folderpath_t, outputdir)
    if (os.path.isdir(outputdir)):                                                  # Make the folder for the current run    
        logbuffer('n','Output directory \'' + tempname_t + '\' already existed.')
    else:
        os.makedirs(foldername_t)
    return foldername_t

## Stores log reports in a buffer until the logfile is made
def logbuffer(level='n',message ='No message given.', tempname_t=None, args_t=None):
    global logs_buffer
    if logstarted:
        logwrite(level, message, tempname_t, args_t)
    else:
        logs_buffer.append([level, message, time.strftime("%Y.%M.%d - %H:%M:%S"), ])

## Writes things to the logbook: errors, completed jobs etc.
def logwrite(level='n',message ='No message given.', tempname_t=None, args_t=None):
    if (tempname_t == None):
        tempname_t = tempname
    if (args_t == None):
        args_t = args
    dest = ''
    if (args_t.log):
        dest = get_path(args_t.log)
    else:
        dest = foldername
        
    #FORMAT: Timestamp \tab Level \tab Message
    levels = {'n': 'notice', 'w' : 'warning', 's': 'severe', 'c': 'critical', 'u' : 'unknown'};
    logfile = open(dest + os.sep + 'logfile.txt', 'a+')
    
    logfile.seek(0)
    first_char = logfile.read(1)
    if first_char != '#':                                                                           # Check for any file contents and create header if none
        logfile.write('##################################################################\n')
        logfile.write('######################                      ######################\n')
        logfile.write('###################### LOGFILE FOR ' + tempname + ' ######################\n')
        logfile.write('######################                      ######################\n')
        logfile.write('##################################################################\n\n')
        logfile.write('YYYY.MM.DD - HH:MM:SS  -   LEVEL    -  MESSAGE\n')
    else:
        logfile.seek(2)
        
    global logstarted
    if (not logstarted):
        logstarted = True
        for command in logs_buffer:
            logfile.write(command[2] + '  -  ' + (levels[command[0]] + '\t').expandtabs(5) + '-  ' + command[1] + '\n')
                                                                                                    # Print the message with information on time and level
    logfile.write(time.strftime('%Y.%M.%d - %H:%M:%S') + '  -  ' + (levels[level] + '\t').expandtabs(5) + '-  ' + message + '\n')
    logfile.close()

## Returns absolute path if not already absolute path, otherwise return argument unchanged
def get_path(path):
    if (not os.path.isabs(path)):
        path = os.path.abspath(path)
    return path

## Prepares the parser to accept correct commandline arguments
def setup_parser():
    parser = argparse.ArgumentParser(version='0.1'
                                     , description='VHDL testbench to TDD parser'
                                     , formatter_class=argparse.ArgumentDefaultsHelpFormatter)
        # The -c/--cmd argument is to specify the script being called by an automated program.
        # The flag is stored in 'cmd', default value is 'False'
    parser.add_argument('-c', '--cmd'
                         , help='specifies script being called from commandline, not automated'
                         , action = 'store_true', default=False)
        # The -l/--list argument is to specify the list of .vhd files to be processed.
        # The flag is stored in 'list', no default value considering there NEEDS to be at least 1 .vhd file
    parser.add_argument('-l', '--list'
                        , help='specifies the list of .vhd files to be processed -- note: ONLY .vhd files'
                        , action = 'store'
                        , nargs = '+', required=True, metavar='path')
        # The -f/--file argument is to specify that the -l/--list is a file/files containing a list of .vhd files.
        # The flag is stored in 'file', default value is 'False'
    parser.add_argument('-f', '--file'
                        , help='specifies -l/--list is a file with a list'
                        , action = 'store_true', default=False)
        # The -d/--dest argument is to specify a custom folderpath for the log.
        # The flag is stored in 'log', default value is 'None'
    parser.add_argument('-d', '--dest'
                        , help='specifies a custom folder for the log'
                        , action = 'store', dest='log', default=None)
        # The -s/--script argument is to specify the testbench(es) use the scriptstart/scriptend comments
        # The flag is stored in 'script', default value is 'False'
    parser.add_argument('-s', '--script'
                        , help='specifies the testbenches use the ScriptStart and ScriptEnd comments'
                        , action = 'store_true', default=False)
    
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
def parse_source(path, script=False):
    
    scriptstart, scriptend = False, False
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
    
    if (script):                                                # Check whether scriptstart/end is being used with 'script'
        while (not scriptstart):
            if (line == ''):
                logwrite('c','Reached end of source file without encountering \'--scriptstart\' despite -s flag specified in file ' + str(path) + '.')
                return ['ERROR', path]
            archheader += line
            if (line_lower.strip() == '--scriptstart'):         # Find the start of the script, add all lines before the start to the header
                scriptstart = True
            line = source.readline()
            line_lower = line.lower()
        
        while (not scriptend):                                  # All lines between scriptstart and scriptend are placed in the body
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
            
        footer += line                                          # All remaining lines are placed in the footer
        for line in source:
            footer += line
        source.close()
    else:
        archbody, archend = False, False
        beginwords = ['function','procedure','for','while','if','process','component',]
        depth = 0
        
        while not archbody:
            if (line == ''):
                logwrite('c','Reached end of source file with incorrect architecture body (found no begin) in file ' + str(path) + '.')
                return ['ERROR', path]
            if words:
                if depth == 0 and words[0] == 'begin':          # If the 'begin' is the begin of the architecture body
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
                        if 'process' in words:                  # Finding the process that envelopes the functions
                            process_start = True
                    else:
                        if depth == 0 and words[0] == 'begin':  # If the 'begin' is the begin of the process
                            process_begin = True
                        elif words[0] in beginwords:            # Else, check for 'depth' changing words
                            depth += 1
                        elif words[0] == 'end':                         
                            depth -= 1
                archheader += line
            else:
                if words:
                    if (line_lower in ['end architecture;', 'end ' + archname + ';','end architecture' + archname + ';']):
                        archend = True
                        footer += line
                    elif (words[0] in ['end','wait;']):         # 'wait;' and 'end process;' don't count as tests
                        footer += line
                    else:
                        body += line
            line = source.readline()
            line_lower = line.lower().strip()
            words = line_lower.split(' ')
            
        for line in source:                                     # All remaining lines, if any, are placed in the footer
            footer += line
        source.close()
        
    tests = 0                                                   # Count the tests in the body
    bodylines = body.split('\n')
    for line in bodylines:                                      # Lines in the body are all single tests/functions (or should be)
        if line.strip():
            tests += 1
            templist.append(line)                               # [META] Improving detection here can be a major asset
    
    logwrite('n','Successfully parsed ' + entname + '.' + archname + ' with ' + str(tests) + ' tests found.')
    return [archname, entname, header, archheader, footer, templist]
    
## arranges found functions & procedures in their own executable files
def test_format(parsedlist_t = None):
    if parsedlist_t == None:
        parsedlist_t = parsedlist
    ## Format: parsedlist is a tuple of tuples: [archname, entname, header, archheader, footer, list_of_tests]
        
    testcount = {}
    
    for parsedfile in parsedlist_t:                             # Assign correct variables for making the files
        if parsedfile[0] != 'ERROR':
            archname = parsedfile[0]
            entname = parsedfile[1]
            header = parsedfile[2]
            archheader = parsedfile[3]
            footer = parsedfile[4]
            tests = parsedfile[5]
            
            localcount = 0
            localname = entname + '.' + archname + '.vhd'
            testfile_path = tempdir + os.sep + localname
            testfile = open(testfile_path,'w+')
            testfile.write(header)
            
            for test in tests:                                      # Place every test in the same file, under a different architecture
                testfile.write(archheader.replace(archname, archname + str(localcount)))
                testfile.write(test + '\n')
                testfile.write(footer.replace(archname, archname + str(localcount)) + '\n\n')
                localcount += 1
            testfile.close()
            testcount[localname] = localcount                       # Keep a dictionary of the tests in each file
        else:
            logwrite('n','Ignoring test ' + str(len(testcount)) + ', file: ' + str(parsedfile[1]))
    return testcount
    
## grabs processed source/files, executes & captures output
def parse_tests(testcount_t = None, tempdir_t = None, foldername_t = None):
    if (testcount_t == None):                                       # Use global values if arguments not filled
        testcount_t = testcount
    if (tempdir_t == None):
        tempdir_t = tempdir
    if (foldername_t == None):
        foldername_t = foldername
    
    os.chdir(tempdir_t)                                             # Change working directory to the temporary folder
    
    testcount = 0
    output = []                                                     # tuple of captured outputs per file
    
    for key,value in testcount_t.iteritems():                       # Run across the dictionary, executing every test in each file
        ## !! vhdlUnit.vhd is a library for the tested files, need to find a way to compile dependencies before actual files first !!
        commands = 'vlib work' + str(testcount) +'\n' + 'vcom -2008 -quiet -work work' + str(testcount) +' C:\\Users\\Joren\\Git\\VHDL\\src\\vhdlUnit.vhd' + ' ' + key
        for line in commands.split('\n'):                           # Form a work directory per file
            os.system(line)
        
        words = str(key).split('.')
        entname = words[0]                                          # Get entity and architecture name from the key
        archname = words[1]
        
        outputfile = open(os.getcwd() + os.sep + words[0] + '_cmd_output.txt','w+')
        logwrite('n','Starting execution of tests in ' + str(key))
        for test in range(0, value):
            readcmd = os.popen('vsim -c "work' + str(testcount) + '.' + entname + '(' + archname + str(test) + ')" -do "run -all;exit"').read()
            outputfile.write(readcmd)
        outputfile.close()
        testcount += 1
    os.chdir(currentdir)
    print 'cookies'
    
## grabs output, formats output
def format():
    return 'format'
    
## grabs processed output, converts to JUnit compatible XML file
def xmlwrite():
    return 'xmlwrite'
    
## removes temporary files & directories
def cleanup():
    logwrite('n','Cleanup started at ' + str(time.time()))
    shutil.rmtree(tempdir)


##########################################
######## MAIN PROGRAM STARTS HERE ########
##########################################

#Allows the code to be used as a module
if __name__ == "__main__":
    
    parsedlist, logs_buffer = [], []
    logstarted = False
    tempdir, tempname, folderpath, foldername = '', '', '', ''
    parser = None
    
    systemtime = time.time()                                                        # Marks the starting time of the script
    currentdir = os.getcwd()
    
    parser = setup_parser()                                                         # Creates an argument parser for the commandline arguments
    args, unknown = parser.parse_known_args()                                       # Parses commandline arguments, stores unknown arguments in 'unknown'
    
    tempdir, tempname = make_tempdir()                                              # Creates a unique (name for the) temporary directory in the systems temp dir
    foldername = setup(args, tempname)                                              # Grab all functions and procedures to be processed, know the path of the output folder
    
    logwrite('n','Started script at ' + str(systemtime))
    if (unknown):                                                                   # If there are unknown arguments, write error to log
        logwrite('w', 'Found unusuable arguments: ' + ', '.join(unknown))

    if (args.file):                                                                 # If the files to be processed are in a file
        for file in args.list:                                                      # Get all filenames
            listfile = open(get_path(file))                                         # Open the files one by one
            sourcelist = [line.strip() for line in open(listfile)]                  # Get all .vhd files listed within
            for line in sourcelist:                                                 # Parse each .vhd file
                parsedlist.append(parse_source(get_path(line), args.script))
    else:                                                                           # If arguments are not lists, they are the files themselves
        for file in args.list:
            parsedlist.append(parse_source(get_path(file), args.script))
    
    testcount = test_format(parsedlist)                                             # Get list of number of tests
    parse_tests(testcount, tempdir, foldername)
    
    #cleanup()
    logwrite('n','Stopped script at ' + str(time.time()))
    
    #global vars / needed for
    #
    ## operating system (linux/Windows)
    ## --Path separator
    ## --temporary folder location: output, processed output, xml file
    #
    ## Functions & procedures
    ## --Executing F&P
    ## --Creating files
    ## --Catching output correctly