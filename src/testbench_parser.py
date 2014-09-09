##############################################################################################################################
###    Author:    Joren Guillaume
###
###    Changelog
###    2014.08.11 - Formatting, filled in code for functions
###    2014.08.17 - More filling in code
###    2014.08.21 - Added logwrite()
###    2014.05.25 - Editing functions, use of global variables
###
############### FUNCTIONS  ###################################################################################################
#  setup()               ## Sets up (temporary) files, sets all global vars, processes cmdline arguments with argparse
#  logwrite()            ## Writes things to the logbook, errors, completed jobs etc.
#  get_path(path)        ## Returns absolute path if not already absolute path, otherwise return argument unchanged
#  setup_parser()        ## Prepares the parser to accept correct commandline arguments
#  tempdir()             ## Creates the temporary working directory
#  parse_source()        ## grabs sourcefile, extracts needed code, arranges functions & procedures
#  test_format()         ## arranges found functions & procedures in their own executable files
#  parse_tests           ## grabs processed source/files, executes & captures output
#  format()              ## grabs output, formats output
#  xmlwrite()            ## grabs processed output, converts to JUnit compatible XML file
#  cleanup()             ## removes temporary files & directories
################## END  ######################################################################################################

import sys
import os
import tempfile
import string
import random
import argparse
import time

## Sets up (temporary) files, sets all global vars, processes cmdline arguments with argparse
def setup(args, tempname=None, folderpath=None, foldername=None):
    if (tempname == None):                                                          # If tempname was not given with the function, use global name
        global tempname
    if (folderpath == None):                                                        # If folderpath was not given with the function, use global name
        global folderpath
    if (foldername == None):                                                        # If folername was not given with the function, use global name
        global foldername
        
    folderpath = os.path.join(os.environ['USERPROFILE'], 'VHDL_TDD_Parser')
    if (not os.path.isdir(folderpath)):                                             # Make the folder that contains all non-deleted files from all time, if non-existant
        os.makedirs(folderpath)
        logwrite('n','Created working directory \'VHDL_TDD_Parser\' for the first time.')
    outputdir = time.strftime("%Y.%M.%d - %H.%M") + tempname
    foldername = os.path.join(folderpath, outputdir)
    if (os.path.isdir(outputdir)):                                                  # Make the folder for the current run    
        logwrite('n','Output directory already existed.')
    else:
        os.makedirs(foldername)

    if (args.file):                                                                 # If the files to be processed are in a file
        for file in args.list:                                                      # Get all filenames
            listfile = open(get_path(file))                                         # Open the files one by one
            sourcelist = [line.strip() for line in open(listfile)]                  # Get all .vhd files listed within
            for line in sourcelist:                                                 # Parse each .vhd file
                parse_source(get_path(line), args.script)
    else:                                                                           # If arguments are not lists, they are the files themselves
        for file in args.list:
            parse_source(get_path(file), args.script)
    return foldername

## Writes things to the logbook: errors, completed jobs etc.
def logwrite(level='n',message ='No message given.', tempname=None, parser=None):
    if (tempname == None):
        global tempname
    if (parser == None):
        global parser
    global foldername
    dest = ''
    if (parser.log != None):
        dest = get_path(parser.log)
    else:
        dest = foldername
        
    #FORMAT: Timestamp \tab Level \tab Message
    levels = {'n': 'notice', 'w' : 'warning', 's': 'severe', 'c': 'critical', 'u' : 'unknown'};
    logfile = open(dest, 'r+')
    
    logfile.seek(0)
    first_char = logfile.read(1)
    if not first_char:                                                                              # Check for any file contents and create header if none
        logfile.write('####################################################\n')
        logfile.write('###############                      ###############\n')
        logfile.write('############### LOGFILE FOR ' + tempname + ' ###############\n')
        logfile.write('###############                      ###############\n')
        logfile.write('####################################################\n\n')
        logfile.write('YYYY.MM.DD - HH:MM  -   LEVEL    -  MESSAGE\n\n')
                                                                                                    # Print the message with information on time and level
    logfile.write(time.strftime("%Y.%M.%d - %H:%M") + '  -  ' + (levels[level] + '\t').expandtabs(5) + '-  ' + message + '\n')

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
                         , action = store_true, dest=cmd, default=False)
        # The -l/--list argument is to specify the list of .vhd files to be processed.
        # The flag is stored in 'list', no default value considering there NEEDS to be at least 1 .vhd file
    parser.add_argument('-l', '--list'
                        , help='specifies the list of .vhd files to be processed -- note: ONLY .vhd files'
                        , action = store, dest=list
                        , nargs = '+', required=True, metavar='path')
        # The -f/--file argument is to specify that the -l/--list is a file/files containing a list of .vhd files.
        # The flag is stored in 'file', default value is 'False'
    parser.add_argument('-f', '--file'
                        , help='specifies -l/--list is a file with a list'
                        , action = store_true, dest=file, default=False)
        # The -d/--dest argument is to specify a custom folderpath for the log.
        # The flag is stored in 'log', default value is 'None'
    parser.add_argument('-d', '--dest'
                        , help='specifies a custom folder for the log'
                        , type = str
                        , action = store, dest=log, default=None)
        # The -s/--script argument is to specify the testbench(es) use the scriptstart/scriptend comments
        # The flag is stored in 'script', default value is 'False'
    parser.add_argument('-s', '--script'
                        , help='specifies the testbenches use the ScriptStart and ScriptEnd comments'
                        , action = store_true, dest=script, default=False)
    
    return parser

## Creates the temporary working directory
def tempdir():
    char_set = string.ascii_uppercase + string.digits       # Generates the character set used: letters & numbers, lower & uppercase
    tempname = ''.join(random.sample(char_set*8,8))         # Save temporary name for deletion later
    tempdir = tempfile.gettempdir() + os.sep + tempname
    
    while(os.path.isdir(tempdir)):                          # Make sure directory doesn't exist yet
         tempname = ''.join(random.sample(char_set*8,8))
         tempdir = tempfile.gettempdir() + os.sep + tempname
    
    os.makedirs(tempdir)                                    # Create temporary working directory
    return tempdir, tempname

    
## grabs sourcefile, extracts needed code, arranges functions & procedures
def parse_source(path, script=False, parsedlist=None):
    
    if(parsedlist == None):
        global parsedlist
    
    scriptstart, scriptend = False, False
    
    source = open(path)                                     # Open the file
    line = source.readline()
    line_lower = line.lower()                               # VHDL is case sensitive, but using lowercase is easier to target identifiers such as 'architecture', 'function' etc.
    words = line_lower.split(' ')
    
    if (script):                                            # Check whether scriptstart/end is being used with 'script'
        while (not scriptstart):
            if (line_lower.strip() == '--scriptstart'):     # Find the start of the script, add all lines before the start to the header
                scriptstart = True
                archheader += '--MARKER--'
            else:
                archheader += line
                line = source.readline()
                line_lower = line.lower()
                
        line = source.readline()
        line_lower = line.lower()
        
        while (not scriptend):
            if (line.lower.strip() == '--scriptend'):
                scriptend = True
                print 'scriptend'
            else:
                archheader += line
        # Find line with --scriptstart, --scriptend, extract code in between
        print 'script'
        # if no line with --scriptstart or --scriptend, error
    else:
        print 'no script'        
        #if script = True, skip the file to the line with '--scriptstart' and extract code
        #find functions & procedures
    return parsedlist.append(templist)
    
## arranges found functions & procedures in their own executable files
def test_format():
    return 'test_format'
    
## grabs processed source/files, executes & captures output
def parse_tests():
    return 'parse_tests'
    
## grabs output, formats output
def format():
    return 'format'
    
## grabs processed output, converts to JUnit compatible XML file
def xmlwrite():
    return 'xmlwrite'
    
## removes temporary files & directories
def cleanup():
    return 'cleanup'


##########################################
######## MAIN PROGRAM STARTS HERE ########
##########################################

parsedlist = []                                                     # Defined as global variables for use elsewhere in script
tempdir, tempname, folderpath, foldername = '', '', '', ''
parser = None

#Allows the code to be used as a module
if __name__ == "__main__":
    
    systemtime = time.time()                                        # Marks the starting time of the script
    logwrite('n','Started at ' + str(systemtime))
    currentdir = os.getcwd()
    
    parser = parser_setup()                                         # Creates an argument parser for the commandline arguments
    args, unknown = parser.parse_known_args()                       # Parses commandline arguments, stores unknown arguments in 'unknown'
    
    if (not unknown):                                               # If there are unknown arguments, write error to log
        logwrite('w', 'Found unusuable arguments: ' + ', '.join(unknown))
    
    tempdir, tempname = tempdir()                                   # Creates a unique (name for the) temporary directory in the systems temp dir
    folderpath = setup(args, tempname)                              # Grab all functions and procedures to be processed, know the path of the output folder
    
    
    
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