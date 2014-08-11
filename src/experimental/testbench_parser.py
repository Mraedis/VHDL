###FUNCTIONS###
#  setup()
## Sets up (temporary) files, sets all global vars, processes cmdline arguments with argparse
#
#  setup_parser()
## Prepares the parser to accept correct commandline arguments
#  
#  tempdir()
## Creates the temporary working directory
#  
#  parse_source()
## grabs sourcefile, extracts needed code, arranges functions & procedures
#
#  test_format()
## arranges found functions & procedures in their own executable files
#
#  parse_tests
## grabs processed source/files, executes & captures output
#
#  format()
## grabs output, formats output
#
#  xmlwrite()
## grabs processed output, converts to JUnit compatible XML file
#
#  cleanup()
## removes temporary files & directories
###END###

import sys
import os
import tempfile
import string
import random

## Sets up (temporary) files, sets all global vars, processes cmdline arguments with argparse
def setup():
    return 'setup'

## Prepares the parser to accept correct commandline arguments
def setup_parser():
    parser = argparse.ArgumentParser(version='0.1'
                                     , description='VHDL testbench to TDD parser'
                                     , formatter_class=argparse.ArgumentDefaultsHelpFormatter)
        #The -c/--cmd argument is to specify the script being called by an automated program.
        #The flag is stored in 'cmd', default value is 'False'
    parser.add_argument('-c', '--cmd'
                         , help='specifies script being called from commandline, not automated'
                         , action = store_true, dest=cmd, default=False)
        #The -l/--list argument is to specify the list of .vhd files to be processed.
        #The flag is stored in 'list', no default value considering there NEEDS to be at least 1 .vhd file
    parser.add_argument('-l', '--list'
                        , help='specifies the list of .vhd files to be processed -- note: ONLY .vhd files'
                        , action = store, dest=list
                        , nargs = '+', required=True, metavar='path')
        #The -f/--file argument is to specify that the -l/--list is a file/files containing a list of .vhd files.
        #The flag is stored in 'file', default value is 'False'
    parser.add_argument('-f', '--file'
                        , help='specifies -l/--list is a file with a list'
                        , action = store_true, dest=file, default=False)
        #The -d/--dest argument is to specify a custom folderpath for the log.
        #The flag is stored in 'log', default value is 'None'
    parser.add_argument('-d', '--dest'
                        , help='specifies a custom folder for the log'
                        , type = str
                        , action = store, dest=log, default=None)
    
    return parser

## Creates the temporary working directory
def tempdir():
    char_set = string.ascii_uppercase + string.digits       #Generates the character set used: letters & numbers, lower & uppercase
    tempname = ''.join(random.sample(char_set*8,8))         #Save temporary name for deletion later
    tempdir = tempfile.gettempdir() + os.sep + tempname
    
    while(os.path.isdir(tempdir)):                          #Make sure directory doesn't exist yet
         tempname = ''.join(random.sample(char_set*8,8))
         tempdir = tempfile.gettempdir() + os.sep + tempname
    
    os.makedirs(tempdir)                                    #create temporary working directory
    return tempdir, tempname

    
## grabs sourcefile, extracts needed code, arranges functions & procedures
def parse_source():
    return 'parse_source'
    
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

#Allows the code to be used as a module
if __name__ == "__main__":
    import argparse
    
    tempdir, tempname = '', ''
    currentdir = os.getcwd()
    
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
    
    parser = parser_setup()                     #Creates an argument parser for the commandline arguments
    args, unknown = parser.parse_args()         #Parses commandline arguments, stores unknown arguments in 'unknown'
    tempdir, tempname = tempdir()               #Creates a unique (name for the) temporary directory in the systems temp dir
    
    #if (args contains list flag) do: open files, extract pathnames
    #Function: check for absolute or relative path and open accordingly
    #
    #
    #
    #