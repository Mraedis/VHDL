#Assume use of testing library 

##1. Get current working directory
##2. Add temporary folder with temp_timestamp (ensures folders are unique)
##3. Create new testbench for each assert
##4. Execute testbenches & gather output >> Not Yet Implemented
##5. Save output and delete temporary testbenches if wanted

from sys import argv
import os
import subprocess
import string
import random
import shutil


#Set up working directories and variables
##Source is the file to be parsed
##Targetpath is the destination folder for all (temporary) files
##Outputfile stores the output of the compiled and ran VHD files
def setup():
	source = open(filename)
	char_set = string.ascii_uppercase + string.digits
	tempdir = ''.join(random.sample(char_set*8,8))
	targetpath = os.getcwd() + os.sep() + 'temp_' + tempdir
	outputfile = open(targetpath + '_cmd_output.txt','w+')
	
	#Purely for testing purposes
	targetpath = targetpath.replace('VHDL', 'Testing_Folder')
	
	#Make temporary folder for testbenches
	if not os.path.isdir(targetpath):
		os.makedirs(targetpath)
	
	return source, targetpath, outputfile
	
#Reads source file to:
##1. Determine tests to be run
##2. Determine dependencies
def parse(source):	
	#Search for architecture definition
	wrapperstart = '\n\tprocess\n\tbegin\n'
	wrapperend = '\n\t\twait;\n\tend process;\n'
	header, body, footer = '', '', ''
	assertcount = 0
	line = source.readline()
	archstart = False
	archbegin = False
	
	while not archstart:
		words = line.split(' ')
		if words:	
			if (words[0].lower() == 'entity'):
				entname = words[1]	#Save entity name	
			if (words[0].lower() == 'architecture'):
				archstart = True
				footer = 'end architecture ' + words[1] + ';' 
				marker = words[1] #Save architecture name for later change (avoid duplicates)
		header += line
		line = source.readline()	
	#"Make" footer (architecture end) out of architecture name
		
	while not archbegin:
		words = line.split(' ')
		if (words[0].lower() == 'begin'):
			archbegin = True
		header += line
		line = source.readline()
	header += wrapperstart
	#Header is finished and contains start of assert process
		
	for line in source:
		body += line
	#while line is not (footer or ''):
	#	body += line
	#	line = source.readline()	
	footer = wrapperend + footer
	#Body is finished, should contain only asserts
	source.close()
	
	allasserts = ''
	assertcount = 0
	bodylines = body.split('\n')
	commands = 'vlib work\nvcom -2008 -quiet -work work '
	for line in bodylines:
		if line.strip()[0:6] == 'assert':
			if (assertcount % 17) == 0: #VHDL editor license doesn't allow great amounts of code
				currentpath = targetpath + os.sep() + 'assert_test_' + str(assertcount) + '.vhd'
				commands += currentpath + ' '
				allasserts += 'assert_test_' + str(assertcount) + '\n'
				targetfile = open(currentpath,'w+')
				targetfile.write(header.replace(marker, 'assert_test_' + str(assertcount)))
				targetfile.write('\n\tassert false report "Start" severity note;\n')
				targetfile.write(line)
				targetfile.write('\n\tassert false report "End" severity note;\n')
				targetfile.write(footer.replace(marker, 'assert_test_' + str(assertcount)))
				targetfile.close()
			assertcount += 1
	#All asserts in a separate .vhd file
	
	return entname, allasserts, commands
	
#Execute code and save output in outputfile
def test(allasserts, commands, entname, outputfile):
	for line in commands.split('\n'):
		os.system(line)
	
	for line in allasserts.split('\n'):
		if line[0:6] == 'assert': #Avoid executing empty newlines
			readcmd = os.popen('vsim -c -quiet "work.' + entname + '(' + line + ')" -do "run -all;exit"').read()
			outputfile.write(readcmd)
	outputfile.close()
	
	return
	
#remove all temporary files
def cleanup(targetpath):
	shutil.rmtree(targetpath)
	workpath = targetpath.replace()
	#shutil.rmtree(
	return

script, filename = argv

#cleanup = raw_input('Delete temporary testbenches? [Y/n]: ')
#while cleanup.lower() != 'y' and cleanup.lower() != 'n':
#	print 'Only valid inputs are Y, y, N or n'
#	cleanup = raw_input('Delete temporary testbenches? [Y/n]: ')
#cleanup = cleanup.lower()
clean = 'y' #Standard clean-up for testing purposes

source, targetpath, outputfile = setup()
entname, allasserts, commands = parse(source)
test(allasserts, commands, entname, outputfile)

if clean == 'y':
	cleanup(targetpath)





