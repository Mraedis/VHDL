#Assume use of testing library 

##1. Get current working directory
##2. Create new architecture for each test
##3. Execute testbench & gather output
##4. Save output and delete temporary testbench if desired
##5. Parse output into readable format

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
	target = ''.join(random.sample(char_set*8,8))
	outputfile = open(os.getcwd() + os.sep + target + '_cmd_output.txt','w+')
		
	return source, target, outputfile
	
#Reads source file to:
##1. Determine tests to be run
##2. Determine dependencies
def parse(source, target):	
	#Search for architecture definition
	header, archheader, archfooter = '', '', ''
	commands = 'vlib work\n' + 'vcom -2008 -quiet -work work vhdlUnit.vhd '+ target +'.vhd\n'
	alltests = ''
	testcount = 0
	line = source.readline()
	archstart, scriptstart, scriptend = False, False, False
	
		#Extract the comments, entity name, libraries, ...
	while not archstart:
		words = line.split(' ')
		if words:	
			if (words[0].lower() == 'entity'):
				entname = words[1]
					#Save entity name	
			if (words[0].lower() == 'architecture'):
				archstart = True
				archname = words[1]
					#Save architecture name
		if not archstart:
			header += line
		else:
			archheader += line
		line = source.readline()
		
		#Extract the architecture part before the tests
	while not scriptstart:
		words = line.split(' ')
		if words:	
			if (words[0].strip().lower() == '--scriptstart'):
				scriptstart = True
		if not scriptstart:
			archheader += line
		line = source.readline()
	
		#Extract all tests
	while not scriptend:
		words = line.split(' ')
		if words:
			if (words[0].strip().lower() == '--scriptend'):
				scriptend = True
		if not scriptend:
			alltests += line
			line = source.readline()
		
		#Finish extracting the architecture part
		#There should be nothing after the architecture end
	for line in source:
		archfooter += line
	source.close()
	
	testlines = alltests.split('\n')
	currentpath = os.getcwd() + os.sep + target + '.vhd'
	targetfile = open(currentpath,'w+')
	targetfile.write(header)
	for line in testlines:
		if line.strip():
			targetfile.write(archheader.replace(archname, archname + str(testcount)))
			targetfile.write(line + '\n\n')
			targetfile.write(archfooter.replace(archname, archname + str(testcount)) + '\n\n')
			testcount += 1
	targetfile.close()
	
	return entname, archname, testcount, commands
	
	
	#Execute code and save output in outputfile
def test(testcount, commands, entname, archname, outputfile):
	for line in commands.split('\n'):
		os.system(line)
	
	for test in range(0, testcount):
		readcmd = os.popen('vsim -c -quiet "work.' + entname + '(' + archname + str(test) + ')" -do "run -all;exit"').read()
		outputfile.write(readcmd)
	outputfile.close()
	
	return

	#format the outputfile
def format(target):
	testsfailed, testspassed, othernotes = 0, 0, 0
	failedlines, passedlines, otherlines = '', '', ''
		#remove modelsim wrappers
	source = open(os.getcwd() + os.sep + target + '_cmd_output.txt')
	for line in source:
		words = line.split(' ')
		if (len(words) > 2):
			if(words[2] == 'Note:'):
				if(len(words) > 4):
					if(words[4] == 'failed,'):
						testsfailed += 1
						failedlines += line
					elif(words[4] == 'success,'):
						testspassed += 1
						passedlines += line
					else:
						othernotes += 1
						otherlines += line
				else:
					othernotes += 1
	targetpath = os.getcwd() + os.sep + target + '__testresults.txt'
	targetfile = open(targetpath,'w+')
	targetfile.write('tests passed: ' + str(testspassed) +'\ntests failed: ' + str(testsfailed) + '\nother notes: ' + str(othernotes))
	targetfile.write('\n\n\nPassed tests reports:\n' + passedlines + '\n\n Failed tests reports:\n' + failedlines + '\n\nOther notes:\n' + otherlines)
	print 'tests passed: ' + str(testspassed) +'\ntests failed: ' + str(testsfailed) + '\nother notes: ' + str(othernotes)
	source.close()
	targetfile.close()
	
	#remove all temporary files
def cleanup(target):
	#shutil.rmtree(os.getcwd() + os.sep + 'work')
	os.remove(os.getcwd() + os.sep + 'transcript')
	os.remove(os.getcwd() + os.sep + target + '.vhd')
	return

script, filename = argv

#clean = raw_input('Delete temporary testbenches? [Y/n]: ')
#while clean.lower() != 'y' and clean.lower() != 'n':
#	print 'Only valid inputs are Y, y, N or n'
#	clean = raw_input('Delete temporary testbenches? [Y/n]: ')
#clean = clean.lower()
clean = 'y' #Standard clean-up for testing purposes

source, target, outputfile = setup()
entname, archname, testcount, commands = parse(source, target)
test(testcount, commands, entname, archname, outputfile)
format(target)

if clean == 'y':
	cleanup(target)





