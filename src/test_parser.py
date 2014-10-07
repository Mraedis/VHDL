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
import re
import StringIO
import function_tests as ft
from junit_xml import TestSuite, TestCase


#Set up working directories and variables
##Source is the content of the file to be parsed
##Target is an 8 character random code for the current project
##Outputfile stores the output of the compiled and ran VHD files
def setup(filename):
	file = open(filename)
	source = file.read().lower()
	char_set = string.ascii_uppercase + string.digits
	target = ''.join(random.sample(char_set*8,8))
	outputfile = open(os.getcwd() + os.sep + target + '_cmd_output.txt','w+')
		
	return source, target, outputfile
	
#Reads source file to:
##1. Determine tests to be run
##2. Determine dependencies
def parse(source, target):
	buffer = StringIO.StringIO(source)
	
		#Search for architecture definition
	header, archheader, archfooter = '', '', ''
	alltests = ''
	testcount = 0
	line = buffer.readline()
	archstart, scriptstart, scriptend = False, False, False
	
		#Extract the comments, entity name, libraries, ...
	while not archstart:
		words = line.split(' ')
		if words:	
			if (words[0] == 'entity'):
				entname = words[1]
					#Save entity name	
			if (words[0] == 'architecture'):
				archstart = True
				archname = words[1]
					#Save architecture name
		if not archstart:
			header += line
		else:
			archheader += line
		line = buffer.readline()
		
		#Extract the architecture part before the tests
	while not scriptstart:
		words = line.split(' ')
		if words:	
			if (words[0].strip() == '--scriptstart'):
				scriptstart = True
		if not scriptstart:
			archheader += line
		line = buffer.readline()
	
		#Extract all tests
	while not scriptend:
		words = line.split(' ')
		if words:
			if (words[0].strip() == '--scriptend'):
				scriptend = True
		if not scriptend:
			alltests += line
			line = buffer.readline()
		
		#Finish extracting the architecture part
		#There should be nothing after the architecture end
	for line in buffer:
		archfooter += line
	buffer.close()
	
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
	
	return entname, archname, testcount
	
	
	#Execute code and save output in outputfile
def test(testcount, target, entname, archname, outputfile):
		#Hudson-CI requires vhdlUnit nearby
	commands = 'vlib work\n' + 'vcom -2008 -quiet -work work H:\\Users\\Joren\\Documents\\GitHub\\VHDL\\src\\experimental\\vhdlUnit.vhd '+ target +'.vhd\n'
	#commands = 'vlib work\n' + 'vcom -2008 -quiet -work work vhdlUnit.vhd '+ target +'.vhd\n'
	for line in commands.split('\n'):
		os.system(line)
	
	for test in range(0, testcount):
		readcmd = os.popen('vsim -c "work.' + entname + '(' + archname + str(test) + ')" -do "run -all;exit"').read()
		#readcmd = os.popen('vsim -c -quiet "work.' + entname + '(' + archname + str(test) + ')" -do "run -all;exit"').read()
		outputfile.write(readcmd)
	outputfile.close()
	
	return

	#format the outputfile
	
def format(target, arguments=''):
	failedtests, passedtests, othernotes, totaltests = 0, 0, 0, 0
	failedlines, passedlines, otherlines, everyline = '', '', '', ''
		#remove modelsim wrappers
	source = open(os.getcwd() + os.sep + target + '_cmd_output.txt')
	
		#This part parses the output to a simple .txt
		#It is meant as a plaintext back-up for the "better" xml output
	ReadNote = False
	lastline = ''
	for line in source:
		words = line.split(' ')
		if ReadNote == True:
			to_add = ' - time: ' + words[5] + ' ' + words[6] + '\n'
			if lastline == 'success':
				passedlines += to_add
			elif lastline == 'failed':
				failedlines += to_add
			elif lastline == 'other':
				otherlines += to_add
			everyline += to_add
			ReadNote = False
			
		elif (len(words) > 2):
			if(words[2] == 'Note:'):
				ReadNote = True
				if(len(words) > 4):
					if(words[4] == 'failed\tname:'):
						failedtests += 1
						totaltests += 1
						failedlines += str(failedtests).zfill(4) + ' - ' + line[11:-1]
						lastline = 'failed'
					elif(words[4] == 'success\tname:'):
						passedtests += 1
						totaltests += 1
						passedlines += str(passedtests).zfill(4) + ' - ' + line[11:-1]
						lastline = 'success'
					else:
						othernotes += 1
						totaltests += 1
						otherlines += str(othernotes).zfill(4) + ' - ' + line[11:-1]
						lastline = 'other'
				else:
					othernotes += 1
					totaltests += 1
					otherlines += str(othernotes).zfill(4) + ' - ' + line[11:-1]
					lastline = 'other'
				everyline += str(totaltests).zfill(4) + ' - ' + line[11:-1]
				
	if ReadNote == True:
		to_add = ' - time: ' + words[5] + ' ' + words[6]
		if lastline == 'succes':
			passedlines += to_add
		elif lastline == 'failed':
			failedlines += to_add
		elif lastline == 'other':
			otherlines += to_add
		everyline += to_add
	
		#Write plain .txt file with testresults
	targetpath = os.getcwd() + os.sep + target + '_testresults.txt'
	targetfile = open(targetpath,'w+')
	targetfile.write('total tests: ' + str(totaltests) + '\ntests passed: ' + str(passedtests) +'\ntests failed: ' + str(failedtests) + '\nother notes: ' + str(othernotes))
	targetfile.write('\n\n\nPassed tests reports:\n' + passedlines + '\n\nFailed tests reports:\n' + failedlines + '\n\nOther notes:\n' + otherlines + '\n\n\nAll test results:\n' + everyline)


	xmlwrite(target, everyline)
	
	#xmltargetpath = os.getcwd() + os.sep + target + '_testresults.xml'
	#xmltargetfile = open(xmltargetpath, 'w+')
	#xmltargetfile.write('<testsuite tests="' + str(totaltests) + '" name ="' + target + '_tests">')
	#for line in everyline.split('\n'):
	#	words = line.split(' ')
	#	if line.find('success') != -1:
	#		xmltargetfile.write('\n\t<testcase classname="' + target + '" status="' + str(words[0]) + '" name="' + re.search('(?<=name: )(.*)(?= -)', line).group(0) + '"/>')
	#	elif line.find('failed') != -1:
	#		xmltargetfile.write('\n\t<testcase classname="' + target + '" status="' + str(words[0]) + '" name="' + re.search('(?<=name: )(.*)(?= -)', line).group(0) + '">')
	#		xmltargetfile.write('\n\t\t<failure> ' + line.split(' - ')[2] + ' </failure>\n\t</testcase>')
	#xmltargetfile.write('\n</testsuite>')
	
	print 'total tests: ' + str(totaltests) + '\ntests passed: ' + str(passedtests) +'\ntests failed: ' + str(failedtests) + '\nother notes: ' + str(othernotes)
	#Print left out - optional command line output
	
	source.close()
	targetfile.close()
	
	
		#Write .xml file in JUnit format (For use in Eclipe)
		#Uses JUnit-xml package by Brian Beyer
		#source at https://github.com/kyrus/python-junit-xml
		#Requires setuptools to install at https://pypi.python.org/pypi/setuptools
def xmlwrite(target, everyline):
	xmltargetpath = os.getcwd() + os.sep + target + '_testresults.xml'
	xmltargetfile = open(xmltargetpath, 'w+')
	test_cases = []
	for line in everyline.split('\n'):
		words = line.split(' ')
		if line.find('success') != -1:
			time_taken = get_time(line.split('-')[-1][7:])
			name = words[0] + ' - ' + line.split('-')[1].split('name:')[1].strip()
			print name
			test_cases.append(TestCase(name, target, time_taken, None))
		elif line.find('failed') != -1:
			time_taken = get_time(line.split('-')[-1][7:])
			name = words[0] + ' - ' + line.split('-')[1].split('name:')[1].strip()
			message = ("-".join(line.split(' - ')[2:-1])).strip()
			tc = TestCase(name, target, time_taken, None)
			tc.add_failure_info(None, message)
			test_cases.append(tc)
	ts = TestSuite("testing this suite", test_cases)
	xmltargetfile.write(TestSuite.to_xml_string([ts]))
	xmltargetfile.close()
	
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
	
	#remove all temporary files
def cleanup(target):
	#shutil.rmtree(os.getcwd() + os.sep + 'work')
	#keep work library for compiled vhdlUnit.vhd in tests.
	os.remove(os.getcwd() + os.sep + 'transcript')
	os.remove(os.getcwd() + os.sep + target + '.vhd')
	return

#Actual script starts here, above only definitions

script, filename = argv
#script, arguments = argv
#words = arguments.split(' ')
#print argv
#if len(words) > 1:
#	filename = words[0]
#	arguments = words[1:]
#else:
#	filename = words[0]
	

#clean = raw_input('Delete temporary testbenches? [Y/n]: ')
#while clean.lower() != 'y' and clean.lower() != 'n':
#	print 'Only valid inputs are Y, y, N or n'
#	clean = raw_input('Delete temporary testbenches? [Y/n]: ')
#clean = clean.lower()
	#Standard clean-up for testing purposes

	#Set random path, open outputfile, open sourcefile
source, target, outputfile = setup(filename)
	#Get entity and architecture names, get amount of tests and command to compile temporary .vhd file

all = []
#Testcodes, please ignore
entname, archname, all = ft.parsetest(source, target)

#entname, archname, testcount = parse(source, target)
#test(testcount, target, entname, archname, outputfile)
#format(target)

#if clean == 'y':
#	cleanup(target)





