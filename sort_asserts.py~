#1. Get current working directory
#2. Add temporary folder with temp_timestamp (ensures folders are unique)
#3. Create new testbench for each assert
#4. Execute testbenches & gather output >> Not Yet Implemented
#5. Save output and delete temporary testbenches if wanted


from sys import argv
import os
import subprocess
import string
import random
import shutil
script, filename = argv


#cleanup = raw_input('Delete temporary testbenches? [Y/n]: ')
#while cleanup.lower() != 'y' and cleanup.lower() != 'n':
#	print 'Only valid inputs are Y, y, N or n'
#	cleanup = raw_input('Delete temporary testbenches? [Y/n]: ')
#cleanup = cleanup.lower()
cleanup = 'n' #Standard no clean-up for testing purposes
	
sourcefile = open(filename)

currentpath = os.getcwd()
char_set = string.ascii_uppercase + string.digits
tempdir = ''.join(random.sample(char_set*8,8))
targetpath = currentpath + '\\temp_' + tempdir
targetpath = targetpath.replace('VHDL', 'Testing_Folder')
#More testing only

#Make temporary folder for testbenches
if not os.path.isdir(targetpath):
	os.makedirs(targetpath)

#Search for architecture definition
header = ''
wrapperstart = '\n\tprocess\n\tbegin\n'
body = ''
wrapperend = '\n\t\twait;\n\tend process;\n'
footer = ''
assertcount = 0
line = sourcefile.readline()
archstart = False
archbegin = False

while not archstart:
	words = line.split(' ')
	if words:
		if (words[0].lower() == 'architecture'):
			archstart = True
			footer = 'end architecture ' + words[1] + ';' 
			marker = words[1] #Save architecture name for later change (avoid duplicates)
	header += line
	line = sourcefile.readline()	
#"Make" footer (architecture end) out of architecture name
	
while not archbegin:
	words = line.split(' ')
	if (words[0].lower() == 'begin'):
		archbegin = True
	header += line
	line = sourcefile.readline()
header += wrapperstart
#Header is finished and contains start of assert process
	
for line in sourcefile:
	body += line
#while line is not (footer or ''):
#	body += line
#	line = sourcefile.readline()	
footer = wrapperend + footer
#Body is finished, should contain only asserts
sourcefile.close()

#Assume presence of ModelSim and add commands accordingly
commandfile = 'vlib work\nvcom -2008 -work work '

bodylines = body.split('\n')
for line in bodylines:
	if line.strip()[0:6] == 'assert':
		if (assertcount % 17) == 0: #VHDL editor license doesn't allow great amounts of code
			currentpath = targetpath + '\\' + 'assert_test_' + str(assertcount) + '.vhd'
			commandfile += currentpath + ' '
			#simcommands += ''
			targetfile = open(currentpath,'w+')
			targetfile.write(header.replace(marker, 'assert_test_' + str(assertcount)))
			targetfile.write('\n\tassert false report "Start" severity note;\n')
			targetfile.write(line)
			targetfile.write('\n\tassert false report "End" severity note;\n')
			targetfile.write(footer.replace(marker, 'assert_test_' + str(assertcount)))
			targetfile.close()
		assertcount += 1
#All asserts in a separate .vhd file

#Execute code
for line in commandfile.split('\n'):
	os.system(line)

os.system('vsim -c work.tb_libv(assert_test_0) -do "run -all;exit"')

if cleanup == 'y':
	shutil.rmtree(targetpath)
#removes temporary folder
