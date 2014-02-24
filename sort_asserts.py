#1. Get current working directory
#2. Add temporary folder with temp_timestamp (ensures folders are unique)
#3. Create new testbench for each assert
#4. Execute testbenches & gather output
#5. Save output and delete temporary testbenches if wanted


from sys import argv
import os
import string
import random
import shutil
script, filename = argv

cleanup = raw_input('Delete temporary testbenches? [Y/n]: ')
while cleanup.lower() != 'y' and cleanup.lower() != 'n':
	print 'Only valid inputs are Y, y, N or n'
	cleanup = raw_input('Delete temporary testbenches? [Y/n]: ')
cleanup = cleanup.lower()
	
sourcefile = open(filename)

currentpath = os.getcwd()
char_set = string.ascii_uppercase + string.digits
tempdir = ''.join(random.sample(char_set*8,8))
targetpath = currentpath + '\\temp_' + tempdir

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

bodylines = body.split('\n')
for line in bodylines:
	if line.strip()[0:6] == 'assert':
		targetfile = open(targetpath + '\\' + 'assert_test_' + str(assertcount) + '.vhd','w+')
		targetfile.write(header)
		targetfile.write(line)
		targetfile.write(footer)
		assertcount += 1
		targetfile.close()
#All asserts in a seperate .vhd file

if cleanup == 'y':
	shutil.rmtree(targetpath)
#removes temporary folder
