import string
import StringIO
import re

def get_syntax(procedure):
	procedure = re.sub('[^a-zA-Z0-9\n\.]', ' ', procedure)
	lines = procedure.split('\n')
	beginline = 0
	begin = False
	procedure_syntax = ''
	while begin == False:
		if (lines[beginline].strip() == 'begin'):
			begin = True
		else:
			beginline += 1
	for x in range (0, beginline):
		words = lines[x].split(' ')
		for y in range (0, len(words)):
			if (words[y].strip() == 'signal') or (words[y].strip() == 'variable'):
				if (len(procedure_syntax) == 0):
					procedure_syntax += words[y+1].strip()
				else:
					procedure_syntax += ', ' + words[y+1].strip()
					 
	return procedure_syntax

	#split source in pre-architecture 'header', architecture and post-architecture
def parsetest(source, target):
	buffer = StringIO.StringIO(source)
	
	header, archprebody, archbody, footer = '', '', '', ''
	entname, archname = '', ''
	line = buffer.readline()
	archstart, archend, archbegin = False, False, False
	
		#Extract the comments, entity name, libraries, ...
	while not archstart:
		line2 = line.strip()
		words = line2.split(' ')
		if words:
			if (str(words[0]) == 'entity'):
					#Save entity name	
				entname = words[1]
			if (str(words[0]) == 'architecture'):
				archstart = True
					#Save architecture name
				archname = words[1]
				
			#Ignore comments
		if (str(words[0])[0:2] != '--'):
			if not archstart:
				header += line
			else:
				archprebody += line
		line = buffer.readline()
		
		#nest is the level of "nesting", to see whether the script is reading inside a function, procedure, process...
		#if this is above 0, 'begin' does not signify the beginning of the architecture and thus archbegin is not set
	nest = 0
	functions_i, procedures_i = 0, 0
	last, current, funcproc = 'nothing', '', ''
	all = []
	count = 0
	while (not archbegin) and count <= 1000:
		line2 = line.strip()
		words = line2.split(' ')
			#Ignore comments
		if (line2[0:2] != '--'):
			if (words[0] == 'function' or words[0] == 'procedure'):
				nest += 1
				funcprocname = words[1]
				if (words[1][0:5] == 'test_'):
					last = str(words[0])
			elif (words[0] == 'end' and (words[1] == last + ';' or words[1] == funcprocname + ';')):
					#Functions and procedures are nest level 1, higher levels might be processes inside them
				if (nest == 1):
					if (last == 'function'):
						functions_i += 1
					elif(last == 'procedure'):
						procedures_i += 1
					if (last != 'nothing'):
						current += line
						all.append(current)
						current = ''
						last = 'nothing'
				nest += - 1
			elif (words[0] == 'begin' and nest == 0):
				archbegin = True
			if not archbegin:
				archprebody += line
				if (nest == 1 and last != 'nothing'):
					current += line
			else:
				archbody += line
		line = buffer.readline()
		count += 1
		
	while (not archend) and count <= 10:
		line2 = line.strip()
		words = line2.split(' ')
		if words:
			if (line2 == 'end architecture;' or line2 == 'end ' + archname + ';' or line2 == 'end architecture' + archname + ';'):
				archend = True
			#Ignore comments
		if (line2[0:2] != '--'):
			archbody += line
		line = buffer.readline()
		count += 1		
	
	while (line != ''):
			#Ignore comments
		if (line.strip()[0:2] != '--'):
			footer += line
		line = buffer.readline()
		
	for procedure in all:
		procedure_syntax = get_syntax(procedure)
		print procedure_syntax
	
#	currentpath = os.getcwd() + os.sep + target + '.vhd'
#	targetfile = open(currentpath,'w+')
#	targetfile.write(header)
#	for line in testlines:
#		if line.strip():
#			targetfile.write(archheader.replace(archname, archname + str(testcount)))
#			targetfile.write(line + '\n\n')
#			targetfile.write(archfooter.replace(archname, archname + str(testcount)) + '\n\n')
#			testcount += 1
#	targetfile.close()
		
	return entname, archname, all
