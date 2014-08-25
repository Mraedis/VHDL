from sys import argv
import re

script = argv
teststring = '0001 - test failed    name: assertVectors (1/4) - expected: 0000 but received: 0101'
words = re.search('(?<=( - )).*', teststring)
words2 = teststring.split(' - ')[2]
words3 = re.search('(?<=name: )(.*)(?= -)', teststring).group(0)

print words.group(0)
print words2
print words3