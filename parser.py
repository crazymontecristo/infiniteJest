#!/usr/bin/env python
 
#Splits by word, although it is just splitting by space.  So if there is a
#symbol it is included in the "word".  I think I am going to have to be more explicit.

sampleText = open('sample.txt')

sampleRead = sampleText.read()

wordSplit = sampleRead.split()

#print  wordSplit #prints the string


print wordSplit.count('the') #this counts the number of occurances of the word "the"

#This prints everything in the list on a new line
# for item in wordSplit:
#      print item

sampleText.close()

#Being more explicit
# split(str, num) 
# num = the # of lines made


