import sys
import random
alphabets = []
codes = (('a','z'),('A','Z'),('0','9'))

# create alphabet list
for r in codes:
    chars = map(chr, range(ord(r[0]),ord(r[1])+1))
    alphabets.extend(chars)

for c in alphabets:
    print("\"{c}\"\treturn \'{c}\';".format(c=c))
