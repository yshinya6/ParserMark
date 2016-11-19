import sys
import random
alphabets = []
codes = (('a','z'),('A','Z'),('0','9'))

# create alphabet list
for r in codes:
    chars = map(chr, range(ord(r[0]),ord(r[1])+1))
    alphabets.extend(chars)

for upper in range(1,29):
    length = upper * 2
    f = open("{l}.txt".format(l=length), 'w')
    f.write('a' * length)
    f.close
