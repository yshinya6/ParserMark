import sys
import random
alphabets = []
codes = (('a','z'),('A','Z'),('0','9'))

# create alphabet list
for r in codes:
    chars = map(chr, range(ord(r[0]),ord(r[1])+1))
    alphabets.extend(chars)

for upper in range(0,62):
    inputList = []
    repUnit = "".join(alphabets[:upper+1])
    print repUnit
    bound = 1000000 / len(repUnit)
    print bound
    for n in range(0,bound):
        inputList.append(repUnit)
    f = open("{u}.txt".format(u=upper), 'w')
    f.write("".join(inputList))
    f.close
