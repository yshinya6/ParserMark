import sys
import random
alphabets = []
codes = (('a','z'),('A','Z'),('0','9'))

# create alphabet list
for r in codes:
    chars = map(chr, range(ord(r[0]),ord(r[1])+1))
    alphabets.extend(chars)

for width in [1,10,20,30,40,50,60,70,80,90,100]:
    for upper in range(0,62):
        inputList = []
        # repUnit = "".join(alphabets[:upper+1])
        repUnitList = []
        for char in alphabets[:upper+1]:
            repUnitList.append(char*(random.randint(1,width)))
        repUnit = "".join(repUnitList)
        print repUnit
        bound = 1000000 / len(repUnit)
        print bound
        for n in range(0,bound):
            inputList.append(repUnit)
        f = open("{w}-{d}.txt".format(w=width,d=upper), 'w')
        f.write("".join(inputList))
        f.close
