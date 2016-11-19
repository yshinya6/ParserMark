import sys
import random
alphabets = []
codes = (('a','z'),('A','Z'),('0','9'))

# create alphabet list
for r in codes:
    chars = map(chr, range(ord(r[0]),ord(r[1])+1))
    alphabets.extend(chars)

print alphabets

for n in range(0,62):
    grammarList = ["A="]
    cnt = 0
    for alt in alphabets:
        grammarList.append("\'{a}{a}\'".format(a=alt))
        cnt = cnt + 1
        if cnt > n:
            break
        else:
            grammarList.append("/")
    print "".join(grammarList)
