import sys
import random
alphabets = []
codes = (('a','z'),('A','Z'),('0','9'))

# create alphabet list
for r in codes:
    chars = map(chr, range(ord(r[0]),ord(r[1])+1))
    alphabets.extend(chars)

#print alphabets

header = open("header")
hCode = header.read()
header.close()

for n in range(0,62):
    grammarList = []
    cnt = 0
    for alt in alphabets:
        nextsymbol = "testParser" + alphabets[cnt + 1] if cnt < n else "testParser"
        if cnt == 0:
            grammarList.append("testParser = char \'{a}\' >> {b}\n".format(a=alt,b=nextsymbol))
        else :
            grammarList.append("testParser{a} = char \'{a}\' >> {b}\n".format(a=alt,b=nextsymbol))
        cnt = cnt + 1
        if cnt > n:
            break
    f = open("./app/middleRecursion{n}.txt".format(n=n), 'w')
    f.write("".join(hCode))
    f.write("".join(grammarList))
    f.close
    # print "".join(grammarList)
