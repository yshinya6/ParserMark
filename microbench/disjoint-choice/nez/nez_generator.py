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
    ntermList = []
    grammarList = ["S=("]
    cnt = 0
    for alt in alphabets:
        ntermList.append("N{a}=\'{a}{a}\'\n".format(a=alt))
        grammarList.append("N{a}".format(a=alt))
        cnt = cnt + 1
        if cnt > n:
            break
        else:
            grammarList.append("/")
    grammarList.append(")*\n")
    f = open("{n}.nez".format(n=n), 'w')
    f.write("".join(grammarList))
    f.write("".join(ntermList))
    f.close
    # print "".join(grammarList)
