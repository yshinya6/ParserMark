import sys
import random
alphabets = []
codes = (('a','z'),('A','Z'),('0','9'))

# create alphabet list
for r in codes:
    chars = map(chr, range(ord(r[0]),ord(r[1])+1))
    alphabets.extend(chars)

# print alphabets

for n in range(0,62):
    ntermList = []
    cnt = 0
    for alt in alphabets:
        nextsymbol = "N" + alphabets[cnt + 1] if cnt < n else "Na"
        if cnt == 0:
            ntermList.append("N{a}=\'{a}\'{b}/!.\n".format(a=alt,b=nextsymbol))
        else :
            ntermList.append("N{a}=\'{a}\'{b}\n".format(a=alt,b=nextsymbol))
        cnt = cnt + 1
        if cnt > n:
            break
    f = open("{n}.nez".format(n=n), 'w')
    f.write("".join(ntermList))
    f.close
    # print "".join(grammarList)
    # print "".join(ntermList)
