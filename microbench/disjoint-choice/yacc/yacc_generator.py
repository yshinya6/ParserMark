import sys
import random
alphabets = []
codes = (('a','z'),('A','Z'),('0','9'))

# create alphabet list
for r in codes:
    chars = map(chr, range(ord(r[0]),ord(r[1])+1))
    alphabets.extend(chars)

print alphabets

header = open("header")
hCode = header.read()
header.close()
footer = open("footer")
fCode = footer.read()
footer.close()

for n in range(0,62):
    ntermList = []
    startProduction = ["S: N | S N;\n"]
    grammarList = ["N:"]
    cnt = 0
    for alt in alphabets:
        ntermList.append("N{a}:\'{a}\'\n".format(a=alt))
        grammarList.append("N{a}".format(a=alt))
        cnt = cnt + 1
        if cnt > n:
            break
        else:
            grammarList.append("|")
    grammarList.append(";\n")

    f = open("R{n}.y".format(n=n), 'w')
    f.write(hCode)
    f.write("".join(startProduction))
    f.write("".join(grammarList))
    f.write("".join(ntermList))
    f.write(fCode)
    f.close()
    # print "".join(grammarList)
