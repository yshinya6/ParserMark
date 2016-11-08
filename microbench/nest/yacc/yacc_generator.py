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
    cnt = 0
    startProduction = "N{n}".format(n=alphabets[n])
    for alt in alphabets:
        nextsymbol = "N" + alphabets[cnt - 1]
        if cnt == 0:
            ntermList.append("N{a}:{s}\'{a}\'|\'{a}\';\n".format(a=alt,s=startProduction))
        else :
            ntermList.append("N{a}:{b}\'{a}\';\n".format(a=alt,b=nextsymbol))
        cnt = cnt + 1
        if cnt > n:
            break

    f = open("R{n}.y".format(n=n), 'w')
    f.write(hCode)
    f.write("%start {s}\n".format(s=startProduction))
    f.write("%%\n")
    f.write("".join(ntermList))
    f.write(fCode)
    f.close()
    # print "".join(grammarList)
