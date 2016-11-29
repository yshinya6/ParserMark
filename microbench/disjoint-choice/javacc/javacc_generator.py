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
hCode = header.readlines()
header.close()

for n in range(0,62):
    ntermList = []
    grammarList = ["void File():{}{("]
    cnt = 0
    for alt in alphabets:
        ntermList.append("void N{a}():{{}}{{\"{b}\" \"{b}\"}}\n".format(a=ord(alt),b=alt))
        grammarList.append("N{a}()".format(a=ord(alt)))
        cnt = cnt + 1
        if cnt > n:
            break
        else:
            grammarList.append("|")
    grammarList.append(")*}\n")

    f = open("R{n}.jj".format(n=n), 'w')
    for line in hCode:
        f.write(line.replace("XXX","R{n}".format(n=n)))
    f.write("".join(grammarList))
    f.write("".join(ntermList))
    f.close()
    # print "".join(grammarList)
