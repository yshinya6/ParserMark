import sys
import random
alphabets = []
codes = (('a','z'),('A','Z'),('0','9'))

# create alphabet list
for r in codes:
    chars = map(chr, range(ord(r[0]),ord(r[1])+1))
    alphabets.extend(chars)

#print alphabets

Import = open("Import")
iCode = Import.read()
Import.close()
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
        #nextsymbol = "N" + alphabets[cnt - 1]
        nextsymbol = "N" + alphabets[cnt + 1] if cnt < n else "Na"
        if cnt == 0:
            #ntermList.append("def N{a}: Parser[Any] ={s}~\'{a}\'|\'{a}\'\n".format(a=alt,s=startProduction))
            ntermList.append("def N{a}: Parser[Any] =\'{a}\'~!{b}|\"!.\".r\n".format(a=alt,b=nextsymbol))
        else :
            ntermList.append("def N{a}: Parser[Any] =\'{a}\'~!{b}\n".format(a=alt,b=nextsymbol))
        cnt = cnt + 1
        if cnt > n:
            break

    f = open("./app/nest{n}.scala".format(n=n), 'w')
    f.write(iCode)
    f.write("object nest{n}".format(n=n))
    f.write(hCode)
    f.write("".join(ntermList))
    f.write("def apply(input: String): Either[String, Any] = parseAll(Na, input) match {")
    f.write(fCode)
    f.close()
    # print "".join(grammarList)
