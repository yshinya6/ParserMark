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
    prefix = "def Prefix= \'@\'\n"
    grammarList = ["def S: Parser[Any] =("]
    cnt = 0
    for alt in alphabets:
        ntermList.append("def N{a}: Parser[Any] =Prefix~\'{a}\'\n".format(a=alt))
        grammarList.append("N{a}".format(a=alt))
        cnt = cnt + 1
        if cnt > n:
            break
        else :
            grammarList.append("|")

    grammarList.append(").*\n")
    f = open("./app/jointChoice{n}.scala".format(n=n), 'w')
    f.write(iCode)
    f.write("object jointChoice{n}".format(n=n))
    f.write(hCode)
    f.write("".join(grammarList))
    f.write(prefix)
    f.write("".join(ntermList))
    f.write("def apply(input: String): Either[String, Any] = parseAll(S, input) match {")
    f.write(fCode)
    f.close()
    # print "".join(grammarList)
