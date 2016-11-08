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
middle = open("middle")
mCode = middle.read()
middle.close()
footer = open("footer")
fCode = footer.read()
footer.close()
grammarHead = open("g4header")
gCode = grammarHead.read()
grammarHead.close()


for n in range(0,62):
    ntermList = []
    cnt = 0
    for alt in alphabets:
        nextsymbol = "n_" + str(ord(alphabets[cnt + 1])) if cnt < n else "n_97"
        if cnt == 0:
            ntermList.append("n_{a}:\'{b}\'{c}|EOF;\n".format(a=ord(alt),b=alt,c=nextsymbol))
        else:
            ntermList.append("n_{a}:\'{b}\'{c};\n".format(a=ord(alt),b=alt,c=nextsymbol))
        cnt = cnt + 1
        if cnt > n:
            break
    f = open("R{n}.g4".format(n=n), 'w')
    f.write("grammar R{n};\n".format(n=n))
    f.write(gCode)
    f.write("".join(ntermList))
    f.close()

    spacing = "\t\t\t"
    main = open("Main{n}.java".format(n=n), 'w')
    main.write(hCode)
    main.write("class Main{n} {{ \n".format(n=n))
    main.write(mCode)
    main.write(spacing + "R{n}Lexer lexer = new R{n}Lexer(input);\n".format(n=n))
    main.write(spacing + "CommonTokenStream tokens = new CommonTokenStream(lexer);\n")
    main.write(spacing + "R{n}Parser parser = new R{n}Parser(tokens);\n".format(n=n))
    main.write(fCode)
    # print "".join(grammarList)
