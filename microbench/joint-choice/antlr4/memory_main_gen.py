import sys
import random
alphabets = []
codes = (('a','z'),('A','Z'),('0','9'))

# create alphabet list
for r in codes:
    chars = map(chr, range(ord(r[0]),ord(r[1])+1))
    alphabets.extend(chars)

print alphabets

header = open("mem_header")
hCode = header.read()
header.close()
middle = open("mem_middle")
mCode = middle.read()
middle.close()
footer = open("mem_footer")
fCode = footer.read()
footer.close()

for n in range(0,62):
    spacing = "\t\t"
    main = open("Main{n}_mem.java".format(n=n), 'w')
    main.write(hCode)
    main.write("class Main{n}_mem {{ \n".format(n=n))
    main.write(mCode)
    main.write(spacing + "R{n}Lexer lexer = new R{n}Lexer(input);\n".format(n=n))
    main.write(spacing + "CommonTokenStream tokens = new CommonTokenStream(lexer);\n")
    main.write(spacing + "R{n}Parser parser = new R{n}Parser(tokens);\n".format(n=n))
    main.write(fCode)
