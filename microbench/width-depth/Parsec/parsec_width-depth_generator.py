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

def gen_choice(char,upper):
    if upper == 1:
        return "((string \"{c}\") >> ".format(c=char)
    else:
        l = []
        for n in list(reversed(range(1,upper+1))):
            l.append("string \"{c}\"".format(c=char*n))
        return "(((" + ")<|>(".join(l) + ")) >> "

for width in [1,10,20,30,40,50,60,70,80,90,100]:
    for depth in range(0,62):
        ntermList = []
        cnt = 0
        for alt in alphabets:
            nextsymbol = "nonterminal" + alphabets[cnt + 1] + ")" if cnt < depth else "nonterminala)"
            choice = gen_choice(alt,width)
            if cnt == 0:
                ntermList.append("nonterminal{a} = try {c}{b} <|> eof\n".format(a=alt,c=choice,b=nextsymbol))
            else :
                ntermList.append("nonterminal{a} = {c}{b}\n".format(a=alt,c=choice,b=nextsymbol))
            cnt = cnt + 1
            if cnt > depth:
                break
        f = open("./app/{w}-{d}.hs".format(w=width,d=depth), 'w')
        f.write("".join(hCode))
        f.write("".join(ntermList))
        f.close
