java8:	lex.yy.c y.tab.c
	gcc y.tab.c lex.yy.c -o java8 -O3
y.tab.c:	java8.y
	yacc -dv java8.y
lex.yy.c:	java8.l
	lex java8.l
clean:
	rm y.tab.c lex.yy.c y.output y.tab.h java8
