cello:	lex.yy.c y.tab.c
	gcc y.tab.c lex.yy.c -o cello -O3
y.tab.c:	cello.y
	yacc -dv cello.y
lex.yy.c:	cello.l
	lex cello.l
clean:
	rm y.tab.c lex.yy.c y.output y.tab.h cello
