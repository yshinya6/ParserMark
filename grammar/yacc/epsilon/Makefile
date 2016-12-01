epsilon:	lex.yy.c y.tab.c
	gcc y.tab.c lex.yy.c -o epsilon -O3
y.tab.c:	epsilon.y
	yacc -dv epsilon.y
lex.yy.c:	epsilon.l
	lex epsilon.l
clean:
	rm y.tab.c lex.yy.c y.output y.tab.h epsilon
