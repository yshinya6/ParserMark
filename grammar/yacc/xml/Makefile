xml:	lex.yy.c y.tab.c
	gcc y.tab.c lex.yy.c -o xml -O3
y.tab.c:	xml.y
	yacc -dv xml.y
lex.yy.c:	xml.l
	lex xml.l
clean:
	rm y.tab.c lex.yy.c y.output y.tab.h xml
