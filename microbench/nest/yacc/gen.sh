lex nest.l

for x in {0..61}; do
  yacc -dv R${x}.y
  gcc y.tab.c lex.yy.c -o R${x} -O3
  mv ./y.output ./table/R${x}.output
done
