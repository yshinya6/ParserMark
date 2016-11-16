
for x in {0..61}; do
  # java -jar ../../../grammar/antlr4/antlr-4.5.3-complete.jar ./R${x}.g4
  javac ./Main${x}_mem.java
done
