python javacc_generator.py
for x in {0..61};do
  java -cp ../../../grammar/javacc/javacc-6.0/bin/lib/javacc.jar: javacc -STATIC=false R${x}.jj
  javac R${x}.java
done
