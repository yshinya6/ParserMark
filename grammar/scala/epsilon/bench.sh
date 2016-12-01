scalac src/main/scala/epsilon.scala
for file in `ls test/*`;do
    scala epsilon ${file}
done
