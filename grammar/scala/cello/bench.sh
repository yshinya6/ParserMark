scalac src/main/scala/cello.scala
for file in `ls test/*`;do
    scala cello ${file}
done
