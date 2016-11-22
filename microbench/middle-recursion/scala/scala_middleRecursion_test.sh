#!/bin/sh

start=1
end=56
filename=scala_middleRecursion_result.txt
python scala_middleRecursion_generator.py

cat<<EOF  > ${filename}
testGrammar,testInput,result,time
EOF

for var in `seq ${start} ${end}`
do
  if [ `expr $var % 2` == 0 ]
  then
    echo middleRecursion${var}.scala,"\c" >> ${filename}
    echo ${var}.txt,"\c" >> ${filename}
    scalac ./app/middleRecursion${var}.scala
    scala middleRecursion${var} ../test/${var}.txt >> ${filename}
  fi
done
