#!/bin/sh

start=0
end=61
filename=scala_disjointChoice_result.txt
python scala_disjointChoice_generator.py

cat<<EOF  > ${filename}
testGrammar,testInput,result,time
EOF

for var in `seq ${start} ${end}`
do
    echo disjointChoice${var}.scala,"\c" >> ${filename}
    echo ${var}.txt,"\c" >> ${filename}
    scalac ./app/disjointChoice${var}.scala
    scala disjointChoice${var} ../test/${var}.txt >> ${filename}
done
