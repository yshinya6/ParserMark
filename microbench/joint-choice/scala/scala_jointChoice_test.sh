#!/bin/sh

start=0
end=61
filename=scala_jointChoice_result.txt
python scala_jointChoice_generator.py

cat<<EOF  > ${filename}
testGrammar,testInput,result,time
EOF

for var in `seq ${start} ${end}`
do
    echo jointChoice${var}.scala,"\c" >> ${filename}
    echo ${var}.txt,"\c" >> ${filename}
    scalac ./app/jointChoice${var}.scala
    scala jointChoice${var} ../test/${var}.txt >> ${filename}
done
