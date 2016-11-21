#!/bin/sh

start=0
end=61
filename=scala_nest_result.txt
python scala_nest_generator.py

cat<<EOF  > ${filename}
testGrammar,testInput,result,time
EOF

for var in `seq ${start} ${end}`
do
    cat ./app/nest${var}.scala > ./app/Main.hs
    echo nest${var}.scala,"\c" >> ${filename}
    echo ${var}.txt,"\c" >> ${filename}
    scalac ./app/nest${var}.scala
    scala nest${var} ../nest/test/${var}.txt >> ${filename}
done
