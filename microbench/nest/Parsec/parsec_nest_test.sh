#!/bin/sh

start=0
end=61
filename=parsec_nest_result.txt
python parsec_nest_generator.py

cat<<EOF  > ${filename}
testGrammar,testInput,result,time
EOF

for var in `seq ${start} ${end}`
do
    cat ./app/nest${var}.txt > ./app/Main.hs
    stack build Parsec
    echo nest${var}.txt,"\c" >> ${filename}
    echo ${var}.txt,"\c" >> ${filename}
    stack exec ParsecMicrobench-exe  < ../test/${var}.txt >> ${filename}
done
