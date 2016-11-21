#!/bin/sh

start=0
end=61
filename=parsec_jointChoice_result.txt
python parsec_jointChoice_generator.py

cat<<EOF  > ${filename}
testGrammar,testInput,result,time
EOF

for var in `seq ${start} ${end}`
do
    cat ./app/jointChoice${var}.txt > ./app/Main.hs
    stack build Parsec
    echo jointChoice${var}.txt,"\c" >> ${filename}
    echo ${var}.txt,"\c" >> ${filename}
    stack exec ParsecMicrobench-exe  < ../test/${var}.txt >> ${filename}
    echo "\n" ${filename}
done
