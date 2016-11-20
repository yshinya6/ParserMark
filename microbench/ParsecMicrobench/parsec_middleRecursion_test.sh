#!/bin/sh

start=0
end=61
filename=parsec_middleRecursion_result.txt
python parsec_middleRecursion_generator.py

cat<<EOF  > ${filename}
testGrammar,testInput,result,time
EOF

for var in `seq ${start} ${end}`
do
    cat ./app/middleRecursion${var}.txt > ./app/Main.hs
    stack build ParsecMicrobench
    echo middleRecursion${var}.txt,"\c" >> ${filename}
    echo ${var}.txt,"\c" >> ${filename}
    stack exec ParsecMicrobench-exe  < ../middle-recursion/test/${var}.txt >> ${filename}
    echo "\n" ${filename}
done
