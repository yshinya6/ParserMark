#!/bin/sh

start=0
end=61
filename=parsec_choice_result.txt
python parsec_choice_generator.py

cat<<EOF  > ${filename}
testGrammar,testInput,result,time
EOF

for var in `seq ${start} ${end}`
do
    cat ./app/choice${var}.txt > ./app/Main.hs
    stack build ParsecMicrobench
    echo choice${var}.txt,"\c" >> ${filename}
    echo ${var}.txt,"\c" >> ${filename}
    stack exec ParsecMicrobench-exe  < ../choice/test/${var}.txt >> ${filename}
    echo "\n" ${filename}
done
