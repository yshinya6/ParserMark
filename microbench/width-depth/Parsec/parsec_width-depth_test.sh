#!/bin/sh

start=0
end=61
filename=parsec_width-depth_result.txt

python parsec_width-depth_generator.py

cat<<EOF  > ${filename}
testGrammar,testInput,result,time
EOF

for num in 1 10 20 30 40 50 60 70 80 90 100; do
    for var in `seq ${start} ${end}`; do
        cat ./app/${num}-${var}.hs > ./app/Main.hs
        stack build Parsec
        echo ${num}-${var}.hs,"\c" >> ${filename}
        echo ${num}-${var}.txt,"\c" >> ${filename}
        stack exec ParsecMicrobench-exe  < ../test/${num}-${var}.txt >> ${filename}
        echo "\n" ${filename}
    done
done
