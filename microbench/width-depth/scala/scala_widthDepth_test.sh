#!/bin/sh

start=0
end=61
filename=scala_width-depth_result.txt
python scala_widthDepth_generator.py

cat<<EOF  > ${filename}
testGrammar,testInput,result,time
EOF

for num in 1 10 20 30 40 50 60 70 80 90 100; do
  for var in `seq ${start} ${end}`
  do
    echo wd${num}-${var}.scala,"\c" >> ${filename}
    echo ${num}-${var}.txt,"\c" >> ${filename}
    scalac ./app/wd${num}-${var}.scala
    scala wd${num}_${var} ../test/${num}-${var}.txt >> ${filename}
    echo "\n" ${filename}
  done
done
