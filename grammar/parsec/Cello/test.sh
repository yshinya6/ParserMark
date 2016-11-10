#!/bin/sh

echo pick out test@@@.cello to test%%%.cello
echo input @@@ ?  --Integer
read start
echo input %%% ?  --Integer
read end

filename=testResultParsec.txt

cat<<EOF  > ${filename}
{-test result:: test${start}.cello to test${end}.cello-}
EOF

for var in `seq ${start} ${end}`
do
    echo testfile:test${var}.cello >> ${filename}
    stack exec Cello < ./testInput/test${var}.cello >> ${filename}
    echo "\n" >> ${filename}
done
