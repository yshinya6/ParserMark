#!/bin/bash

g++ -std=c++11 math_nez.cc -O2 -o math_nez

for f in `ls *.in`
do
    echo $f
    ./math_nez $f
done
