#!/bin/sh

XML_LUA="xml-classic.lua"
JS_LUA="javascript4.lua"
JAVA_LUA="java8.lua"
XML="../../source/xml"
JAVA="../../source/java"
JS="../../source/js"

echo "\n#### bench xml files ####"
echo "input, latency[ms]"
for file in ${XML}/*.xml
do
  lua ${XML_LUA} ${file}
done

# echo "\n#### bench js files ####"
# echo "input, latency[ms]"
# for file in ${JS}/*.js
# do
#   lua ${JS_LUA} ${file}
# done

echo "\n#### bench java files ####"
echo "input, latency[ms]"
for file in ${JAVA}/*.java
do
  lua ${JAVA_LUA} ${file}
done
