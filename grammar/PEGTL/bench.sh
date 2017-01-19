#!/bin/sh

JS_PEGTL="./pegtl_js"
JAVA_PEGTL="./pegtl_java"
XML_PEGTL="./pegtl_xml"

JS="../../source/js"
JAVA="../../source/java"
XML="../../source/xml"

# echo 'java files'
# echo 'input, latency[ms]'
for file in ${JS}/*.js
do
  # echo "$file start"
  timeout -s 9 30 ${JS_PEGTL} ${file}
done

# echo '\nxml files'
# echo 'input, latency[ms]'
# for file in ${XML}/*.xml
# do
#   ${XML_PEGTL} ${file}
# done
