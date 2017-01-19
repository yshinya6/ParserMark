### Files
* parser
  - parser/(target)\_(optimize)_(memo)?.js
    + target
      - parser from the PEG grammar specified in the (target).pegjs
    + optimize
      - speed / codesize
    + memo
      - make generated parser cache results

* pegjs definition
  - pegjs/(name).pegjs

### Usage
1. install node (https://nodejs.org/)
2. ```shell
node example.js -p parser/js_speed_memo.js -i ../../source/js/jquery-2.1.1.js -n 10```
  - "-p" or "-g"
    + path to parser
  - "-i"
    + path to input file
  - "-n"
    + repeat count
