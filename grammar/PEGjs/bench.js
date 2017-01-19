var parserPath = "./parser/js_speed_memo.js";
var inputPath = "../../source/js/jquery-2.1.1.js";
var repeat = 1;
for (var i = 2; i < process.argv.length; i++) {
  switch (process.argv[i]) {
    case "-p":
    case "-g":
      i++;
      parserPath = "./" + process.argv[i];
      break;
    case "-i":
      i++;
      inputPath = "./" + process.argv[i];
      break;
    case "-n":
      i++;
      repeat = process.argv[i];
      break;
    default:

  }
}

var p = require(parserPath);
var fs = require("fs");
var file = fs.readFileSync(inputPath);
var input = file.toString("utf-8");
for (var i = 0; i < repeat; i++) {
  console.time(inputPath);
  p.parse(input);
  console.timeEnd(inputPath);
}
