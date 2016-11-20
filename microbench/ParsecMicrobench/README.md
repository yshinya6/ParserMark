# micro benchmark for Parsec
_the test result is available in CSV format_
## Usage
### for choice
* prepare input text datas(run microbench/choice/test/test_generator.py)
* run the script as follows
```
$ sh parsec_choice_test.sh
```
* the result is written in `parsec_choice_result`
* the grammar is generated in the app directory.

### for nest
* prepare input text datas(run microbench/nest/test/test_generator.py)
* run the script as follows
```
$ sh parsec_nest_test.sh
```
* the result is written in `parsec_nest_result`
* the grammar is generated in the app directory.

### for joint-choice
* prepare input text datas(run microbench/joint-choice/test/test_generator.py)
* run the script as follows
```
$ sh parsec_jointChoice_test.sh
```
* the result is written in `parsec_jointChoice_result`
* the grammar is generated in the app directory.


### for disjoint-choice
* prepare input text datas(run microbench/disjoint-choice/test/test_generator.py)
* run the script as follows
```
$ sh parsec_disjointChoice_test.sh
```
* the result is written in `parsec_disjointChoice_result`
* the grammar is generated in the app directory.

### for middle-recureion
* prepare input text datas(run microbench/middle-recureion/test/test_generator.py)
* run the script as follows
```
$ sh parsec_middleRecursion_test.sh
```
* the result is written in `parsec_middleRecursion_result`
* the grammar is generated in the app directory.
