%{
#include <stdio.h>
#include <stdlib.h>
//#include "y.tab.h"

int yywrap(void) {
  return 1;
}

%}

space [ \n\r\t]+

comment //[^\n]*


%%

"a"	return 'a';
"b"	return 'b';
"c"	return 'c';
"d"	return 'd';
"e"	return 'e';
"f"	return 'f';
"g"	return 'g';
"h"	return 'h';
"i"	return 'i';
"j"	return 'j';
"k"	return 'k';
"l"	return 'l';
"m"	return 'm';
"n"	return 'n';
"o"	return 'o';
"p"	return 'p';
"q"	return 'q';
"r"	return 'r';
"s"	return 's';
"t"	return 't';
"u"	return 'u';
"v"	return 'v';
"w"	return 'w';
"x"	return 'x';
"y"	return 'y';
"z"	return 'z';
"A"	return 'A';
"B"	return 'B';
"C"	return 'C';
"D"	return 'D';
"E"	return 'E';
"F"	return 'F';
"G"	return 'G';
"H"	return 'H';
"I"	return 'I';
"J"	return 'J';
"K"	return 'K';
"L"	return 'L';
"M"	return 'M';
"N"	return 'N';
"O"	return 'O';
"P"	return 'P';
"Q"	return 'Q';
"R"	return 'R';
"S"	return 'S';
"T"	return 'T';
"U"	return 'U';
"V"	return 'V';
"W"	return 'W';
"X"	return 'X';
"Y"	return 'Y';
"Z"	return 'Z';
"0"	return '0';
"1"	return '1';
"2"	return '2';
"3"	return '3';
"4"	return '4';
"5"	return '5';
"6"	return '6';
"7"	return '7';
"8"	return '8';
"9"	return '9';
"@" return '@';
<<EOF>>     return EOF;


%%
