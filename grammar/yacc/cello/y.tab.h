/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

#ifndef YY_YY_Y_TAB_H_INCLUDED
# define YY_YY_Y_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    VAR_LEN_PARAM = 258,
    EQ = 259,
    NE = 260,
    AND = 261,
    OR = 262,
    INC = 263,
    DEC = 264,
    STRING_TYPE = 265,
    INT_TYPE = 266,
    BOOLEAN_TYPE = 267,
    LONG_TYPE = 268,
    IF = 269,
    FOR = 270,
    ELSE = 271,
    RETURN = 272,
    FALSE = 273,
    TRUE = 274,
    IMPORT = 275,
    INT = 276,
    STRING = 277,
    NAME = 278,
    EOF_SYMBOL = 279,
    NULL_LITERAL = 280
  };
#endif
/* Tokens.  */
#define VAR_LEN_PARAM 258
#define EQ 259
#define NE 260
#define AND 261
#define OR 262
#define INC 263
#define DEC 264
#define STRING_TYPE 265
#define INT_TYPE 266
#define BOOLEAN_TYPE 267
#define LONG_TYPE 268
#define IF 269
#define FOR 270
#define ELSE 271
#define RETURN 272
#define FALSE 273
#define TRUE 274
#define IMPORT 275
#define INT 276
#define STRING 277
#define NAME 278
#define EOF_SYMBOL 279
#define NULL_LITERAL 280

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_Y_TAB_H_INCLUDED  */
