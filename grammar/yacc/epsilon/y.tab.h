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
    GTEQ = 265,
    LTEQ = 266,
    IF = 267,
    FOR = 268,
    ELSE = 269,
    RETURN = 270,
    FALSE = 271,
    TRUE = 272,
    IMPORT = 273,
    INT = 274,
    STRING = 275,
    NAME = 276,
    EOF_SYMBOL = 277,
    NULL_LITERAL = 278,
    ELSE1 = 279,
    ELSE2 = 280,
    ELSE3 = 281,
    ELSE4 = 282,
    ELSE5 = 283,
    ELSE6 = 284,
    ELSE7 = 285,
    ELSE8 = 286,
    ELSE9 = 287,
    ELSEA = 288,
    ELSEB = 289,
    ELSEC = 290,
    LAMBDA = 291,
    FUNCTION = 292,
    VAR = 293,
    AADD = 294,
    ASUB = 295,
    AMUL = 296,
    ADIV = 297,
    AMOD = 298,
    ALEFTSHIFT = 299,
    ARIGHTSHIFT = 300,
    ALOGICALRIGHTSHIFT = 301,
    ABITAND = 302,
    ABITXOR = 303,
    ABITOR = 304
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
#define GTEQ 265
#define LTEQ 266
#define IF 267
#define FOR 268
#define ELSE 269
#define RETURN 270
#define FALSE 271
#define TRUE 272
#define IMPORT 273
#define INT 274
#define STRING 275
#define NAME 276
#define EOF_SYMBOL 277
#define NULL_LITERAL 278
#define ELSE1 279
#define ELSE2 280
#define ELSE3 281
#define ELSE4 282
#define ELSE5 283
#define ELSE6 284
#define ELSE7 285
#define ELSE8 286
#define ELSE9 287
#define ELSEA 288
#define ELSEB 289
#define ELSEC 290
#define LAMBDA 291
#define FUNCTION 292
#define VAR 293
#define AADD 294
#define ASUB 295
#define AMUL 296
#define ADIV 297
#define AMOD 298
#define ALEFTSHIFT 299
#define ARIGHTSHIFT 300
#define ALOGICALRIGHTSHIFT 301
#define ABITAND 302
#define ABITXOR 303
#define ABITOR 304

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_Y_TAB_H_INCLUDED  */
