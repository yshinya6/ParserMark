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
    GR = 258,
    CAST = 259,
    VAR_LEN_PARAM = 260,
    EQ = 261,
    NE = 262,
    COMB_AND = 263,
    NON_COMB_AND = 264,
    COMB_VB = 265,
    NON_COMB_VB = 266,
    INC = 267,
    DEC = 268,
    MR = 269,
    AADD = 270,
    ASUB = 271,
    AMUL = 272,
    ADIV = 273,
    AMOD = 274,
    ALEFTSHIFT = 275,
    ARIGHTSHIFT = 276,
    ALOGICALRIGHTSHIFT = 277,
    ABITAND = 278,
    ABITXOR = 279,
    ABITOR = 280,
    RARROW = 281,
    LEFTSHIFT = 282,
    LTEQ = 283,
    GTEQ = 284,
    COMB_GT = 285,
    ANNO_DOT = 286,
    E_DIM_DOT = 287,
    TYPE_ARG_OP = 288,
    STRING_TYPE = 289,
    INT_TYPE = 290,
    BOOLEAN_TYPE = 291,
    DOUBLE_TYPE = 292,
    FLOAT_TYPE = 293,
    SHORT_TYPE = 294,
    CHAR_TYPE = 295,
    BYTE_TYPE = 296,
    LONG_TYPE = 297,
    VOID_TYPE = 298,
    E_DIM = 299,
    E_DIAMOND = 300,
    IF = 301,
    FOR = 302,
    ELSE = 303,
    RETURN = 304,
    FALSE = 305,
    TRUE = 306,
    ABSTRACT = 307,
    ASSERT = 308,
    BREAK = 309,
    CASE = 310,
    CATCH = 311,
    CLASS = 312,
    CONST = 313,
    CONTINUE = 314,
    DEFAULT = 315,
    DO = 316,
    ENUM = 317,
    EXTENDS = 318,
    FINAL = 319,
    FINALLY = 320,
    GOTO = 321,
    IMPLEMENTS = 322,
    IMPORT = 323,
    INSTANCEOF = 324,
    INTERFACE = 325,
    ANNO_INTERFACE = 326,
    NATIVE = 327,
    NEW = 328,
    NULL_LITERAL = 329,
    PACKAGE = 330,
    PRIVATE = 331,
    PROTECTED = 332,
    PUBLIC = 333,
    STATIC = 334,
    STRICTFP = 335,
    SUPER = 336,
    SWITCH = 337,
    SYNCHRONIZED = 338,
    THIS = 339,
    THROW = 340,
    THROWS = 341,
    TRANSIENT = 342,
    TRY = 343,
    VOLATILE = 344,
    WHILE = 345,
    INT = 346,
    STRING = 347,
    IDENTIFIER = 348,
    EOF_SYMBOL = 349,
    LONG = 350,
    FLOAT = 351,
    DOUBLE = 352,
    CHAR = 353
  };
#endif
/* Tokens.  */
#define GR 258
#define CAST 259
#define VAR_LEN_PARAM 260
#define EQ 261
#define NE 262
#define COMB_AND 263
#define NON_COMB_AND 264
#define COMB_VB 265
#define NON_COMB_VB 266
#define INC 267
#define DEC 268
#define MR 269
#define AADD 270
#define ASUB 271
#define AMUL 272
#define ADIV 273
#define AMOD 274
#define ALEFTSHIFT 275
#define ARIGHTSHIFT 276
#define ALOGICALRIGHTSHIFT 277
#define ABITAND 278
#define ABITXOR 279
#define ABITOR 280
#define RARROW 281
#define LEFTSHIFT 282
#define LTEQ 283
#define GTEQ 284
#define COMB_GT 285
#define ANNO_DOT 286
#define E_DIM_DOT 287
#define TYPE_ARG_OP 288
#define STRING_TYPE 289
#define INT_TYPE 290
#define BOOLEAN_TYPE 291
#define DOUBLE_TYPE 292
#define FLOAT_TYPE 293
#define SHORT_TYPE 294
#define CHAR_TYPE 295
#define BYTE_TYPE 296
#define LONG_TYPE 297
#define VOID_TYPE 298
#define E_DIM 299
#define E_DIAMOND 300
#define IF 301
#define FOR 302
#define ELSE 303
#define RETURN 304
#define FALSE 305
#define TRUE 306
#define ABSTRACT 307
#define ASSERT 308
#define BREAK 309
#define CASE 310
#define CATCH 311
#define CLASS 312
#define CONST 313
#define CONTINUE 314
#define DEFAULT 315
#define DO 316
#define ENUM 317
#define EXTENDS 318
#define FINAL 319
#define FINALLY 320
#define GOTO 321
#define IMPLEMENTS 322
#define IMPORT 323
#define INSTANCEOF 324
#define INTERFACE 325
#define ANNO_INTERFACE 326
#define NATIVE 327
#define NEW 328
#define NULL_LITERAL 329
#define PACKAGE 330
#define PRIVATE 331
#define PROTECTED 332
#define PUBLIC 333
#define STATIC 334
#define STRICTFP 335
#define SUPER 336
#define SWITCH 337
#define SYNCHRONIZED 338
#define THIS 339
#define THROW 340
#define THROWS 341
#define TRANSIENT 342
#define TRY 343
#define VOLATILE 344
#define WHILE 345
#define INT 346
#define STRING 347
#define IDENTIFIER 348
#define EOF_SYMBOL 349
#define LONG 350
#define FLOAT 351
#define DOUBLE 352
#define CHAR 353

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_Y_TAB_H_INCLUDED  */
