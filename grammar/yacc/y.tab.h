/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton interface for Bison's Yacc-like parsers in C

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

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

/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
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
     IF = 268,
     FOR = 269,
     ELSE = 270,
     RETURN = 271,
     FALSE = 272,
     TRUE = 273,
     INT = 274,
     STRING = 275,
     NAME = 276,
     EOF_SYMBOL = 277
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
#define IF 268
#define FOR 269
#define ELSE 270
#define RETURN 271
#define FALSE 272
#define TRUE 273
#define INT 274
#define STRING 275
#define NAME 276
#define EOF_SYMBOL 277




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif

extern YYSTYPE yylval;

