/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison implementation for Yacc-like parsers in C

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

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.0.4"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* Copy the first part of user declarations.  */
#line 1 "java8.y" /* yacc.c:339  */

#include <stdio.h>
#include <stdlib.h>
#include <getopt.h>
#include <sys/time.h> // gettimeofday
#define YYDEBUG 1

int yyerror(char const *str);

int yylex(void);

uint64_t timer() {
struct timeval tv;
gettimeofday(&tv, NULL);
return tv.tv_sec * 1000 + tv.tv_usec / 1000;
}


#line 85 "y.tab.c" /* yacc.c:339  */

# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* In a future release of Bison, this section will be replaced
   by #include "y.tab.h".  */
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

/* Copy the second part of user declarations.  */

#line 332 "y.tab.c" /* yacc.c:358  */

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

#ifndef YY_ATTRIBUTE
# if (defined __GNUC__                                               \
      && (2 < __GNUC__ || (__GNUC__ == 2 && 96 <= __GNUC_MINOR__)))  \
     || defined __SUNPRO_C && 0x5110 <= __SUNPRO_C
#  define YY_ATTRIBUTE(Spec) __attribute__(Spec)
# else
#  define YY_ATTRIBUTE(Spec) /* empty */
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# define YY_ATTRIBUTE_PURE   YY_ATTRIBUTE ((__pure__))
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# define YY_ATTRIBUTE_UNUSED YY_ATTRIBUTE ((__unused__))
#endif

#if !defined _Noreturn \
     && (!defined __STDC_VERSION__ || __STDC_VERSION__ < 201112)
# if defined _MSC_VER && 1200 <= _MSC_VER
#  define _Noreturn __declspec (noreturn)
# else
#  define _Noreturn YY_ATTRIBUTE ((__noreturn__))
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN \
    _Pragma ("GCC diagnostic push") \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")\
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif


#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYSIZE_T yynewbytes;                                            \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / sizeof (*yyptr);                          \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, (Count) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYSIZE_T yyi;                         \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  43
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   4610

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  122
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  153
/* YYNRULES -- Number of rules.  */
#define YYNRULES  460
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  848

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   353

#define YYTRANSLATE(YYX)                                                \
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, without out-of-bounds checking.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,   121,     2,     2,     2,   119,     2,     2,
     100,   101,   108,   116,   102,   117,   107,   118,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,   113,   106,
     115,   103,   111,   112,    99,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,   109,     2,   110,   114,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,   104,     2,   105,   120,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    98
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,    41,    41,    42,    46,    47,    48,    49,    50,    51,
      60,    61,    64,    65,    66,    67,    70,    71,    74,    77,
      78,    79,    82,    83,    86,    87,    88,    94,    95,    99,
     100,   103,   104,   107,   108,   114,   115,   118,   119,   120,
     124,   125,   126,   127,   130,   131,   132,   133,   134,   135,
     136,   137,   138,   139,   140,   141,   144,   145,   147,   149,
     152,   154,   156,   159,   161,   163,   166,   167,   170,   171,
     174,   175,   176,   177,   180,   181,   182,   183,   184,   187,
     188,   189,   190,   191,   192,   193,   196,   197,   200,   201,
     202,   203,   204,   205,   206,   207,   212,   213,   214,   215,
     217,   219,   222,   223,   226,   227,   230,   231,   232,   233,
     234,   237,   238,   241,   242,   245,   246,   247,   248,   249,
     252,   253,   255,   257,   262,   263,   266,   267,   270,   271,
     274,   275,   276,   279,   280,   283,   286,   287,   290,   291,
     294,   295,   296,   301,   302,   307,   308,   330,   331,   332,
     334,   336,   337,   338,   340,   345,   346,   347,   350,   353,
     354,   355,   358,   359,   360,   363,   364,   367,   368,   372,
     376,   377,   378,   379,   380,   381,   382,   383,   386,   387,
     388,   389,   392,   393,   394,   395,   396,   397,   398,   399,
     408,   409,   410,   413,   414,   417,   418,   421,   422,   425,
     426,   427,   428,   429,   432,   433,   436,   439,   440,   445,
     446,   451,   452,   455,   456,   459,   462,   464,   465,   466,
     488,   490,   493,   494,   495,   496,   499,   500,   503,   504,
     507,   508,   511,   512,   515,   516,   519,   520,   521,   522,
     525,   526,   529,   530,   531,   532,   533,   536,   537,   540,
     541,   544,   545,   549,   550,   553,   554,   557,   558,   559,
     562,   563,   564,   565,   566,   567,   568,   569,   570,   571,
     572,   573,   574,   575,   576,   577,   578,   579,   580,   581,
     582,   587,   588,   589,   590,   593,   594,   597,   598,   601,
     602,   604,   606,   609,   610,   613,   616,   617,   618,   619,
     622,   623,   628,   631,   634,   635,   638,   639,   640,   641,
     644,   645,   654,   655,   657,   659,   661,   663,   666,   667,
     670,   671,   674,   675,   676,   677,   678,   679,   680,   681,
     682,   683,   684,   685,   688,   691,   692,   695,   696,   699,
     700,   703,   704,   707,   708,   711,   712,   715,   716,   717,
     720,   721,   722,   723,   724,   725,   728,   729,   730,   731,
     734,   735,   736,   739,   740,   741,   742,   745,   746,   747,
     748,   749,   750,   751,   752,   755,   758,   759,   760,   762,
     763,   766,   767,   768,   769,   772,   775,   776,   777,   779,
     780,   782,   783,   784,   785,   786,   787,   790,   791,   792,
     793,   794,   795,   796,   799,   800,   801,   804,   805,   808,
     809,   810,   813,   814,   817,   818,   821,   822,   825,   826,
     828,   830,   833,   834,   837,   838,   841,   842,   843,   844,
     845,   846,   849,   850,   854,   855,   858,   859,   860,   861,
     864,   865,   868,   871,   872,   873,   882,   883,   886,   888,
     891,   892,   897,   898,   899,   900,   901,   902,   903,   904,
     907
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 0
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "GR", "CAST", "VAR_LEN_PARAM", "EQ",
  "NE", "COMB_AND", "NON_COMB_AND", "COMB_VB", "NON_COMB_VB", "INC", "DEC",
  "MR", "AADD", "ASUB", "AMUL", "ADIV", "AMOD", "ALEFTSHIFT",
  "ARIGHTSHIFT", "ALOGICALRIGHTSHIFT", "ABITAND", "ABITXOR", "ABITOR",
  "RARROW", "LEFTSHIFT", "LTEQ", "GTEQ", "COMB_GT", "ANNO_DOT",
  "E_DIM_DOT", "TYPE_ARG_OP", "STRING_TYPE", "INT_TYPE", "BOOLEAN_TYPE",
  "DOUBLE_TYPE", "FLOAT_TYPE", "SHORT_TYPE", "CHAR_TYPE", "BYTE_TYPE",
  "LONG_TYPE", "VOID_TYPE", "E_DIM", "E_DIAMOND", "IF", "FOR", "ELSE",
  "RETURN", "FALSE", "TRUE", "ABSTRACT", "ASSERT", "BREAK", "CASE",
  "CATCH", "CLASS", "CONST", "CONTINUE", "DEFAULT", "DO", "ENUM",
  "EXTENDS", "FINAL", "FINALLY", "GOTO", "IMPLEMENTS", "IMPORT",
  "INSTANCEOF", "INTERFACE", "ANNO_INTERFACE", "NATIVE", "NEW",
  "NULL_LITERAL", "PACKAGE", "PRIVATE", "PROTECTED", "PUBLIC", "STATIC",
  "STRICTFP", "SUPER", "SWITCH", "SYNCHRONIZED", "THIS", "THROW", "THROWS",
  "TRANSIENT", "TRY", "VOLATILE", "WHILE", "INT", "STRING", "IDENTIFIER",
  "EOF_SYMBOL", "LONG", "FLOAT", "DOUBLE", "CHAR", "'@'", "'('", "')'",
  "','", "'='", "'{'", "'}'", "';'", "'.'", "'*'", "'['", "']'", "'>'",
  "'?'", "':'", "'^'", "'<'", "'+'", "'-'", "'/'", "'%'", "'~'", "'!'",
  "$accept", "Program", "TopLevel", "Annotations", "Annotation",
  "ElementValuePairList", "ElementValuePair", "ElementValue",
  "ElementValueList", "ElementValueArrayInitializer", "PackageDeclaration",
  "ImportDeclarations", "ImportDeclaration", "PackageName",
  "TypeDeclarations", "TypeDeclaration", "ClassDeclaration",
  "CommonModifier", "CommonModifiers", "OptSuperClass", "SuperClass",
  "OptSuperInterfaces", "SuperInterfaces", "OptClassBody", "ClassBody",
  "ClassBodyDeclarations", "ClassBodyDeclaration",
  "ClassMemberDeclaration", "EnumBody", "Enumerators", "Enumerator",
  "InterfaceDeclaration", "ExtendsInterfaces", "InterfaceBody",
  "InterfaceMemberDeclarations", "InterfaceMemberDeclaration",
  "AnnotationTypeBody", "AnnotationTypeMemberDeclarations",
  "AnnotationTypeMemberDeclaration", "AnnotationTypeElementDeclaration",
  "AnnotationTypeElementDefaultValue", "VariableDeclaration",
  "InitDeclList", "InitDecl", "VarName", "ArrayModifiers", "ArrayModifier",
  "Initializer", "InitializerList", "ArrayInitializer", "FieldDeclaration",
  "ConstantDeclaration", "MethodDeclaration", "BlockOrSemicolon",
  "InterfaceMethodDeclaration", "MethodParameters", "MethodParamList",
  "MethodParam", "VarParam", "Throws", "ConstructorDeclaration",
  "ConstructorBody", "ExplicitConstructorInvocation", "Type",
  "PrimitiveType", "UnannoPrimitiveType", "NumericType", "IntegralType",
  "FloatingPointType", "BooleanType", "ReferenceType", "ArrayType",
  "InitArrays", "InitArray", "ClassOrInterfaceType", "ClassType",
  "OptTypeParameters", "TypeParameters", "TypeParameterList",
  "TypeParameter", "UnannoTypeParameter", "ExtendsTypeModifier",
  "InterfaceTypeList", "TypeArguments", "TypeArgumentList", "TypeArgument",
  "ExtendsOrSuper", "NonArrayType", "ClassOrInterfaceTypes", "Block",
  "BlockStatements", "BlockStatement", "Statement", "TryStatement",
  "ResourceList", "Resources", "Resource", "OptCatches", "Catches",
  "Catch", "AddCatchParameter", "AddClassOrInterfaceTypes",
  "SwitchStatement", "SwitchBlock", "SwitchConditions", "SwitchCondition",
  "CaseBlock", "Expression", "OptExpression", "OptExpressionList",
  "ExpressionList", "AssignmentExpression", "AddAssignmentOperator",
  "ConstantExpression", "ConditionalExpression", "LogicalOrExpression",
  "LogicalAndExpression", "InclusiveOrExpression", "ExclusiveOrExpression",
  "AndExpression", "EqualityExpression", "RelationalExpression",
  "ShiftExpression", "AdditiveExpression", "MultiplicativeExpression",
  "UnaryExpression", "CastExpression", "CastOrGroup", "PostfixExpression",
  "ArgumentExpressionList", "PrimaryExpression", "ClassLiteral",
  "InstanceCreationExpression", "UnqualifiedInstanceCreationExpression",
  "FieldAccess", "ArrayAccess", "ArrayCreationExpression",
  "DimExpressions", "DimExpression", "OptDims", "Dims", "Dim",
  "MethodInvocation", "EmptyOrArgumentList", "MethodReference",
  "IDENTIFIEROrNew", "LambdaExpression", "LambdaParameters", "LambdaBody",
  "OptIDENTIFIER", "QualifiedName", "Literal", "Constant", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,   316,   317,   318,   319,   320,   321,   322,   323,   324,
     325,   326,   327,   328,   329,   330,   331,   332,   333,   334,
     335,   336,   337,   338,   339,   340,   341,   342,   343,   344,
     345,   346,   347,   348,   349,   350,   351,   352,   353,    64,
      40,    41,    44,    61,   123,   125,    59,    46,    42,    91,
      93,    62,    63,    58,    94,    60,    43,    45,    47,    37,
     126,    33
};
# endif

#define YYPACT_NINF -683

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-683)))

#define YYTABLE_NINF -444

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
    4383,  -683,    98,   125,  -683,   -12,   258,   272,  -683,   289,
    -683,  -683,  -683,  -683,  -683,  -683,  -683,  -683,   289,  -683,
    3239,  -683,   -16,  -683,   173,  4416,  -683,  4449,  -683,  -683,
    -683,  4482,  -683,    51,   390,   289,  -683,   333,   355,    51,
     367,   306,   105,  -683,  -683,   289,  -683,  4416,   383,  -683,
    4449,  -683,   395,   418,   423,   438,  -683,   218,   441,  -683,
     289,   450,  -683,   439,  -683,   270,   505,  3810,  -683,  -683,
     485,  2197,   417,  4449,    51,   390,    51,   367,  -683,   516,
    -683,   143,    64,  -683,  -683,   289,   390,  -683,  -683,   372,
     479,   476,   309,  -683,  -683,  -683,  -683,   289,   480,  -683,
    -683,  -683,  -683,  -683,  -683,  -683,  -683,  -683,  -683,  -683,
    2001,  -683,  4220,  -683,  3882,  -683,  -683,  -683,   492,     0,
    -683,  -683,  -683,  -683,  -683,  -683,  -683,     0,  3089,  3089,
      82,   483,  -683,  -683,  2038,  -683,    47,  -683,  -683,  -683,
     491,  -683,  -683,  -683,  2732,  -683,  2287,  3089,  3089,  3089,
    3089,  -683,   433,  -683,   500,  -683,   160,   474,   582,   594,
     593,   494,   600,   530,   199,   177,   425,   325,  -683,  -683,
    3089,   536,   378,  -683,  -683,  -683,   511,  -683,  -683,  -683,
    -683,   219,  -683,  -683,  -683,   441,   450,   505,  -683,   289,
    -683,  -683,  -683,   143,  -683,  -683,   508,   383,   869,   506,
     289,   326,  -683,   189,   336,  -683,   479,  3664,  -683,  -683,
     521,  -683,  -683,   176,   362,  -683,   514,  -683,    13,     0,
    -683,     0,  -683,  -683,   517,   568,   569,  2001,  -683,   511,
     289,   187,   178,   535,  2465,    82,   604,  2018,   295,   160,
     537,     0,   538,  -683,   474,  1047,  -683,   605,   489,  -683,
    -683,   148,  -683,  -683,  -683,  -683,  -683,   542,  -683,   539,
     588,  3178,   637,   641,  3089,  3089,  3089,  3089,  3089,  3089,
    3089,  2306,  3089,  3089,  3089,    76,  3089,  3089,  3089,  3089,
    3089,  -683,  -683,  -683,   165,  3178,  2821,  -683,  -683,   186,
     544,   534,  3178,  -683,   390,  -683,   480,   643,  -683,  1096,
    -683,   280,  -683,  -683,    -9,   975,     0,  -683,    75,  -683,
     289,  -683,  -683,   508,   326,   341,  -683,  3294,  -683,  -683,
    -683,  3956,  -683,  3738,  -683,  -683,  -683,  -683,   560,  2127,
     276,   374,  -683,   555,  3089,   182,   548,  -683,   260,  -683,
     565,  -683,  2911,  -683,  -683,   606,  -683,  -683,   508,   511,
    3178,   216,   187,  -683,    -2,  -683,   575,  -683,  -683,   483,
    -683,  4350,   451,  -683,   565,   295,   537,  -683,  -683,  -683,
    -683,  -683,  -683,  -683,  -683,  -683,  -683,  -683,  -683,  -683,
    -683,  -683,  3178,  3000,   254,  2376,  -683,   491,  -683,   615,
    -683,   549,  3089,  3089,   494,   600,   530,   199,   199,   177,
     177,  -683,   177,   177,   425,   562,  3089,   325,   325,  -683,
    -683,  -683,   190,  -683,   581,  -683,   567,  -683,  -683,   459,
    -683,  -683,   124,  -683,   618,  -683,    60,  -683,   585,  -683,
     570,   508,  -683,   289,   675,   584,   590,  1306,  -683,  -683,
    -683,  4021,  3368,  -683,  -683,  -683,  -683,  -683,  -683,   560,
    2176,  -683,  -683,  2306,  2306,  -683,    -9,  -683,  1009,  -683,
     372,  -683,  -683,   508,  -683,  3442,  -683,  -683,  3516,   609,
    2127,  -683,  -683,   292,   616,   597,  -683,   648,   601,   474,
    -683,  -683,  -683,   322,  -683,  2554,  -683,  -683,  -683,  -683,
    -683,  -683,   508,   607,  3178,  -683,  -683,   260,  -683,  -683,
     565,  -683,  4350,  -683,  -683,  -683,  -683,  -683,   614,  -683,
    -683,  -683,  -683,  3089,   594,   593,  3089,   425,   511,   511,
    -683,  -683,  3178,  -683,  -683,   197,   274,   511,  -683,  -683,
    -683,   289,  -683,  4086,    -8,   610,   619,  3178,  3178,   628,
     628,  1929,   623,   624,  3178,   377,   625,    27,  -683,  -683,
    -683,  4285,   620,   565,  -683,  1395,  -683,  -683,  -683,  -683,
     621,   785,   590,   609,  2216,  -683,  -683,   399,   590,  -683,
    -683,  -683,  -683,  -683,  -683,  3590,  -683,   292,   636,   246,
     590,   648,  2465,   626,  -683,  -683,  -683,   204,  -683,  -683,
     629,  -683,  4350,  -683,  -683,    34,  -683,   582,   425,  -683,
    -683,  -683,   638,   511,   640,  -683,  -683,  -683,   461,   639,
     289,  1211,   642,  -683,  3178,  2108,  -683,   635,   277,  -683,
     644,   645,   624,   652,  3178,  3178,   646,  4350,   691,   584,
    3178,  -683,   565,  -683,   647,  -683,  -683,  -683,    -8,   404,
     590,  -683,    -8,  -683,   246,   590,  1484,  -683,  -683,   403,
    -683,   246,   650,  -683,  -683,  2643,  -683,  -683,    43,   565,
    -683,  -683,   511,  -683,  -683,   479,    42,   511,  -683,  1573,
     411,  1662,   405,   830,  -683,   656,  4350,   653,   669,   657,
     662,  -683,  -683,  3178,  -683,  -683,   666,   667,   668,  -683,
    4350,   674,   -36,  -683,   672,   709,   145,  -683,   691,   679,
     647,   642,  -683,  -683,    -8,   642,  -683,  -683,   403,   246,
     676,  -683,  -683,   403,  -683,  -683,  -683,   565,  -683,  -683,
     680,   681,  -683,  1751,   511,   511,  -683,   386,   586,  1929,
     682,  3178,   156,  3178,   683,  3178,   677,   584,   695,   688,
    -683,  4153,  4511,   584,  -683,   727,   691,  1929,  -683,   642,
    -683,  -683,  -683,  -683,   403,  -683,  -683,  -683,  -683,  -683,
    -683,   687,   689,   511,   279,    81,   342,   748,   234,   692,
    3178,   694,  -683,   700,   401,  -683,  -683,   701,  3178,  -683,
    -683,  4511,   715,   708,    18,  -683,   584,  -683,  -683,  -683,
    -683,  -683,   716,   511,   717,   511,  1929,  3178,  3178,   713,
    3178,   718,  3089,   719,   275,  -683,  3178,  -683,   735,    66,
    -683,   584,   289,  -683,  -683,  -683,   724,  -683,   725,  -683,
     733,   737,  1929,   739,  -683,   734,  1840,  -683,  -683,  -683,
    -683,  -683,  -683,  -683,  -683,  -683,  1929,  1929,  -683,  1929,
    1840,  -683,  1840,  -683,  -683,  -683,  1840,  -683
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint16 yydefact[] =
{
       0,    48,     0,     0,    51,     0,     0,     0,    53,     0,
      47,    46,    45,    49,    50,    55,    52,    54,     0,    39,
       0,     2,    44,    10,     4,     5,    29,     6,    35,    37,
      56,     0,    38,   220,    61,     0,   450,     0,    34,   220,
       0,     0,    15,     1,     3,     0,    11,     7,    44,    30,
       8,    36,     0,     0,     0,     0,    57,     0,    58,   221,
       0,     0,    62,     0,    32,     0,   100,     0,    98,    28,
       0,     0,     0,     9,   220,    61,   220,     0,   223,   231,
     222,     0,     0,   226,   229,     0,    61,    59,   251,   215,
      63,   216,     0,    42,    31,   451,    33,     0,     0,   202,
     206,   205,   204,   201,   200,   199,   203,   192,   111,   119,
      44,   117,     0,   118,     0,   113,   115,   116,     0,   191,
     194,   195,   197,   198,   196,   190,   207,   208,     0,     0,
       0,     0,   456,   455,     0,   459,     0,   388,   454,   458,
     450,   452,   453,   457,     0,    14,     0,     0,     0,     0,
       0,    21,     0,    16,     0,    19,     0,    20,   335,   337,
     339,   341,   343,   345,   347,   350,   356,   360,   363,   374,
     389,   367,   381,   387,   391,   404,   392,   393,   394,   395,
     396,   382,   460,   386,    27,    58,     0,   100,    99,     0,
     230,   228,   225,     0,   224,    60,     0,     0,     0,   217,
       0,    94,    85,     0,     0,    86,   101,     0,    96,   193,
       0,   112,   114,   130,     0,   126,   128,   214,     0,   209,
     211,   210,   368,   369,     0,     0,     0,     0,   250,   249,
       0,     0,     0,     0,     0,   206,   450,     0,     0,   195,
       0,     0,     0,   313,   321,   363,   312,     0,   382,    26,
      22,     0,   370,   371,   372,   373,    12,     0,    13,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   375,   383,   384,     0,     0,     0,   433,   428,     0,
       0,     0,     0,   426,    61,    43,     0,   232,   227,     0,
      40,     0,   237,   236,   243,     0,     0,   242,     0,   240,
       0,   252,    92,    88,    95,     0,    84,     0,   102,   110,
     108,     0,   109,     0,   104,   106,   158,   107,     0,     0,
     130,     0,   424,     0,     0,     0,   131,   133,   132,   422,
       0,   145,     0,   213,   212,     0,   402,   399,    64,     0,
       0,     0,   420,   416,     0,   438,     0,   410,    18,   192,
     444,     0,     0,   162,     0,   191,   207,   376,   377,   378,
     326,   327,   323,   324,   325,   328,   329,   330,   331,   332,
     333,   322,     0,     0,     0,     0,    25,     0,    17,     0,
     400,     0,     0,     0,   342,   344,   346,   348,   349,   353,
     354,   355,   352,   351,   357,     0,     0,   361,   362,   364,
     365,   366,     0,   409,     0,   406,     0,   432,   318,     0,
     441,   440,     0,   434,     0,   397,     0,   390,     0,   405,
       0,     0,    97,     0,   233,    49,   450,     0,    66,    78,
      76,     0,     0,    68,    70,    77,    74,    75,    73,     0,
       0,    71,   219,     0,     0,   244,   245,   239,     0,   238,
     218,    89,    93,    90,    83,     0,    87,    82,     0,     0,
       0,   103,   105,   130,     0,     0,   146,   122,     0,   334,
     425,   134,   423,   130,   127,     0,   129,   137,   136,   403,
     407,    65,    64,     0,     0,   417,   414,   421,   415,   439,
       0,   445,     0,   165,   320,   447,   446,   442,     0,   379,
      24,    23,   401,     0,   338,   340,     0,   358,     0,     0,
     413,   385,     0,   435,   398,     0,     0,     0,   412,    41,
     234,     0,    72,     0,     0,     0,     0,   314,     0,   448,
     448,     0,     0,    55,     0,     0,     0,   450,   253,   280,
     259,     0,     0,     0,   260,     0,   255,   257,   275,   265,
       0,   382,   450,     0,     0,    67,    69,     0,   450,   247,
     248,   246,   241,    91,    81,     0,    80,   130,     0,     0,
       0,   122,     0,     0,   135,   140,   138,     0,   408,   418,
       0,   166,     0,   164,   163,     0,   380,   336,   359,   429,
     319,   436,     0,   411,     0,   427,   235,   161,     0,     0,
       0,     0,     0,   174,     0,   316,   315,     0,     0,   449,
       0,     0,     0,     0,     0,     0,     0,     0,   291,     0,
       0,   278,     0,   258,   124,   254,   256,   279,     0,     0,
     450,   144,     0,    79,     0,     0,     0,   157,   147,     0,
     155,     0,     0,   123,   120,     0,   141,   419,     0,     0,
     437,   430,     0,   159,   160,   169,     0,   388,   178,     0,
       0,     0,   381,   382,   176,     0,     0,     0,     0,     0,
     317,   274,   261,     0,   273,   272,     0,     0,     0,   276,
       0,     0,     0,   287,     0,     0,   284,   293,   291,     0,
     125,     0,   170,   143,     0,     0,   175,   151,     0,     0,
     253,   148,   149,     0,   121,   142,   139,     0,   167,   431,
       0,     0,   179,     0,     0,     0,   180,     0,     0,     0,
       0,   314,   130,   314,     0,     0,     0,     0,     0,     0,
     285,     0,     0,     0,   294,   281,   292,     0,   172,     0,
     171,   177,   152,   153,     0,   156,   150,   168,   184,   182,
     181,     0,     0,     0,     0,     0,     0,   263,   130,     0,
     314,     0,   262,     0,     0,   302,   277,     0,     0,   286,
     288,     0,   300,     0,     0,   283,     0,   266,   173,   154,
     185,   183,     0,     0,     0,     0,     0,   314,   316,     0,
     316,     0,     0,     0,     0,   304,     0,   289,   300,     0,
     296,     0,     0,   298,   282,   188,     0,   186,     0,   264,
       0,     0,     0,     0,   267,     0,   308,   303,   305,   290,
     297,   299,   295,   301,   189,   187,     0,     0,   270,     0,
     306,   310,   309,   271,   269,   268,   307,   311
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -683,  -683,   826,    16,   755,  -683,   595,  -124,  -683,  -683,
    -683,   829,    39,   819,   324,   402,   337,   -21,   101,   671,
    -683,   -72,  -683,   366,  -168,  -292,  -410,  -683,   673,  -683,
     545,   -43,   684,   571,  -683,   541,   789,  -683,   754,  -683,
     291,   255,  -205,   533,  -356,  -683,   540,  -473,  -683,   523,
    -683,  -176,  -170,  -551,  -683,  -446,   346,   373,   347,  -519,
    -683,  -525,  -683,   528,   -65,   -89,   179,  -683,  -683,  -683,
    -181,   -68,  -104,   298,   -59,   572,   169,   -24,  -683,   690,
     800,  -683,  -683,  -128,  -683,   429,   430,  -683,   -95,   456,
    -566,  -431,  -495,  -683,  -683,  -683,   147,   192,   195,  -628,
    -683,   114,  -683,  -683,  -683,    92,    57,   428,  -682,  -271,
     617,  -683,  -683,   112,   -64,   387,   524,   522,   654,   660,
     651,   290,   221,  -254,   288,   427,  -683,  -683,  -683,  -683,
     308,  -683,  -683,  -273,  -683,  -683,  -683,  -683,   574,  -683,
    -197,  -319,  -683,    20,  -683,   509,  -683,  -683,  -683,   388,
      -5,  -683,  -683
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,    20,    21,   110,    23,   152,   153,   154,   251,   155,
      24,    25,    26,    37,    27,    28,   550,    30,   551,    86,
      87,    61,    62,   490,   491,   442,   443,   444,    93,   204,
     205,    32,    98,   208,   323,   324,    68,   114,   115,   116,
     583,   552,   214,   215,   216,   336,   337,   486,   587,   487,
     446,   117,   447,   648,   327,   534,   362,   363,   594,   612,
     448,   613,   669,   553,   119,   120,   156,   122,   123,   124,
     125,   126,   219,   220,   127,    89,    58,   450,    82,    83,
      84,   190,   434,   199,   308,   309,   455,   231,    90,   554,
     555,   556,   557,   558,   629,   692,   693,   695,   696,   697,
     783,   784,   559,   775,   804,   805,   842,   560,   617,   679,
     680,   243,   382,   478,   244,   158,   159,   160,   161,   162,
     163,   164,   165,   166,   167,   245,   169,   170,   171,   287,
     172,   173,   174,   175,   176,   177,   178,   352,   353,   496,
     338,   339,   179,   293,   180,   423,   246,   247,   507,   620,
     181,   182,   183
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      38,    88,   206,   186,    41,   331,   230,   157,   503,    59,
      56,   415,   586,    42,   196,    59,    22,   307,   429,   482,
     404,   209,   250,   221,   113,   468,   195,   579,   300,   812,
      38,   325,   566,   312,   354,   482,    22,   326,    88,   659,
      72,    48,   332,    48,   217,   671,   623,    48,   717,   769,
      59,   771,    59,  -443,   453,    91,   232,   343,   566,    45,
     649,   232,    91,    48,    49,   740,    48,    35,   744,   228,
     741,   113,   454,    81,   525,   229,   240,   812,   610,   238,
      91,    36,   157,    18,    57,   241,    49,   674,   799,    48,
     401,    56,    91,   707,   192,   525,   611,    18,   711,    18,
     712,    31,   485,   723,   356,   457,   405,    91,   203,    91,
     358,   813,    18,   702,   224,   820,   638,   706,   744,   701,
     384,    31,   642,   705,   636,   708,    31,   483,    31,    91,
     297,   644,   713,   306,   651,   218,   483,   221,   209,   248,
     631,   311,   286,   218,   591,   461,   462,   325,    31,   233,
     227,    31,   517,   326,   233,   497,   414,   752,   753,   831,
     227,   422,   756,   428,   322,   566,   193,   526,   112,   366,
     157,   349,   365,   575,    31,   194,   748,   458,   482,   750,
     751,   286,   716,   329,    91,   749,   459,   406,   526,   225,
     754,    33,   259,    91,   704,    91,   288,   420,   198,   709,
     332,   694,    91,   789,   274,    71,   306,   275,    66,    81,
    -292,   198,    70,   301,   305,   112,   209,   421,    34,   198,
     332,   313,   431,   198,   788,    91,   480,   269,   270,   335,
     198,   332,   248,   289,   767,   218,    79,   218,   412,   503,
     636,     5,    18,   185,   567,   187,   121,   351,    78,   348,
     385,   290,   787,   386,   218,    18,   445,   218,   413,   420,
     480,   511,   598,   529,   331,   334,    91,   260,   271,   770,
     479,   355,   569,   570,   445,    18,   333,   307,   332,   421,
     322,    18,   314,    36,   230,   334,    18,   227,    18,   121,
     601,   121,   636,   121,    91,   573,   350,   470,   508,   329,
      56,   819,   591,   718,   332,    91,   655,   198,   321,   656,
     272,    79,    91,   121,   273,    18,    91,    18,    91,   286,
     332,   157,   218,   239,    91,   494,   291,   838,   292,    80,
     802,   203,   610,    18,   463,   803,   332,    29,   361,   217,
      56,   843,   844,   334,   845,   227,   335,   797,   634,    50,
     646,    39,   647,   518,   335,   509,    91,    29,   639,    18,
     793,   757,    29,    95,    29,    40,   332,   603,   351,   492,
     335,    73,   519,   452,   530,    18,   475,   121,    96,    18,
     827,   218,    36,   682,    29,   334,   121,    29,   306,   306,
     683,    18,   533,   306,    18,   841,   367,   602,   604,   445,
     441,   334,   201,   197,   111,   198,   121,    91,    18,   841,
      29,   847,    69,    70,   202,   847,   239,   564,   441,   198,
      56,    18,   445,   795,   321,   445,   286,   700,    91,    51,
     299,   334,   561,   278,   201,   527,    91,    91,   315,    64,
      18,   316,   317,   279,   280,    91,   464,   465,    91,    91,
     121,   111,    51,    91,   415,   429,   802,    60,   653,   412,
      91,   803,    65,    91,   340,    91,   227,   763,   341,   227,
     227,    67,   606,   634,   305,    51,   340,   627,   121,   413,
     476,   437,    18,   670,   121,   284,   227,   285,    74,   335,
     399,   400,   724,   402,   403,   725,   121,    91,   168,   335,
     121,   340,   121,   289,    85,   641,   340,   646,   121,   647,
     703,    75,   727,   335,   285,   665,    76,   344,   157,   344,
    -216,   290,   198,   184,    70,   700,    91,   821,    91,   823,
      56,    77,   445,  -216,   256,   257,   267,   268,   348,   599,
     121,   276,   277,   441,   320,    94,    91,   605,   282,   283,
     561,    88,   501,   502,    92,   222,   223,   397,   398,    91,
     521,   522,   663,   502,   407,   408,   441,   198,    97,   441,
      91,    56,   242,   168,   252,   253,   254,   255,    95,   189,
     227,   200,  -216,    70,   207,   213,   261,    91,  -216,   286,
     226,   425,   262,   335,   234,   118,   291,   281,   292,   764,
     766,   258,   263,   592,   264,    91,   673,   412,   265,   266,
     561,   286,   299,   310,   330,   426,   239,   342,   427,   198,
     121,   121,    91,   661,   345,   346,   347,    95,   357,   121,
    -443,   383,   121,   121,   592,   387,   440,   121,   368,   369,
     210,   561,   118,   425,   121,   390,   389,   121,   392,   121,
     393,   424,   433,   473,   440,    56,   477,   334,   483,   412,
     320,   168,   513,   489,   561,   242,   561,   765,   499,    56,
     427,    91,   512,   516,   519,   524,   441,   520,   527,    95,
     528,   121,   719,   782,   531,    91,   720,   721,   437,   391,
     533,   168,   168,   168,   168,   168,   168,   168,   581,   168,
     168,   168,   577,   168,   168,   409,   410,   411,   582,   580,
     614,   584,   121,   416,   418,   596,   676,   589,   561,   615,
     430,   619,   808,   624,   625,   630,   633,   637,   690,   645,
     121,   660,   654,   662,   239,   328,    91,    91,   479,   657,
     664,   681,   686,   121,   761,   762,   611,   694,   335,   340,
     684,   685,   689,   833,   121,   451,   714,   729,    48,   731,
      56,   168,   732,   733,   522,   364,   735,   739,   736,   737,
     488,   121,   742,   451,   743,   768,    91,    46,   493,   440,
     747,   774,   755,   792,   335,   794,   758,   759,   777,   772,
     239,   778,   786,   790,   239,   791,   796,    48,   798,   289,
     800,   801,   440,    46,   806,   440,   121,    91,   810,   811,
     504,   506,   168,   816,   822,   818,  -216,   290,  -216,   168,
     168,   561,   815,   817,   824,   239,   151,   449,   830,  -216,
     834,   835,   826,   168,   836,   561,    46,   561,   837,   505,
     839,   561,   690,   781,   289,   449,    44,   840,   239,   469,
     239,   328,   388,    47,    63,   121,   294,   474,   588,   295,
     466,  -216,   290,  -216,   472,    46,   188,   432,   212,   121,
     677,   296,   652,   484,  -216,   593,   481,   498,  -216,   608,
     609,   191,   460,   298,  -216,   286,   571,   572,   780,   500,
     745,   532,   291,   746,   292,   809,   828,   846,   451,   302,
     597,   151,   239,   419,    99,   100,   101,   102,   103,   104,
     105,   106,   440,   488,   825,   515,   514,   396,   394,   672,
     121,   451,   590,  -216,   451,   395,   495,     0,   621,  -216,
     286,   523,     0,     0,     0,     0,     0,   728,     0,   292,
     168,     0,     0,   168,     0,     0,     0,     0,     0,     0,
     600,     0,     0,     0,     0,     0,     0,     0,    46,     0,
       0,     0,    36,     0,     0,   616,   618,     0,    18,   563,
     449,     0,   626,    46,     0,     0,     0,     0,   474,     0,
     303,   304,    46,     0,     0,     0,     0,     0,     0,   151,
       0,     0,     0,   449,     0,     0,   449,     0,   578,     0,
       0,   628,     0,     0,     0,   239,     0,     0,     0,   168,
      99,   100,   101,   102,   103,   104,   105,   106,     0,   239,
       0,   239,     0,     0,     0,   239,     0,     0,     0,     0,
     595,   451,     0,     0,     0,   650,     0,     0,     0,     0,
       0,     0,   675,   418,    99,   100,   101,   102,   103,   104,
     105,   106,   687,   688,     0,     0,    46,     0,   699,     0,
      46,   595,   370,   371,   372,   373,   374,   375,   376,   377,
     378,   379,   380,     0,    18,     0,     0,     0,     0,   632,
       0,     0,     0,   488,     0,   698,     0,   456,     0,     0,
      46,     0,   578,     0,     0,     0,     0,     0,     0,     0,
     650,     0,    36,   449,     0,   650,    46,   650,    18,     0,
       0,   734,     0,     0,     0,     0,     0,     0,     0,     0,
     658,   304,     0,     0,     0,     0,     0,     0,     0,    57,
       0,    99,   100,   101,   102,   103,   104,   105,   106,   107,
     151,     0,     0,   678,     0,     0,     0,     0,     1,     0,
     381,     0,     0,     2,     0,   691,     0,     0,     3,   616,
       4,   616,     0,   773,   650,   650,     6,     7,     8,   650,
       0,     0,    10,    11,    12,   435,    14,     0,     0,    15,
       0,     0,     0,    16,     0,    17,     0,     0,     0,   436,
       0,     0,     0,   776,     0,    18,     0,     0,   616,   785,
     437,   438,   439,     0,   730,     0,   807,     0,     0,     0,
     650,     0,     0,     0,     0,     0,     0,     0,   738,     0,
       0,     0,     0,   128,   129,   616,   418,     0,   418,   168,
       0,     0,     0,     0,   829,     0,     0,     0,     0,     0,
       0,     0,   814,     0,   198,     0,    99,   235,   101,   102,
     103,   104,   105,   106,   359,     0,     0,   535,   536,     0,
     537,   132,   133,     1,   538,   539,     0,   832,     2,   691,
     540,     0,   541,     3,     0,     4,     0,     0,     0,     0,
       0,     0,     0,     8,   134,   135,     0,    10,    11,    12,
      13,    14,   666,   542,   543,   667,   544,     0,    16,   545,
      17,   546,   138,   139,   547,     0,     0,   141,   142,   143,
      18,   237,     0,     0,     0,   437,   668,   549,   128,   129,
       0,     0,     0,     0,     0,     0,     0,   147,   148,     0,
       0,   149,   150,     0,     0,     0,     0,   151,     0,     0,
       0,    99,   235,   101,   102,   103,   104,   105,   106,   359,
       0,     0,   535,   536,     0,   537,   132,   133,     1,   538,
     539,     0,     0,     2,     0,   540,     0,   541,     3,     0,
       4,     0,     0,     0,     0,     0,     0,     0,     8,   134,
     135,     0,    10,    11,    12,    13,    14,   136,   542,   543,
     137,   544,     0,    16,   545,    17,   546,   138,   139,   547,
       0,     0,   141,   142,   143,    18,   237,   128,   129,     0,
     437,   548,   549,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   147,   148,     0,     0,   149,   150,     0,     0,
      99,   235,   101,   102,   103,   104,   105,   106,   359,     0,
       0,   535,   536,     0,   537,   132,   133,     1,   538,   539,
       0,     0,     2,     0,   540,     0,   541,     3,     0,     4,
       0,     0,     0,     0,     0,     0,     0,     8,   134,   135,
       0,    10,    11,    12,    13,    14,   136,   542,   543,   137,
     544,     0,    16,   545,    17,   546,   138,   139,   547,     0,
       0,   141,   142,   143,    18,   237,   128,   129,     0,   437,
     635,   549,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   147,   148,     0,     0,   149,   150,     0,     0,    99,
     235,   101,   102,   103,   104,   105,   106,   359,     0,     0,
     535,   536,     0,   537,   132,   133,     1,   538,   539,     0,
       0,     2,     0,   540,     0,   541,     3,     0,     4,     0,
       0,     0,     0,     0,     0,     0,     8,   134,   135,     0,
      10,    11,    12,    13,    14,   136,   542,   543,   137,   544,
       0,    16,   545,    17,   546,   138,   139,   547,     0,     0,
     141,   142,   143,    18,   237,   128,   129,     0,   437,   710,
     549,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     147,   148,     0,     0,   149,   150,     0,     0,    99,   235,
     101,   102,   103,   104,   105,   106,   359,     0,     0,   535,
     536,     0,   537,   132,   133,     1,   538,   539,     0,     0,
       2,     0,   540,     0,   541,     3,     0,     4,     0,     0,
       0,     0,     0,     0,     0,     8,   134,   135,     0,    10,
      11,    12,    13,    14,   136,   542,   543,   137,   544,     0,
      16,   545,    17,   546,   138,   139,   547,     0,     0,   141,
     142,   143,    18,   237,   128,   129,     0,   437,   722,   549,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   147,
     148,     0,     0,   149,   150,     0,     0,    99,   235,   101,
     102,   103,   104,   105,   106,   359,     0,     0,   535,   536,
       0,   537,   132,   133,     1,   538,   539,     0,     0,     2,
       0,   540,     0,   541,     3,     0,     4,     0,     0,     0,
       0,     0,     0,     0,     8,   134,   135,     0,    10,    11,
      12,    13,    14,   136,   542,   543,   137,   544,     0,    16,
     545,    17,   546,   138,   139,   547,     0,     0,   141,   142,
     143,    18,   237,   128,   129,     0,   437,   726,   549,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   147,   148,
       0,     0,   149,   150,     0,     0,    99,   235,   101,   102,
     103,   104,   105,   106,   359,     0,     0,   535,   536,     0,
     537,   132,   133,     1,   538,   539,     0,     0,     2,     0,
     540,     0,   541,     3,     0,     4,     0,     0,     0,     0,
       0,     0,     0,     8,   134,   135,     0,    10,    11,    12,
      13,    14,   136,   542,   543,   137,   544,     0,    16,   545,
      17,   546,   138,   139,   547,     0,     0,   141,   142,   143,
      18,   237,   128,   129,     0,   437,   760,   549,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   147,   148,     0,
       0,   149,   150,     0,     0,    99,   235,   101,   102,   103,
     104,   105,   106,   359,     0,     0,   535,   536,     0,   537,
     132,   133,     1,   538,   539,     0,     0,     2,     0,   540,
       0,   541,     3,     0,     4,     0,     0,     0,     0,     0,
       0,     0,     8,   134,   135,     0,    10,    11,    12,    13,
      14,   136,   542,   543,   137,   544,     0,    16,   545,    17,
     546,   138,   139,   547,     0,     0,   141,   142,   143,    18,
     237,   128,   129,     0,   437,     0,   549,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   147,   148,     0,     0,
     149,   150,     0,     0,    99,   130,   101,   102,   103,   104,
     105,   106,   131,     0,     0,   535,   536,     0,   537,   132,
     133,     0,   538,   539,     0,     0,     0,     0,   540,     0,
     541,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   134,   135,     0,     0,     0,     0,     0,     0,
     136,   542,   622,   137,   544,     0,     0,   545,     0,   546,
     138,   139,   547,     0,     0,   141,   142,   143,     0,   237,
     128,   129,     0,   437,     0,   549,    99,   100,   101,   102,
     103,   104,   105,   106,     0,   147,   148,     0,     0,   149,
     150,     0,     0,    99,   235,   101,   102,   103,   104,   105,
     106,   359,     0,     0,     0,     0,     0,     0,   132,   133,
       1,   198,     0,    99,   100,   101,   102,   103,   104,   105,
     106,     0,     4,     0,     0,     0,     0,     0,     0,     0,
       8,   134,   135,     0,    10,    11,    12,    13,    14,   136,
      18,    15,   137,     0,     0,    16,     0,    17,     0,   138,
     139,   236,     0,     0,   141,   142,   143,    18,   237,   360,
     128,   129,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    36,     0,     0,   147,   148,     0,    18,   149,   150,
       0,     0,     0,    99,   235,   101,   102,   103,   104,   105,
     106,   359,     0,     0,     0,     0,     0,     0,   132,   133,
       1,     0,    99,   100,   101,   102,   103,   104,   105,   106,
     107,     0,     4,     0,     0,     0,     0,     0,     0,     0,
       8,   134,   135,     0,    10,    11,    12,    13,    14,   136,
       0,    15,   137,     0,     0,    16,     0,    17,     0,   138,
     139,   236,     0,     0,   141,   142,   143,    18,   237,   128,
     129,    99,   100,   101,   102,   103,   104,   105,   106,   107,
      36,     0,     0,     0,   147,   148,    18,     0,   149,   150,
       0,     0,    99,   130,   101,   102,   103,   104,   105,   106,
     131,     0,     0,     0,     0,     0,     0,   132,   133,     0,
       0,    99,   100,   101,   102,   103,   104,   105,   106,   107,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   568,
     134,   135,     0,     0,     0,    18,     0,     0,   136,     0,
       0,   137,     0,     0,     0,     0,     0,     0,   138,   139,
     140,     0,     0,   141,   142,   143,    18,   144,   145,   128,
     129,   146,     0,     0,     0,     0,     0,     0,     0,   640,
       0,     0,     0,   147,   148,    18,     0,   149,   150,     0,
       0,     0,    99,   130,   101,   102,   103,   104,   105,   106,
     131,     0,     0,     0,     0,     0,     0,   132,   133,     0,
       0,    99,   100,   101,   102,   103,   104,   105,   106,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     134,   135,     0,     0,     0,     0,     0,     0,   136,     0,
       0,   137,     0,     0,     0,     0,     0,     0,   138,   139,
      36,     0,     0,   141,   142,   143,    18,   144,   128,   129,
       0,   146,   249,     0,     0,     0,     0,     0,     0,    36,
       0,     0,     0,   147,   148,    18,     0,   149,   150,     0,
       0,    99,   130,   101,   102,   103,   104,   105,   106,   131,
       0,     0,     0,     0,     0,     0,   132,   133,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   134,
     135,     0,     0,     0,     0,     0,     0,   136,     0,     0,
     137,     0,     0,     0,     0,     0,     0,   138,   139,    36,
       0,     0,   141,   142,   143,    18,   144,   128,   129,     0,
     146,   510,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   147,   148,     0,     0,   149,   150,     0,     0,
      99,   130,   101,   102,   103,   104,   105,   106,   131,     0,
       0,     0,     0,     0,     0,   132,   133,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   134,   135,
       0,     0,     0,     0,     0,     0,   136,     0,     0,   137,
       0,     0,     0,     0,     0,     0,   138,   139,    36,     0,
       0,   141,   142,   143,    18,   144,   128,   129,     0,   146,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   147,   148,     0,     0,   149,   150,     0,     0,    99,
     130,   101,   102,   103,   104,   105,   106,   131,     0,     0,
       0,     0,     0,     0,   132,   133,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   134,   135,     0,
       0,     0,     0,     0,     0,   136,     0,     0,   137,     0,
       0,     0,     0,     0,     0,   138,   139,   236,     0,     0,
     141,   142,   143,     0,   237,   128,   129,     0,   485,   585,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     147,   148,     0,     0,   149,   150,     0,     0,    99,   130,
     101,   102,   103,   104,   105,   106,   131,     0,     0,     0,
       0,     0,     0,   132,   133,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   134,   135,     0,     0,
       0,     0,     0,     0,   136,     0,     0,   137,     0,     0,
       0,     0,     0,     0,   138,   139,   236,     0,     0,   141,
     142,   143,     0,   237,   128,   129,     0,   485,   715,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   147,
     148,     0,     0,   149,   150,     0,     0,    99,   235,   101,
     102,   103,   104,   105,   106,   131,     0,     0,     0,     0,
       0,     0,   132,   133,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   134,   135,     0,     0,     0,
       0,     0,     0,   136,     0,     0,   137,     0,     0,     0,
       0,     0,     0,   138,   139,   236,     0,     0,   141,   142,
     143,    18,   237,   128,   129,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   147,   148,
       0,     0,   149,   150,     0,     0,    99,   130,   101,   102,
     103,   104,   105,   106,   131,     0,     0,     0,     0,     0,
       0,   132,   133,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   134,   135,     0,     0,     0,     0,
       0,     0,   136,     0,     0,   137,     0,     0,     0,     0,
       0,     0,   138,   139,   236,     0,     0,   141,   142,   143,
       0,   237,   417,   128,   129,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   147,   148,     0,
       0,   149,   150,     0,     0,     0,    99,   130,   101,   102,
     103,   104,   105,   106,   131,     0,     0,     0,     0,     0,
       0,   132,   133,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   134,   135,     0,     0,     0,     0,
       0,     0,   136,     0,     0,   137,     0,     0,     0,     0,
       0,     0,   138,   139,   236,     0,     0,   141,   142,   143,
       0,   237,   128,   129,     0,   485,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   147,   148,     0,
       0,   149,   150,     0,     0,    99,   130,   101,   102,   103,
     104,   105,   106,   131,     0,     0,     0,     0,     0,     0,
     132,   133,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   134,   135,     0,     0,     0,     0,     0,
       0,   136,     0,     0,   137,     0,     0,     0,     0,     0,
       0,   138,   139,   236,     0,     0,   141,   142,   143,     0,
     237,   128,   129,     0,   437,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   147,   148,     0,     0,
     149,   150,     0,     0,    99,   130,   101,   102,   103,   104,
     105,   106,   131,     0,     0,     0,     0,     0,     0,   132,
     133,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   134,   135,     0,     0,     0,     0,     0,     0,
     136,     0,     0,   137,     0,     0,     0,     0,     0,     0,
     138,   139,    36,     0,     0,   141,   142,   143,     0,   144,
     128,   129,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   147,   148,     0,     0,   149,
     150,     0,     0,    99,   130,   101,   102,   103,   104,   105,
     106,   131,     0,     0,     0,     0,     0,     0,   132,   133,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    43,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   134,   135,     0,     0,     0,     0,     0,     0,   136,
       0,     0,   137,     0,     0,     0,     0,     0,     0,   138,
     139,   236,     0,     0,   141,   142,   143,     0,   237,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     1,     0,     0,   147,   148,     2,     0,   149,   150,
       0,     3,     0,     4,     0,     0,     0,     5,     0,     6,
       7,     8,     0,     0,     9,    10,    11,    12,    13,    14,
       0,     0,    15,     0,     0,     0,    16,    57,    17,    99,
     100,   101,   102,   103,   104,   105,   106,   107,    18,     0,
       0,     0,     0,     0,     0,    19,     1,     0,     0,     0,
       0,     2,     0,     0,     0,     0,     3,     0,     4,     0,
       0,     0,     0,     0,     6,     7,     8,     0,     0,     0,
      10,    11,    12,   435,    14,     0,     0,    15,     0,     0,
       0,    16,     0,    17,     0,     0,     0,   436,     0,     0,
       0,     0,     0,    18,     0,     0,     0,     0,   437,   467,
     439,    57,     0,    99,   100,   101,   102,   103,   104,   105,
     106,   107,     0,     0,     0,     0,     0,     0,     0,     0,
       1,     0,     0,     0,     0,     2,     0,     0,     0,     0,
       3,     0,     4,     0,     0,     0,     0,     0,     6,     7,
       8,     0,     0,     0,    10,    11,    12,   435,    14,     0,
       0,    15,     0,     0,     0,    16,     0,    17,     0,     0,
       0,   436,     0,     0,     0,     0,     0,    18,     0,     0,
       0,     0,   437,   565,   439,    57,     0,    99,   100,   101,
     102,   103,   104,   105,   106,   107,     0,     0,     0,     0,
       0,     0,     0,     0,     1,     0,     0,     0,     0,     2,
       0,     0,     0,     0,     3,     0,     4,     0,     0,     0,
       0,     0,     6,     7,     8,     0,     0,     0,    10,    11,
      12,   435,    14,     0,     0,    15,     0,     0,     0,    16,
       0,    17,     0,     0,     0,   436,     0,     0,     0,     0,
       0,    18,     0,     0,     0,     0,   437,   574,   439,    57,
       0,    99,   100,   101,   102,   103,   104,   105,   106,   107,
       0,     0,     0,     0,     0,     0,     0,     0,     1,     0,
       0,     0,     0,     2,     0,     0,     0,     0,     3,     0,
       4,     0,     0,     0,     0,     0,     6,     7,     8,     0,
       0,     0,    10,    11,    12,   435,    14,     0,     0,    15,
       0,     0,     0,    16,     0,    17,     0,     0,     0,   436,
       0,     0,     0,     0,     0,    18,     0,     0,     0,     0,
     437,   576,   439,    57,     0,    99,   100,   101,   102,   103,
     104,   105,   106,   107,     0,     0,     0,     0,     0,     0,
       0,     0,     1,     0,     0,     0,     0,     2,     0,     0,
       0,     0,     3,     0,     4,     0,     0,     0,     0,     0,
       6,     7,     8,     0,     0,     0,    10,    11,    12,   435,
      14,     0,     0,    15,     0,     0,     0,    16,     0,    17,
       0,     0,     0,   436,     0,     0,     0,     0,     0,    18,
       0,     0,     0,     0,   437,   643,   439,    57,     0,    99,
     100,   101,   102,   103,   104,   105,   106,   107,     0,     0,
       0,     0,     0,     0,     0,     0,     1,     0,     0,     0,
       0,     2,     0,     0,     0,     0,     3,     0,     4,     0,
       0,     0,     0,     0,     6,     7,     8,     0,     0,     0,
      10,    11,    12,    13,    14,     0,     0,    15,     0,     0,
       0,    16,     0,    17,     0,     0,     0,    36,     0,     0,
       0,     0,     0,    18,     0,     0,     0,     0,     0,   318,
     319,    57,     0,    99,   100,   101,   102,   103,   104,   105,
     106,   107,     0,     0,     0,     0,     0,     0,     0,     0,
       1,     0,     0,     0,     0,     2,     0,     0,     0,     0,
       3,     0,     4,     0,     0,     0,     0,     0,     6,     7,
       8,     0,     0,     0,    10,    11,    12,    13,    14,     0,
       0,    15,     0,     0,     0,    16,     0,    17,     0,     0,
       0,    36,     0,     0,     0,     0,     0,    18,     0,     0,
       0,     0,     0,   471,   319,    99,   100,   101,   102,   103,
     104,   105,   106,   107,     0,     0,     0,     0,     0,     0,
       0,     0,     1,     0,     0,     0,     0,     2,     0,     0,
       0,     0,     3,     0,     4,     0,     0,     0,     0,     0,
       6,     7,     8,     0,     0,     0,    10,    11,    12,    13,
      14,     0,     0,    15,     0,     0,     0,    16,     0,    17,
       0,     0,     0,    36,     0,     0,     0,     0,     0,    18,
       0,     0,     0,     0,     0,   108,   109,    99,   100,   101,
     102,   103,   104,   105,   106,   107,     0,     0,     0,     0,
       0,     0,     0,     0,     1,     0,     0,     0,     0,     2,
       0,     0,     0,     0,     3,     0,     4,     0,     0,     0,
       0,     0,     6,     7,     8,     0,     0,     0,    10,    11,
      12,    13,    14,     0,     0,    15,     0,     0,     0,    16,
       0,    17,     0,     0,     0,    36,     0,     0,     0,     0,
       0,    18,     0,     0,     0,     0,     0,   211,   109,    57,
       0,    99,   100,   101,   102,   103,   104,   105,   106,   107,
       0,     0,     0,     0,     0,     0,     0,     0,     1,     0,
       0,     0,     0,    52,     0,     0,     0,     0,    53,     0,
       4,     0,     0,     0,     0,     0,    54,    55,     8,     0,
       0,     0,    10,    11,    12,    13,    14,     0,     0,    15,
       0,     0,     0,    16,     0,    17,     0,     0,     0,    36,
       0,     0,     0,     0,    57,    18,    99,   100,   101,   102,
     103,   104,   105,   106,   107,     0,     0,     0,     0,     0,
       0,     0,     0,     1,     0,     0,     0,     0,    52,     0,
       0,     0,     0,    53,     0,     4,     0,     0,     0,     0,
       0,    54,    55,     8,     0,     0,     0,    10,    11,    12,
      13,    14,     0,     0,    15,     0,     0,     0,    16,     0,
      17,     0,     0,     0,   562,     0,     0,     0,     0,     0,
      18,    99,   100,   101,   102,   103,   104,   105,   106,   107,
       0,     0,     0,     0,     0,     0,     0,     0,     1,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       4,     0,     0,     0,     0,     0,     0,     0,     8,     0,
       0,     0,    10,    11,    12,    13,    14,     0,     0,    15,
       0,     0,     0,    16,     0,    17,     0,     0,     0,    36,
       0,     0,     0,     0,     0,    18,     0,   607,    99,   100,
     101,   102,   103,   104,   105,   106,   107,     0,     0,     0,
       0,     0,     0,     0,     0,     1,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     4,     0,     0,
       0,     0,     0,     0,     0,     8,     0,     0,     0,    10,
      11,    12,    13,    14,     0,     0,    15,     0,     0,     0,
      16,     0,    17,     0,     0,     0,    36,     0,     0,     0,
       0,     0,    18,     0,   779,    99,   100,   101,   102,   103,
     104,   105,   106,   107,     0,     0,     0,     0,     0,     0,
       0,     0,     1,     0,     0,     0,     0,    52,     0,     0,
       0,     0,    53,     0,     4,     0,     0,     0,     0,     0,
      54,    55,     8,     0,     0,     0,    10,    11,    12,    13,
      14,     0,     0,    15,     0,     0,     0,    16,     0,    17,
       0,     0,     0,    36,     0,     0,     0,     0,     0,    18,
      99,   100,   101,   102,   103,   104,   105,   106,   107,     0,
       0,     0,     0,     0,     0,     0,     0,     1,     0,     0,
       0,     0,    52,     0,     0,     0,     0,    53,     0,     4,
       0,     0,     0,     0,     0,     0,     0,     8,     0,     0,
       0,    10,    11,    12,    13,    14,     0,     0,    15,     0,
       0,     0,    16,     0,    17,     0,     0,     0,    36,     0,
       0,     0,     0,     0,    18,    99,   100,   101,   102,   103,
     104,   105,   106,   107,     0,     0,     0,     0,     0,     0,
       0,     0,     1,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     4,     0,     0,     0,     0,     0,
       0,     0,     8,     0,     0,     0,    10,    11,    12,    13,
      14,     0,     0,    15,     0,     1,     0,    16,     0,    17,
       2,     0,     0,    36,     0,     3,     0,     4,     0,    18,
       0,     5,     0,     6,     7,     8,     0,     0,     9,    10,
      11,    12,    13,    14,     0,     0,    15,     0,     1,     0,
      16,     0,    17,     2,     0,     0,     0,     0,     3,     0,
       4,     0,    18,     0,     5,     0,     6,     7,     8,    19,
       0,     0,    10,    11,    12,    13,    14,     0,     0,    15,
       0,     1,     0,    16,     0,    17,     2,     0,     0,     0,
       0,     3,     0,     4,     0,    18,     0,     0,     0,     6,
       7,     8,    19,     0,     0,    10,    11,    12,    13,    14,
       0,     0,    15,     0,     1,     0,    16,     0,    17,    52,
       0,     0,     0,     0,    53,     0,     4,     0,    18,     0,
       0,     0,    54,    55,     8,    19,     0,     0,    10,    11,
      12,    13,    14,     1,     0,    15,     0,     0,     0,    16,
       0,    17,     0,     0,     0,     4,     0,     0,     0,     0,
       0,    18,     0,     8,     0,     0,     0,    10,    11,    12,
      13,    14,     0,     0,    15,     0,     0,     0,    16,     0,
      17,     0,     0,     0,    36,     0,     0,     0,     0,     0,
      18
};

static const yytype_int16 yycheck[] =
{
       5,    60,    97,    75,     9,   210,   134,    71,   364,    33,
      31,   284,   485,    18,    86,    39,     0,   198,   291,   338,
     274,   110,   146,   127,    67,   317,    85,   473,   196,    11,
      35,   207,   442,   201,   231,   354,    20,   207,    97,     5,
      45,    25,    44,    27,    44,   611,   541,    31,     5,   731,
      74,   733,    76,    26,    63,    60,    14,    44,   468,    75,
     579,    14,    67,    47,    25,   101,    50,    79,   696,   134,
     106,   114,    81,    57,    14,   134,   144,    11,    86,   144,
      85,    93,   146,    99,    33,   144,    47,   612,   770,    73,
     271,   112,    97,   644,    30,    14,   104,    99,   649,    99,
     651,     0,   104,   669,   232,    30,    30,   112,    92,   114,
     234,    93,    99,   638,    32,   797,   562,   642,   746,   638,
     248,    20,   568,   642,   555,   644,    25,    93,    27,   134,
     189,   577,   651,   198,   580,   119,    93,   241,   227,   144,
     113,   200,   100,   127,   500,   313,   314,   323,    47,   107,
     134,    50,   406,   323,   107,   352,   284,   708,   709,    93,
     144,   289,   713,   291,   207,   575,   102,   107,    67,   237,
     234,   230,   237,   465,    73,   111,   701,   102,   497,   704,
     705,   100,   655,   207,   189,   704,   111,   111,   107,   107,
     709,    93,    32,   198,   640,   200,   176,    73,    33,   645,
      44,    56,   207,   754,    27,   100,   271,    30,    39,   193,
      65,    33,   107,   197,   198,   114,   305,    93,    93,    33,
      44,   201,   294,    33,   749,   230,    44,    28,    29,   213,
      33,    44,   237,    14,   729,   219,    93,   221,    73,   595,
     671,    68,    99,    74,   449,    76,    67,   231,    30,   229,
     102,    32,   747,   105,   238,    99,   299,   241,    93,    73,
      44,   385,   516,   431,   469,   109,   271,   107,    69,   113,
     334,    93,   453,   454,   317,    99,   100,   458,    44,    93,
     323,    99,    93,    93,   412,   109,    99,   271,    99,   110,
      93,   112,   723,   114,   299,   463,   109,   321,    44,   323,
     321,   796,   658,   659,    44,   310,   102,    33,   207,   105,
     111,    93,   317,   134,   115,    99,   321,    99,   323,   100,
      44,   385,   306,   144,   329,   109,   107,   822,   109,   111,
      55,   315,    86,    99,   314,    60,    44,     0,   237,    44,
     361,   836,   837,   109,   839,   329,   330,   113,   553,    25,
     104,    93,   106,   412,   338,   101,   361,    20,   563,    99,
      81,   717,    25,    93,    27,    93,    44,    93,   352,   349,
     354,    47,    93,    93,   433,    99,   100,   198,   108,    99,
     105,   365,    93,   106,    47,   109,   207,    50,   453,   454,
     113,    99,   100,   458,    99,   826,   101,   525,   526,   442,
     299,   109,    93,    31,    67,    33,   227,   412,    99,   840,
      73,   842,   106,   107,   105,   846,   237,   441,   317,    33,
     441,    99,   465,    81,   323,   468,   100,   632,   433,    27,
     104,   109,   437,   108,    93,    93,   441,   442,   102,   106,
      99,   105,   106,   118,   119,   450,   105,   106,   453,   454,
     271,   114,    50,   458,   727,   728,    55,    67,   582,    73,
     465,    60,   107,   468,   102,   470,   450,    81,   106,   453,
     454,   104,   531,   678,   458,    73,   102,   100,   299,    93,
     106,   104,    99,   611,   305,   107,   470,   109,    93,   473,
     269,   270,    81,   272,   273,    84,   317,   502,    71,   483,
     321,   102,   323,    14,    63,   106,   102,   104,   329,   106,
     106,    93,   107,   497,   109,   610,    93,   219,   582,   221,
      31,    32,    33,   106,   107,   730,   531,   798,   533,   800,
     551,    93,   575,    44,   101,   102,     6,     7,   518,   519,
     361,   116,   117,   442,   207,   106,   551,   527,    12,    13,
     555,   610,   101,   102,   104,   128,   129,   267,   268,   564,
     101,   102,   101,   102,   276,   277,   465,    33,    63,   468,
     575,   592,   144,   146,   147,   148,   149,   150,    93,    63,
     564,   102,    93,   107,   104,    93,   112,   592,    99,   100,
     107,    57,    10,   577,   103,    67,   107,   170,   109,   727,
     728,   101,     8,   502,    11,   610,   611,    73,   114,     9,
     615,   100,   104,   107,    93,    81,   437,   103,    84,    33,
     441,   442,   627,   603,   107,    57,    57,    93,    93,   450,
      26,    26,   453,   454,   533,    93,   299,   458,   101,   101,
     112,   646,   114,    57,   465,    57,   107,   468,    11,   470,
       9,   107,     9,    93,   317,   676,   101,   109,    93,    73,
     323,   234,   113,    57,   669,   237,   671,    81,    93,   690,
      84,   676,    57,   111,    93,    57,   575,   110,    93,    93,
     110,   502,   662,   742,     9,   690,   666,   667,   104,   261,
     100,   264,   265,   266,   267,   268,   269,   270,   101,   272,
     273,   274,    93,   276,   277,   278,   279,   280,    60,    93,
     100,   110,   533,   285,   286,   101,   615,   110,   723,   100,
     292,    93,   781,   100,   100,   100,   106,   106,   627,    93,
     551,    93,   106,    93,   555,   207,   741,   742,   802,   110,
     101,   106,    90,   564,   724,   725,   104,    56,   732,   102,
     106,   106,   106,   812,   575,   299,   106,   101,   742,   106,
     781,   334,    93,   106,   102,   237,   100,    93,   101,   101,
     342,   592,   100,   317,    65,    93,   781,    22,   350,   442,
     101,   104,   106,   763,   768,   765,   106,   106,    93,   106,
     611,   103,    65,   106,   615,   106,    48,   781,   106,    14,
     106,   101,   465,    48,   103,   468,   627,   812,    93,   101,
     382,   383,   385,   793,   101,   795,    31,    32,    33,   392,
     393,   826,   106,   106,   106,   646,    71,   299,    93,    44,
     106,   106,   113,   406,   101,   840,    81,   842,   101,   383,
     101,   846,   741,   742,    14,   317,    20,   113,   669,   321,
     671,   323,   257,    24,    35,   676,   185,   329,   492,   186,
     315,    31,    32,    33,   323,   110,    77,   296,   114,   690,
     615,   187,   581,   340,    44,   502,   336,   354,    93,   533,
     533,    81,   310,   193,    99,   100,   456,   458,   741,   361,
     698,   435,   107,   698,   109,   781,   804,   840,   442,    30,
     513,   146,   723,   286,    35,    36,    37,    38,    39,    40,
      41,    42,   575,   485,   802,   393,   392,   266,   264,   611,
     741,   465,   494,    93,   468,   265,   352,    -1,   540,    99,
     100,   422,    -1,    -1,    -1,    -1,    -1,   107,    -1,   109,
     513,    -1,    -1,   516,    -1,    -1,    -1,    -1,    -1,    -1,
     522,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   203,    -1,
      -1,    -1,    93,    -1,    -1,   537,   538,    -1,    99,   441,
     442,    -1,   544,   218,    -1,    -1,    -1,    -1,   450,    -1,
     111,   112,   227,    -1,    -1,    -1,    -1,    -1,    -1,   234,
      -1,    -1,    -1,   465,    -1,    -1,   468,    -1,   470,    -1,
      -1,   545,    -1,    -1,    -1,   826,    -1,    -1,    -1,   582,
      35,    36,    37,    38,    39,    40,    41,    42,    -1,   840,
      -1,   842,    -1,    -1,    -1,   846,    -1,    -1,    -1,    -1,
     502,   575,    -1,    -1,    -1,   579,    -1,    -1,    -1,    -1,
      -1,    -1,   614,   615,    35,    36,    37,    38,    39,    40,
      41,    42,   624,   625,    -1,    -1,   301,    -1,   630,    -1,
     305,   533,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    -1,    99,    -1,    -1,    -1,    -1,   551,
      -1,    -1,    -1,   655,    -1,   629,    -1,   112,    -1,    -1,
     335,    -1,   564,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     644,    -1,    93,   575,    -1,   649,   351,   651,    99,    -1,
      -1,   683,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     592,   112,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    33,
      -1,    35,    36,    37,    38,    39,    40,    41,    42,    43,
     385,    -1,    -1,   615,    -1,    -1,    -1,    -1,    52,    -1,
     103,    -1,    -1,    57,    -1,   627,    -1,    -1,    62,   731,
      64,   733,    -1,   735,   708,   709,    70,    71,    72,   713,
      -1,    -1,    76,    77,    78,    79,    80,    -1,    -1,    83,
      -1,    -1,    -1,    87,    -1,    89,    -1,    -1,    -1,    93,
      -1,    -1,    -1,   737,    -1,    99,    -1,    -1,   770,   743,
     104,   105,   106,    -1,   676,    -1,   778,    -1,    -1,    -1,
     754,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   690,    -1,
      -1,    -1,    -1,    12,    13,   797,   798,    -1,   800,   802,
      -1,    -1,    -1,    -1,   806,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   786,    -1,    33,    -1,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    -1,    -1,    46,    47,    -1,
      49,    50,    51,    52,    53,    54,    -1,   811,    57,   741,
      59,    -1,    61,    62,    -1,    64,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    73,    74,    -1,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    -1,    87,    88,
      89,    90,    91,    92,    93,    -1,    -1,    96,    97,    98,
      99,   100,    -1,    -1,    -1,   104,   105,   106,    12,    13,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   116,   117,    -1,
      -1,   120,   121,    -1,    -1,    -1,    -1,   582,    -1,    -1,
      -1,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      -1,    -1,    46,    47,    -1,    49,    50,    51,    52,    53,
      54,    -1,    -1,    57,    -1,    59,    -1,    61,    62,    -1,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    73,
      74,    -1,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    85,    -1,    87,    88,    89,    90,    91,    92,    93,
      -1,    -1,    96,    97,    98,    99,   100,    12,    13,    -1,
     104,   105,   106,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   116,   117,    -1,    -1,   120,   121,    -1,    -1,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    -1,
      -1,    46,    47,    -1,    49,    50,    51,    52,    53,    54,
      -1,    -1,    57,    -1,    59,    -1,    61,    62,    -1,    64,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    73,    74,
      -1,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    -1,    87,    88,    89,    90,    91,    92,    93,    -1,
      -1,    96,    97,    98,    99,   100,    12,    13,    -1,   104,
     105,   106,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   116,   117,    -1,    -1,   120,   121,    -1,    -1,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    -1,    -1,
      46,    47,    -1,    49,    50,    51,    52,    53,    54,    -1,
      -1,    57,    -1,    59,    -1,    61,    62,    -1,    64,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    73,    74,    -1,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    85,
      -1,    87,    88,    89,    90,    91,    92,    93,    -1,    -1,
      96,    97,    98,    99,   100,    12,    13,    -1,   104,   105,
     106,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     116,   117,    -1,    -1,   120,   121,    -1,    -1,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    -1,    -1,    46,
      47,    -1,    49,    50,    51,    52,    53,    54,    -1,    -1,
      57,    -1,    59,    -1,    61,    62,    -1,    64,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    73,    74,    -1,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    -1,
      87,    88,    89,    90,    91,    92,    93,    -1,    -1,    96,
      97,    98,    99,   100,    12,    13,    -1,   104,   105,   106,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   116,
     117,    -1,    -1,   120,   121,    -1,    -1,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    -1,    -1,    46,    47,
      -1,    49,    50,    51,    52,    53,    54,    -1,    -1,    57,
      -1,    59,    -1,    61,    62,    -1,    64,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    73,    74,    -1,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    -1,    87,
      88,    89,    90,    91,    92,    93,    -1,    -1,    96,    97,
      98,    99,   100,    12,    13,    -1,   104,   105,   106,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   116,   117,
      -1,    -1,   120,   121,    -1,    -1,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    -1,    -1,    46,    47,    -1,
      49,    50,    51,    52,    53,    54,    -1,    -1,    57,    -1,
      59,    -1,    61,    62,    -1,    64,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    73,    74,    -1,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    85,    -1,    87,    88,
      89,    90,    91,    92,    93,    -1,    -1,    96,    97,    98,
      99,   100,    12,    13,    -1,   104,   105,   106,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   116,   117,    -1,
      -1,   120,   121,    -1,    -1,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    -1,    -1,    46,    47,    -1,    49,
      50,    51,    52,    53,    54,    -1,    -1,    57,    -1,    59,
      -1,    61,    62,    -1,    64,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    73,    74,    -1,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    85,    -1,    87,    88,    89,
      90,    91,    92,    93,    -1,    -1,    96,    97,    98,    99,
     100,    12,    13,    -1,   104,    -1,   106,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   116,   117,    -1,    -1,
     120,   121,    -1,    -1,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    -1,    -1,    46,    47,    -1,    49,    50,
      51,    -1,    53,    54,    -1,    -1,    -1,    -1,    59,    -1,
      61,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    74,    -1,    -1,    -1,    -1,    -1,    -1,
      81,    82,    83,    84,    85,    -1,    -1,    88,    -1,    90,
      91,    92,    93,    -1,    -1,    96,    97,    98,    -1,   100,
      12,    13,    -1,   104,    -1,   106,    35,    36,    37,    38,
      39,    40,    41,    42,    -1,   116,   117,    -1,    -1,   120,
     121,    -1,    -1,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    -1,    -1,    -1,    -1,    -1,    -1,    50,    51,
      52,    33,    -1,    35,    36,    37,    38,    39,    40,    41,
      42,    -1,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    73,    74,    -1,    76,    77,    78,    79,    80,    81,
      99,    83,    84,    -1,    -1,    87,    -1,    89,    -1,    91,
      92,    93,    -1,    -1,    96,    97,    98,    99,   100,   101,
      12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    93,    -1,    -1,   116,   117,    -1,    99,   120,   121,
      -1,    -1,    -1,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    -1,    -1,    -1,    -1,    -1,    -1,    50,    51,
      52,    -1,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    -1,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    73,    74,    -1,    76,    77,    78,    79,    80,    81,
      -1,    83,    84,    -1,    -1,    87,    -1,    89,    -1,    91,
      92,    93,    -1,    -1,    96,    97,    98,    99,   100,    12,
      13,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      93,    -1,    -1,    -1,   116,   117,    99,    -1,   120,   121,
      -1,    -1,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    -1,    -1,    -1,    -1,    -1,    -1,    50,    51,    -1,
      -1,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    93,
      73,    74,    -1,    -1,    -1,    99,    -1,    -1,    81,    -1,
      -1,    84,    -1,    -1,    -1,    -1,    -1,    -1,    91,    92,
      93,    -1,    -1,    96,    97,    98,    99,   100,   101,    12,
      13,   104,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    93,
      -1,    -1,    -1,   116,   117,    99,    -1,   120,   121,    -1,
      -1,    -1,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    -1,    -1,    -1,    -1,    -1,    -1,    50,    51,    -1,
      -1,    35,    36,    37,    38,    39,    40,    41,    42,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      73,    74,    -1,    -1,    -1,    -1,    -1,    -1,    81,    -1,
      -1,    84,    -1,    -1,    -1,    -1,    -1,    -1,    91,    92,
      93,    -1,    -1,    96,    97,    98,    99,   100,    12,    13,
      -1,   104,   105,    -1,    -1,    -1,    -1,    -1,    -1,    93,
      -1,    -1,    -1,   116,   117,    99,    -1,   120,   121,    -1,
      -1,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      -1,    -1,    -1,    -1,    -1,    -1,    50,    51,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,
      74,    -1,    -1,    -1,    -1,    -1,    -1,    81,    -1,    -1,
      84,    -1,    -1,    -1,    -1,    -1,    -1,    91,    92,    93,
      -1,    -1,    96,    97,    98,    99,   100,    12,    13,    -1,
     104,   105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   116,   117,    -1,    -1,   120,   121,    -1,    -1,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    -1,
      -1,    -1,    -1,    -1,    -1,    50,    51,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    74,
      -1,    -1,    -1,    -1,    -1,    -1,    81,    -1,    -1,    84,
      -1,    -1,    -1,    -1,    -1,    -1,    91,    92,    93,    -1,
      -1,    96,    97,    98,    99,   100,    12,    13,    -1,   104,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   116,   117,    -1,    -1,   120,   121,    -1,    -1,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    -1,    -1,
      -1,    -1,    -1,    -1,    50,    51,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    73,    74,    -1,
      -1,    -1,    -1,    -1,    -1,    81,    -1,    -1,    84,    -1,
      -1,    -1,    -1,    -1,    -1,    91,    92,    93,    -1,    -1,
      96,    97,    98,    -1,   100,    12,    13,    -1,   104,   105,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     116,   117,    -1,    -1,   120,   121,    -1,    -1,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    -1,    -1,    -1,
      -1,    -1,    -1,    50,    51,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    74,    -1,    -1,
      -1,    -1,    -1,    -1,    81,    -1,    -1,    84,    -1,    -1,
      -1,    -1,    -1,    -1,    91,    92,    93,    -1,    -1,    96,
      97,    98,    -1,   100,    12,    13,    -1,   104,   105,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   116,
     117,    -1,    -1,   120,   121,    -1,    -1,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    -1,    -1,    -1,    -1,
      -1,    -1,    50,    51,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    73,    74,    -1,    -1,    -1,
      -1,    -1,    -1,    81,    -1,    -1,    84,    -1,    -1,    -1,
      -1,    -1,    -1,    91,    92,    93,    -1,    -1,    96,    97,
      98,    99,   100,    12,    13,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   116,   117,
      -1,    -1,   120,   121,    -1,    -1,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    -1,    -1,    -1,    -1,    -1,
      -1,    50,    51,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    74,    -1,    -1,    -1,    -1,
      -1,    -1,    81,    -1,    -1,    84,    -1,    -1,    -1,    -1,
      -1,    -1,    91,    92,    93,    -1,    -1,    96,    97,    98,
      -1,   100,   101,    12,    13,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   116,   117,    -1,
      -1,   120,   121,    -1,    -1,    -1,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    -1,    -1,    -1,    -1,    -1,
      -1,    50,    51,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    73,    74,    -1,    -1,    -1,    -1,
      -1,    -1,    81,    -1,    -1,    84,    -1,    -1,    -1,    -1,
      -1,    -1,    91,    92,    93,    -1,    -1,    96,    97,    98,
      -1,   100,    12,    13,    -1,   104,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   116,   117,    -1,
      -1,   120,   121,    -1,    -1,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    -1,    -1,    -1,    -1,    -1,    -1,
      50,    51,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    73,    74,    -1,    -1,    -1,    -1,    -1,
      -1,    81,    -1,    -1,    84,    -1,    -1,    -1,    -1,    -1,
      -1,    91,    92,    93,    -1,    -1,    96,    97,    98,    -1,
     100,    12,    13,    -1,   104,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   116,   117,    -1,    -1,
     120,   121,    -1,    -1,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    -1,    -1,    -1,    -1,    -1,    -1,    50,
      51,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    73,    74,    -1,    -1,    -1,    -1,    -1,    -1,
      81,    -1,    -1,    84,    -1,    -1,    -1,    -1,    -1,    -1,
      91,    92,    93,    -1,    -1,    96,    97,    98,    -1,   100,
      12,    13,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   116,   117,    -1,    -1,   120,
     121,    -1,    -1,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    -1,    -1,    -1,    -1,    -1,    -1,    50,    51,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     0,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    73,    74,    -1,    -1,    -1,    -1,    -1,    -1,    81,
      -1,    -1,    84,    -1,    -1,    -1,    -1,    -1,    -1,    91,
      92,    93,    -1,    -1,    96,    97,    98,    -1,   100,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    52,    -1,    -1,   116,   117,    57,    -1,   120,   121,
      -1,    62,    -1,    64,    -1,    -1,    -1,    68,    -1,    70,
      71,    72,    -1,    -1,    75,    76,    77,    78,    79,    80,
      -1,    -1,    83,    -1,    -1,    -1,    87,    33,    89,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    99,    -1,
      -1,    -1,    -1,    -1,    -1,   106,    52,    -1,    -1,    -1,
      -1,    57,    -1,    -1,    -1,    -1,    62,    -1,    64,    -1,
      -1,    -1,    -1,    -1,    70,    71,    72,    -1,    -1,    -1,
      76,    77,    78,    79,    80,    -1,    -1,    83,    -1,    -1,
      -1,    87,    -1,    89,    -1,    -1,    -1,    93,    -1,    -1,
      -1,    -1,    -1,    99,    -1,    -1,    -1,    -1,   104,   105,
     106,    33,    -1,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      52,    -1,    -1,    -1,    -1,    57,    -1,    -1,    -1,    -1,
      62,    -1,    64,    -1,    -1,    -1,    -1,    -1,    70,    71,
      72,    -1,    -1,    -1,    76,    77,    78,    79,    80,    -1,
      -1,    83,    -1,    -1,    -1,    87,    -1,    89,    -1,    -1,
      -1,    93,    -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,
      -1,    -1,   104,   105,   106,    33,    -1,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    52,    -1,    -1,    -1,    -1,    57,
      -1,    -1,    -1,    -1,    62,    -1,    64,    -1,    -1,    -1,
      -1,    -1,    70,    71,    72,    -1,    -1,    -1,    76,    77,
      78,    79,    80,    -1,    -1,    83,    -1,    -1,    -1,    87,
      -1,    89,    -1,    -1,    -1,    93,    -1,    -1,    -1,    -1,
      -1,    99,    -1,    -1,    -1,    -1,   104,   105,   106,    33,
      -1,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    52,    -1,
      -1,    -1,    -1,    57,    -1,    -1,    -1,    -1,    62,    -1,
      64,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,    -1,
      -1,    -1,    76,    77,    78,    79,    80,    -1,    -1,    83,
      -1,    -1,    -1,    87,    -1,    89,    -1,    -1,    -1,    93,
      -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,    -1,    -1,
     104,   105,   106,    33,    -1,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    52,    -1,    -1,    -1,    -1,    57,    -1,    -1,
      -1,    -1,    62,    -1,    64,    -1,    -1,    -1,    -1,    -1,
      70,    71,    72,    -1,    -1,    -1,    76,    77,    78,    79,
      80,    -1,    -1,    83,    -1,    -1,    -1,    87,    -1,    89,
      -1,    -1,    -1,    93,    -1,    -1,    -1,    -1,    -1,    99,
      -1,    -1,    -1,    -1,   104,   105,   106,    33,    -1,    35,
      36,    37,    38,    39,    40,    41,    42,    43,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    52,    -1,    -1,    -1,
      -1,    57,    -1,    -1,    -1,    -1,    62,    -1,    64,    -1,
      -1,    -1,    -1,    -1,    70,    71,    72,    -1,    -1,    -1,
      76,    77,    78,    79,    80,    -1,    -1,    83,    -1,    -1,
      -1,    87,    -1,    89,    -1,    -1,    -1,    93,    -1,    -1,
      -1,    -1,    -1,    99,    -1,    -1,    -1,    -1,    -1,   105,
     106,    33,    -1,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      52,    -1,    -1,    -1,    -1,    57,    -1,    -1,    -1,    -1,
      62,    -1,    64,    -1,    -1,    -1,    -1,    -1,    70,    71,
      72,    -1,    -1,    -1,    76,    77,    78,    79,    80,    -1,
      -1,    83,    -1,    -1,    -1,    87,    -1,    89,    -1,    -1,
      -1,    93,    -1,    -1,    -1,    -1,    -1,    99,    -1,    -1,
      -1,    -1,    -1,   105,   106,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    52,    -1,    -1,    -1,    -1,    57,    -1,    -1,
      -1,    -1,    62,    -1,    64,    -1,    -1,    -1,    -1,    -1,
      70,    71,    72,    -1,    -1,    -1,    76,    77,    78,    79,
      80,    -1,    -1,    83,    -1,    -1,    -1,    87,    -1,    89,
      -1,    -1,    -1,    93,    -1,    -1,    -1,    -1,    -1,    99,
      -1,    -1,    -1,    -1,    -1,   105,   106,    35,    36,    37,
      38,    39,    40,    41,    42,    43,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    52,    -1,    -1,    -1,    -1,    57,
      -1,    -1,    -1,    -1,    62,    -1,    64,    -1,    -1,    -1,
      -1,    -1,    70,    71,    72,    -1,    -1,    -1,    76,    77,
      78,    79,    80,    -1,    -1,    83,    -1,    -1,    -1,    87,
      -1,    89,    -1,    -1,    -1,    93,    -1,    -1,    -1,    -1,
      -1,    99,    -1,    -1,    -1,    -1,    -1,   105,   106,    33,
      -1,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    52,    -1,
      -1,    -1,    -1,    57,    -1,    -1,    -1,    -1,    62,    -1,
      64,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,    -1,
      -1,    -1,    76,    77,    78,    79,    80,    -1,    -1,    83,
      -1,    -1,    -1,    87,    -1,    89,    -1,    -1,    -1,    93,
      -1,    -1,    -1,    -1,    33,    99,    35,    36,    37,    38,
      39,    40,    41,    42,    43,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    52,    -1,    -1,    -1,    -1,    57,    -1,
      -1,    -1,    -1,    62,    -1,    64,    -1,    -1,    -1,    -1,
      -1,    70,    71,    72,    -1,    -1,    -1,    76,    77,    78,
      79,    80,    -1,    -1,    83,    -1,    -1,    -1,    87,    -1,
      89,    -1,    -1,    -1,    93,    -1,    -1,    -1,    -1,    -1,
      99,    35,    36,    37,    38,    39,    40,    41,    42,    43,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    52,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,
      -1,    -1,    76,    77,    78,    79,    80,    -1,    -1,    83,
      -1,    -1,    -1,    87,    -1,    89,    -1,    -1,    -1,    93,
      -1,    -1,    -1,    -1,    -1,    99,    -1,   101,    35,    36,
      37,    38,    39,    40,    41,    42,    43,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    52,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    64,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,    -1,    76,
      77,    78,    79,    80,    -1,    -1,    83,    -1,    -1,    -1,
      87,    -1,    89,    -1,    -1,    -1,    93,    -1,    -1,    -1,
      -1,    -1,    99,    -1,   101,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    52,    -1,    -1,    -1,    -1,    57,    -1,    -1,
      -1,    -1,    62,    -1,    64,    -1,    -1,    -1,    -1,    -1,
      70,    71,    72,    -1,    -1,    -1,    76,    77,    78,    79,
      80,    -1,    -1,    83,    -1,    -1,    -1,    87,    -1,    89,
      -1,    -1,    -1,    93,    -1,    -1,    -1,    -1,    -1,    99,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    52,    -1,    -1,
      -1,    -1,    57,    -1,    -1,    -1,    -1,    62,    -1,    64,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    -1,    -1,
      -1,    76,    77,    78,    79,    80,    -1,    -1,    83,    -1,
      -1,    -1,    87,    -1,    89,    -1,    -1,    -1,    93,    -1,
      -1,    -1,    -1,    -1,    99,    35,    36,    37,    38,    39,
      40,    41,    42,    43,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    52,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    64,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    -1,    -1,    -1,    76,    77,    78,    79,
      80,    -1,    -1,    83,    -1,    52,    -1,    87,    -1,    89,
      57,    -1,    -1,    93,    -1,    62,    -1,    64,    -1,    99,
      -1,    68,    -1,    70,    71,    72,    -1,    -1,    75,    76,
      77,    78,    79,    80,    -1,    -1,    83,    -1,    52,    -1,
      87,    -1,    89,    57,    -1,    -1,    -1,    -1,    62,    -1,
      64,    -1,    99,    -1,    68,    -1,    70,    71,    72,   106,
      -1,    -1,    76,    77,    78,    79,    80,    -1,    -1,    83,
      -1,    52,    -1,    87,    -1,    89,    57,    -1,    -1,    -1,
      -1,    62,    -1,    64,    -1,    99,    -1,    -1,    -1,    70,
      71,    72,   106,    -1,    -1,    76,    77,    78,    79,    80,
      -1,    -1,    83,    -1,    52,    -1,    87,    -1,    89,    57,
      -1,    -1,    -1,    -1,    62,    -1,    64,    -1,    99,    -1,
      -1,    -1,    70,    71,    72,   106,    -1,    -1,    76,    77,
      78,    79,    80,    52,    -1,    83,    -1,    -1,    -1,    87,
      -1,    89,    -1,    -1,    -1,    64,    -1,    -1,    -1,    -1,
      -1,    99,    -1,    72,    -1,    -1,    -1,    76,    77,    78,
      79,    80,    -1,    -1,    83,    -1,    -1,    -1,    87,    -1,
      89,    -1,    -1,    -1,    93,    -1,    -1,    -1,    -1,    -1,
      99
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint16 yystos[] =
{
       0,    52,    57,    62,    64,    68,    70,    71,    72,    75,
      76,    77,    78,    79,    80,    83,    87,    89,    99,   106,
     123,   124,   125,   126,   132,   133,   134,   136,   137,   138,
     139,   140,   153,    93,    93,    79,    93,   135,   272,    93,
      93,   272,   272,     0,   124,    75,   126,   133,   125,   134,
     136,   137,    57,    62,    70,    71,   139,    33,   198,   199,
      67,   143,   144,   135,   106,   107,   198,   104,   158,   106,
     107,   100,   272,   136,    93,    93,    93,    93,    30,    93,
     111,   125,   200,   201,   202,    63,   141,   142,   196,   197,
     210,   272,   104,   150,   106,    93,   108,    63,   154,    35,
      36,    37,    38,    39,    40,    41,    42,    43,   105,   106,
     125,   138,   140,   153,   159,   160,   161,   173,   185,   186,
     187,   188,   189,   190,   191,   192,   193,   196,    12,    13,
      36,    43,    50,    51,    73,    74,    81,    84,    91,    92,
      93,    96,    97,    98,   100,   101,   104,   116,   117,   120,
     121,   126,   127,   128,   129,   131,   188,   236,   237,   238,
     239,   240,   241,   242,   243,   244,   245,   246,   247,   248,
     249,   250,   252,   253,   254,   255,   256,   257,   258,   264,
     266,   272,   273,   274,   106,   198,   143,   198,   158,    63,
     203,   202,    30,   102,   111,   196,   143,    31,    33,   205,
     102,    93,   105,   125,   151,   152,   210,   104,   155,   187,
     185,   105,   160,    93,   164,   165,   166,    44,   125,   194,
     195,   194,   247,   247,    32,   107,   107,   125,   186,   196,
     205,   209,    14,   107,   103,    36,    93,   100,   186,   188,
     193,   196,   229,   233,   236,   247,   268,   269,   272,   105,
     129,   130,   247,   247,   247,   247,   101,   102,   101,    32,
     107,   112,    10,     8,    11,   114,     9,     6,     7,    28,
      29,    69,   111,   115,    27,    30,   116,   117,   108,   118,
     119,   247,    12,    13,   107,   109,   100,   251,   265,    14,
      32,   107,   109,   265,   141,   150,   154,   196,   201,   104,
     146,   125,    30,   111,   112,   125,   186,   192,   206,   207,
     107,   196,   146,   265,    93,   102,   105,   106,   105,   106,
     138,   140,   153,   156,   157,   173,   174,   176,   185,   199,
      93,   164,    44,   100,   109,   125,   167,   168,   262,   263,
     102,   106,   103,    44,   195,   107,    57,    57,   265,   196,
     109,   125,   259,   260,   262,    93,   205,    93,   129,    43,
     101,   140,   178,   179,   185,   186,   193,   101,   101,   101,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,   103,   234,    26,   205,   102,   105,    93,   128,   107,
      57,   229,    11,     9,   240,   241,   242,   243,   243,   244,
     244,   192,   244,   244,   245,    30,   111,   246,   246,   247,
     247,   247,    73,    93,   205,   255,   229,   101,   229,   232,
      73,    93,   205,   267,   107,    57,    81,    84,   205,   255,
     229,   143,   155,     9,   204,    79,    93,   104,   105,   106,
     138,   140,   147,   148,   149,   153,   172,   174,   182,   185,
     199,   211,    93,    63,    81,   208,   112,    30,   102,   111,
     197,   146,   146,   265,   105,   106,   152,   105,   147,   185,
     199,   105,   157,    93,   185,   100,   106,   101,   235,   236,
      44,   168,   263,    93,   165,   104,   169,   171,   229,    57,
     145,   146,   265,   229,   109,   260,   261,   262,   171,    93,
     185,   101,   102,   166,   229,   211,   229,   270,    44,   101,
     105,   129,    57,   113,   238,   239,   111,   245,   196,    93,
     110,   101,   102,   267,    57,    14,   107,    93,   110,   146,
     196,     9,   211,   100,   177,    46,    47,    49,    53,    54,
      59,    61,    82,    83,    85,    88,    90,    93,   105,   106,
     138,   140,   163,   185,   211,   212,   213,   214,   215,   224,
     229,   272,    93,   185,   199,   105,   148,   164,    93,   192,
     192,   208,   207,   146,   105,   147,   105,    93,   185,   177,
      93,   101,    60,   162,   110,   105,   169,   170,   145,   110,
     229,   166,   140,   179,   180,   185,   101,   237,   245,   265,
     229,    93,   205,    93,   205,   265,   196,   101,   178,   180,
      86,   104,   181,   183,   100,   100,   229,   230,   229,    93,
     271,   271,    83,   214,   100,   100,   229,   100,   211,   216,
     100,   113,   185,   106,   164,   105,   213,   106,   177,   164,
      93,   106,   177,   105,   177,    93,   104,   106,   175,   181,
     211,   177,   162,   129,   106,   102,   105,   110,   185,     5,
      93,   265,    93,   101,   101,   210,    81,    84,   105,   184,
     205,   212,   252,   272,   183,   229,   140,   163,   185,   231,
     232,   106,   106,   113,   106,   106,    90,   229,   229,   106,
     140,   185,   217,   218,    56,   219,   220,   221,   211,   229,
     164,   181,   183,   106,   177,   181,   183,   175,   181,   177,
     105,   175,   175,   181,   106,   105,   169,     5,   166,   265,
     265,   265,   105,   212,    81,    84,   105,   107,   107,   101,
     185,   106,    93,   106,   229,   100,   101,   101,   185,    93,
     101,   106,   100,    65,   221,   219,   220,   101,   183,   181,
     183,   183,   175,   175,   181,   106,   175,   166,   106,   106,
     105,   265,   265,    81,   205,    81,   205,   214,    93,   230,
     113,   230,   106,   229,   104,   225,   211,    93,   103,   101,
     218,   140,   196,   222,   223,   211,    65,   214,   183,   175,
     106,   106,   265,    81,   265,    81,    48,   113,   106,   230,
     106,   101,    55,    60,   226,   227,   103,   229,   196,   223,
      93,   101,    11,    93,   211,   106,   265,   106,   265,   214,
     230,   231,   101,   231,   106,   235,   113,   105,   227,   229,
      93,    93,   211,   196,   106,   106,   101,   101,   214,   101,
     113,   213,   228,   214,   214,   214,   228,   213
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint16 yyr1[] =
{
       0,   122,   123,   123,   124,   124,   124,   124,   124,   124,
     125,   125,   126,   126,   126,   126,   127,   127,   128,   129,
     129,   129,   130,   130,   131,   131,   131,   132,   132,   133,
     133,   134,   134,   135,   135,   136,   136,   137,   137,   137,
     138,   138,   138,   138,   139,   139,   139,   139,   139,   139,
     139,   139,   139,   139,   139,   139,   140,   140,   141,   141,
     142,   143,   143,   144,   145,   145,   146,   146,   147,   147,
     148,   148,   148,   148,   149,   149,   149,   149,   149,   150,
     150,   150,   150,   150,   150,   150,   151,   151,   152,   152,
     152,   152,   152,   152,   152,   152,   153,   153,   153,   153,
     154,   154,   155,   155,   156,   156,   157,   157,   157,   157,
     157,   158,   158,   159,   159,   160,   160,   160,   160,   160,
     161,   161,   162,   162,   163,   163,   164,   164,   165,   165,
     166,   166,   166,   167,   167,   168,   169,   169,   170,   170,
     171,   171,   171,   172,   172,   173,   173,   174,   174,   174,
     174,   174,   174,   174,   174,   175,   175,   175,   176,   177,
     177,   177,   178,   178,   178,   179,   179,   180,   180,   181,
     182,   182,   182,   182,   182,   182,   182,   182,   183,   183,
     183,   183,   184,   184,   184,   184,   184,   184,   184,   184,
     185,   185,   185,   186,   186,   187,   187,   188,   188,   189,
     189,   189,   189,   189,   190,   190,   191,   192,   192,   193,
     193,   194,   194,   195,   195,   196,   197,   197,   197,   197,
     198,   198,   199,   199,   199,   199,   200,   200,   201,   201,
     202,   202,   203,   203,   204,   204,   205,   205,   205,   205,
     206,   206,   207,   207,   207,   207,   207,   208,   208,   209,
     209,   210,   210,   211,   211,   212,   212,   213,   213,   213,
     214,   214,   214,   214,   214,   214,   214,   214,   214,   214,
     214,   214,   214,   214,   214,   214,   214,   214,   214,   214,
     214,   215,   215,   215,   215,   216,   216,   217,   217,   218,
     218,   219,   219,   220,   220,   221,   222,   222,   222,   222,
     223,   223,   224,   225,   226,   226,   227,   227,   227,   227,
     228,   228,   229,   229,   230,   230,   231,   231,   232,   232,
     233,   233,   234,   234,   234,   234,   234,   234,   234,   234,
     234,   234,   234,   234,   235,   236,   236,   237,   237,   238,
     238,   239,   239,   240,   240,   241,   241,   242,   242,   242,
     243,   243,   243,   243,   243,   243,   244,   244,   244,   244,
     245,   245,   245,   246,   246,   246,   246,   247,   247,   247,
     247,   247,   247,   247,   247,   248,   249,   249,   249,   249,
     249,   250,   250,   250,   250,   251,   252,   252,   252,   252,
     252,   252,   252,   252,   252,   252,   252,   253,   253,   253,
     253,   253,   253,   253,   254,   254,   254,   255,   255,   256,
     256,   256,   257,   257,   258,   258,   259,   259,   260,   260,
     261,   261,   262,   262,   263,   263,   264,   264,   264,   264,
     264,   264,   265,   265,   266,   266,   266,   266,   266,   266,
     267,   267,   268,   269,   269,   269,   270,   270,   271,   271,
     272,   272,   273,   273,   273,   273,   273,   273,   273,   273,
     274
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     1,     2,     1,     1,     1,     2,     2,     3,
       1,     2,     5,     5,     4,     2,     1,     3,     3,     1,
       1,     1,     1,     3,     4,     3,     2,     4,     3,     1,
       2,     4,     3,     3,     1,     1,     2,     1,     1,     1,
       6,     7,     4,     5,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     2,     0,     1,
       2,     0,     1,     2,     0,     1,     2,     3,     1,     2,
       1,     1,     2,     1,     1,     1,     1,     1,     1,     6,
       5,     5,     4,     4,     3,     2,     1,     3,     2,     3,
       3,     4,     2,     3,     1,     2,     5,     6,     3,     4,
       0,     2,     2,     3,     1,     2,     1,     1,     1,     1,
       1,     2,     3,     1,     2,     1,     1,     1,     1,     1,
       6,     7,     0,     2,     2,     3,     1,     3,     1,     3,
       1,     2,     2,     1,     2,     3,     1,     1,     1,     3,
       2,     3,     4,     4,     3,     3,     4,     4,     5,     5,
       6,     5,     6,     6,     7,     1,     3,     1,     1,     3,
       3,     2,     1,     3,     3,     2,     3,     3,     4,     2,
       4,     5,     5,     6,     3,     4,     4,     5,     2,     3,
       3,     4,     3,     4,     3,     4,     5,     6,     5,     6,
       1,     1,     1,     2,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     2,
       2,     1,     2,     2,     1,     1,     1,     2,     4,     4,
       0,     1,     2,     2,     3,     3,     1,     3,     2,     1,
       2,     1,     2,     3,     2,     3,     2,     2,     3,     3,
       1,     3,     1,     1,     2,     2,     3,     2,     2,     1,
       1,     1,     3,     2,     3,     1,     2,     1,     2,     1,
       1,     3,     5,     5,     7,     1,     5,     7,     9,     9,
       8,     9,     3,     3,     3,     1,     3,     5,     2,     2,
       1,     4,     6,     5,     3,     3,     4,     1,     3,     4,
       5,     0,     1,     1,     2,     5,     2,     3,     2,     3,
       1,     3,     5,     3,     1,     2,     3,     4,     2,     3,
       1,     2,     1,     1,     0,     1,     0,     1,     1,     3,
       3,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     5,     1,     4,     1,
       4,     1,     3,     1,     3,     1,     3,     1,     3,     3,
       1,     3,     3,     3,     3,     3,     1,     3,     4,     5,
       1,     3,     3,     1,     3,     3,     3,     1,     2,     2,
       2,     2,     2,     2,     1,     2,     3,     3,     3,     4,
       5,     1,     1,     2,     2,     3,     1,     1,     1,     1,
       3,     1,     1,     1,     1,     1,     1,     3,     4,     3,
       3,     4,     3,     4,     1,     3,     3,     4,     5,     3,
       3,     5,     4,     4,     4,     4,     1,     2,     3,     4,
       0,     1,     1,     2,     1,     2,     2,     5,     2,     5,
       6,     7,     2,     1,     3,     4,     5,     6,     3,     4,
       1,     1,     3,     1,     2,     3,     1,     1,     0,     1,
       1,     3,     1,     1,     1,     1,     1,     1,     1,     1,
       1
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                  \
do                                                              \
  if (yychar == YYEMPTY)                                        \
    {                                                           \
      yychar = (Token);                                         \
      yylval = (Value);                                         \
      YYPOPSTACK (yylen);                                       \
      yystate = *yyssp;                                         \
      goto yybackup;                                            \
    }                                                           \
  else                                                          \
    {                                                           \
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;                                                  \
    }                                                           \
while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256



/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)

/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*----------------------------------------.
| Print this symbol's value on YYOUTPUT.  |
`----------------------------------------*/

static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  FILE *yyo = yyoutput;
  YYUSE (yyo);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
  YYUSE (yytype);
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyoutput, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yytype_int16 *yyssp, YYSTYPE *yyvsp, int yyrule)
{
  unsigned long int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[yyssp[yyi + 1 - yynrhs]],
                       &(yyvsp[(yyi + 1) - (yynrhs)])
                                              );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
yystrlen (const char *yystr)
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            /* Fall through.  */
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYSIZE_T *yymsg_alloc, char **yymsg,
                yytype_int16 *yyssp, int yytoken)
{
  YYSIZE_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[*yyssp];
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYSIZE_T yysize1 = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (! (yysize <= yysize1
                         && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                    return 2;
                  yysize = yysize1;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    YYSIZE_T yysize1 = yysize + yystrlen (yyformat);
    if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
      return 2;
    yysize = yysize1;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          yyp++;
          yyformat++;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
{
  YYUSE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;


/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        YYSTYPE *yyvs1 = yyvs;
        yytype_int16 *yyss1 = yyss;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * sizeof (*yyssp),
                    &yyvs1, yysize * sizeof (*yyvsp),
                    &yystacksize);

        yyss = yyss1;
        yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yytype_int16 *yyss1 = yyss;
        union yyalloc *yyptr =
          (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
                  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
      
#line 2820 "y.tab.c" /* yacc.c:1661  */
      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = (char *) YYSTACK_ALLOC (yymsg_alloc);
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYTERROR;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  return yyresult;
}
#line 911 "java8.y" /* yacc.c:1906  */

  int
  yyerror(char const *str)
  {
      extern char *yytext;
      extern int yylineno;
      fprintf(stderr, "line:%d parse error near %s\n",yylineno, yytext);
      return 0;
  }

  static double timediff(struct timeval *s, struct timeval *e) {
    double t1 = (e->tv_sec - s->tv_sec) * 1000.0;
    double t2 = (e->tv_usec - s->tv_usec) / 1000.0;
    return t1 + t2; /* ms */
  }

  int main(int argc, char *const argv[])
  {
      extern int yyparse(void);
      extern FILE *yyin;
      // const char *input_file = NULL;
      int input_size = 0;
      const char *orig_argv0 = argv[0];
      // int opt;


      if (argc == 1 ) {
        fprintf(stdout,"Usage: %s [inputfiles]",argv[0]);
        return 1;
      }

      for (int i = 1; i < argc; i++) {
        if (!(yyin = fopen(argv[i], "rb"))) {
    			fprintf(stderr, "File [%s] is not found!\n", argv[i]);
    			return 1;
        }
        int result = 0;
        struct timeval start, end;
        gettimeofday(&start, NULL);
        result = yyparse();
        gettimeofday(&end, NULL);
        // printf("%d\n",result );
        if (result) {
          fprintf(stderr, "[%s] Parse Error!!!\n", argv[i]);
          break;
        }
        fprintf(stdout, "%s OK %.4f [ms]\n", argv[i], timediff(&start, &end));
      }

      // for (int i = 1; i < argc; i++) {
      //   if (!(yyin = fopen(argv[i], "rb"))) {
      //     fprintf(stderr, "File [%s] is not found!\n", argv[i]);
      //     return 1;
      //   }
      //   double tsum = 0.0;
      //   double t[5];
      //   for (int c = 0; c < 5; c++) {
      //     int result = 0;
      //     struct timeval start, end;
      //     gettimeofday(&start, NULL);
      //     result = yyparse();
      //     gettimeofday(&end, NULL);
      //     // printf("%d\n",result );
      //     if (result) {
      //       fprintf(stdout, "%s FAIL %.4f [ms]\n", argv[i], timediff(&start, &end));
      //       break;
      //     }
      //     t[c] = timediff(&start, &end);
      //     tsum += t[c];
      //   }
      //   if (tsum != 0.0) {
      //     fprintf(stdout, "%s OK %.4f [ms]\n", argv[i], tsum/5);
      //   }
      // }


      // uint64_t start, end;
      // start = timer();
      // if (yyparse()) {
      //     fprintf(stderr, "[%s] Parse Error!!!\n", argv[2]);
      //     exit(1);
      // } else {
      //     // fprintf(stderr, "Match!!\n");
      // }
      // end = timer();
      //
      // printf("[%s] %llu [ms]\n",argv[2],end - start);
      //
      // return 0;
  }
