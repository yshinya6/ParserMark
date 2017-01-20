#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<assert.h>

struct Tree;
struct TreeLog;
struct MemoEntry;

typedef unsigned long int symbol_t;

typedef struct ParserContext {
    const unsigned char  *inputs;
    size_t       length;
    const unsigned char  *pos;
    struct Tree *left;
    // AST
    struct TreeLog *logs;
    size_t          log_size;
    size_t          unused_log;
	// stack
    struct Wstack  *stacks;
    size_t          stack_size;
    size_t          unused_stack;
    size_t          fail_stack;
    // SymbolTable
    struct SymbolTableEntry* tables;
	size_t                   tableSize;
	size_t                   tableMax;
	size_t                   stateValue;
	size_t                   stateCount;
	unsigned long int        count;
    // Memo
    struct MemoEntry *memoArray;
    size_t memoSize;
    // APIs
    void *thunk;
	void* (*fnew)(symbol_t, const unsigned char *, size_t, size_t, void *);
	void  (*fsub)(void *, size_t, symbol_t, void *, void *);
	void  (*fgc)(void *, int, void*);
} ParserContext;

#ifdef CNEZ_NOGC
#define GCINC(c, v2)     
#define GCDEC(c, v1)     
#define GCSET(c, v1, v2) 
#else
#define GCINC(c, v2)     c->fgc(v2,  1, c->thunk)
#define GCDEC(c, v1)     c->fgc(v1, -1, c->thunk)
#define GCSET(c, v1, v2) c->fgc(v2, 1, c->thunk); c->fgc(v1, -1, c->thunk)
#endif

/* TreeLog */

#define STACKSIZE 64

#define OpLink 0
#define OpTag  1
#define OpReplace 2
#define OpNew 3

typedef struct TreeLog {
	int op;
	void *value;
	struct Tree *tree;
} TreeLog;

static const char* ops[5] = {"link", "tag", "value", "new"};

static size_t cnez_used = 0;

static void *_malloc(size_t t)
{
	size_t *d = (size_t*)malloc(sizeof(size_t) + t);
	cnez_used += t;
	d[0] = t;
	memset((void*)(d+1), 0, t);
	return (void*)(d+1);
}

static void *_calloc(size_t items, size_t size)
{
	return _malloc(items * size);
}

static void _free(void *p)
{
	size_t *d = (size_t*)p;
	cnez_used -= d[-1];
	free(d-1);
}

// stack

typedef struct Wstack {
	size_t value;
	struct Tree *tree;
} Wstack;

static Wstack *unusedStack(ParserContext *c)
{
	if (c->stack_size == c->unused_stack) {
		Wstack *newstack = (Wstack *)_calloc(c->stack_size * 2, sizeof(struct Wstack));
		memcpy(newstack, c->stacks, sizeof(struct Wstack) * c->stack_size);
		_free(c->stacks);
		c->stacks = newstack;
		c->stack_size *= 2;
	}
	Wstack *s = c->stacks + c->unused_stack;
	c->unused_stack++;
	return s;
}

static
void push(ParserContext *c, size_t value)
{
	Wstack *s = unusedStack(c);
	s->value = value;
	GCDEC(c, s->tree);
	s->tree  = NULL;
}

static
void pushW(ParserContext *c, size_t value, struct Tree *t)
{
	Wstack *s = unusedStack(c);
	s->value = value;
	GCSET(c, s->tree, t);
	s->tree  = t;
}

static
Wstack *popW(ParserContext *c)
{
	c->unused_stack--;
	return c->stacks + c->unused_stack;
}

/* memoization */

#define NotFound    0
#define SuccFound   1
#define FailFound   2

typedef long long int  uniquekey_t;

typedef struct MemoEntry {
    uniquekey_t key;
    long consumed;
	struct Tree *memoTree;
	int result;
	int stateValue;
} MemoEntry;


/* Tree */

typedef struct Tree {
    long           refc;
    symbol_t       tag;
    const unsigned char    *text;
    size_t         len;
    size_t         size;
    symbol_t      *labels;
    struct Tree  **childs;
} Tree;

static size_t t_used = 0;
static size_t t_newcount = 0;
static size_t t_gccount = 0;
 
static void *tree_malloc(size_t t)
{
	size_t *d = (size_t*)malloc(sizeof(size_t) + t);
	t_used += t;
	d[0] = t;
	return (void*)(d+1);
}

static void *tree_calloc(size_t items, size_t size)
{
	void *d = tree_malloc(items * size);
	memset(d, 0, items * size);
	return d;
}

static void tree_free(void *p)
{
	size_t *d = (size_t*)p;
	t_used -= d[-1];
	free(d-1);
}

static
void *NEW(symbol_t tag, const unsigned char *text, size_t len, size_t n, void *thunk)
{
    Tree *t = (Tree*)tree_malloc(sizeof(struct Tree));
	t->refc = 0;
    t->tag = tag;
    t->text = text;
    t->len = len;
    t->size = n;
    if(n > 0) {
        t->labels = (symbol_t*)tree_calloc(n, sizeof(symbol_t));
        t->childs = (struct Tree**)tree_calloc(n, sizeof(struct Tree*));
    }
    else {
        t->labels = NULL;
        t->childs = NULL;
    }
	t_newcount++;
    return t;
}

static
void GC(void *parent, int c, void *thunk)
{
    Tree *t = (Tree*)parent;
	if(t == NULL) {
		return;
	}
#ifdef CNEZ_NOGC
	if(t->size > 0) {
		size_t i = 0;
		for(i = 0; i < t->size; i++) {
			GC(t->childs[i], -1, thunk);
		}
		tree_free(t->labels);
		tree_free(t->childs);
	}
	tree_free(t);
#else
	if(c == 1) {
		t->refc ++;
		return;
	}
	t->refc --;
	if(t->refc == 0) {
		if(t->size > 0) {
			size_t i = 0;
			for(i = 0; i < t->size; i++) {
				GC(t->childs[i], -1, thunk);
			}
			tree_free(t->labels);
			tree_free(t->childs);
		}
		tree_free(t);
		t_gccount++;
	}
#endif
}

static
void LINK(void *parent, size_t n, symbol_t label, void *child, void *thunk)
{
    Tree *t = (Tree*)parent;
    t->labels[n] = label;
    t->childs[n] = (struct Tree*)child;
#ifndef CNEZ_NOGC
	GC(child, 1, thunk);
#endif
}

void cnez_free(void *t)
{
	GC(t, -1, NULL);
}

static size_t cnez_count(void *v, size_t c)
{
	size_t i;
	Tree *t = (Tree*)v;
	if(t == NULL) {
		return c+0;
	}
	c++;
	for(i = 0; i < t->size; i++) {
		c = cnez_count(t->childs[i], c);
	}
	return c;
}

static void cnez_dump_memory(const char *msg, void *t)
{
	size_t alive = cnez_count(t, 0);
	size_t used = (t_newcount - t_gccount);
	fprintf(stdout, "%s: tree=%ld[bytes], new=%ld, gc=%ld, alive=%ld %s\n", msg, t_used, t_newcount, t_gccount, alive, alive == used ? "OK" : "LEAK");
}

static void ParserContext_initTreeFunc(ParserContext *c, 
	void *thunk,
	void* (*fnew)(symbol_t, const unsigned char *, size_t, size_t, void *), 
	void  (*fset)(void *, size_t, symbol_t, void *, void *), 
	void  (*fgc)(void *, int, void*))
{
	if(fnew != NULL && fset != NULL && fgc != NULL) {
		c->fnew = fnew;
		c->fsub = fset;
		c->fgc  = fgc;
	}
	else {
		c->thunk = c;
    	c->fnew = NEW;
    	c->fsub = LINK;
    	c->fgc  = GC;
    }
    c->thunk = thunk == NULL ? c : thunk;
}

static
void *nonew(symbol_t tag, const unsigned char *pos, size_t len, size_t n, void *thunk)
{
    return NULL;
}

static
void nosub(void *parent, size_t n, symbol_t label, void *child, void *thunk)
{
}

static
void nogc(void *parent, int c, void *thunk)
{
}

static void ParserContext_initNoTreeFunc(ParserContext *c)
{
    c->fnew = nonew;
    c->fsub = nosub;
    c->fgc  = nogc;
    c->thunk = c;
}

/* ParserContext */

static ParserContext *ParserContext_new(const unsigned char *text, size_t len)
{
    ParserContext *c = (ParserContext*) _malloc(sizeof(ParserContext));
    c->inputs = text;
    c->length = len;
    c->pos = text;
    c->left = NULL;
    // tree
    c->log_size = 64;
    c->logs = (struct TreeLog*) _calloc(c->log_size, sizeof(struct TreeLog));
    c->unused_log = 0;
    // stack
    c->stack_size = 64;
    c->stacks = (struct Wstack*) _calloc(c->stack_size, sizeof(struct Wstack));
    c->unused_stack = 0;
    c->fail_stack   = 0;
    // symbol table
    c->tables = NULL;
    c->tableSize = 0;
    c->tableMax  = 0;
    c->stateValue = 0;
    c->stateCount = 0;
    c->count = 0;
    // memo
    c->memoArray = NULL;
    c->memoSize = 0;
    return c;
}

static int ParserContext_eof(ParserContext *c)
{
    return !(c->pos < (c->inputs + c->length));
}

static const unsigned char ParserContext_read(ParserContext *c)
{
    return *(c->pos++);
}

static const unsigned char ParserContext_prefetch(ParserContext *c)
{
    return *(c->pos);
}

static void ParserContext_move(ParserContext *c, int shift)
{
    c->pos += shift;
}

static void ParserContext_back(ParserContext *c, const unsigned char *ppos)
{
    c->pos = ppos;
}

static int ParserContext_match(ParserContext *c, const unsigned char *text, size_t len) {
	if (c->pos + len > c->inputs + c->length) {
		return 0;
	}
	size_t i;
	for (i = 0; i < len; i++) {
		if (text[i] != c->pos[i]) {
			return 0;
		}
	}
	c->pos += len;
	return 1;
}

static int ParserContext_eof2(ParserContext *c, size_t n) {
	if (c->pos + n <= c->inputs + c->length) {
		return 1;
	}
	return 0;
}

static int ParserContext_match2(ParserContext *c, const unsigned char c1, const unsigned char c2) {
	if (c->pos[0] == c1 && c->pos[1] == c2) {
		c->pos+=2;
		return 1;
	}
	return 0;
}

static int ParserContext_match3(ParserContext *c, const unsigned char c1, const unsigned char c2, const unsigned char c3) {
	if (c->pos[0] == c1 && c->pos[1] == c2 && c->pos[2] == c3) {
		c->pos+=3;
		return 1;
	}
	return 0;
}

static int ParserContext_match4(ParserContext *c, const unsigned char c1, const unsigned char c2, const unsigned char c3, const unsigned char c4) {
	if (c->pos[0] == c1 && c->pos[1] == c2 && c->pos[2] == c3 && c->pos[3] == c4) {
		c->pos+=4;
		return 1;
	}
	return 0;
}

static int ParserContext_match5(ParserContext *c, const unsigned char c1, const unsigned char c2, const unsigned char c3, const unsigned char c4, const unsigned char c5) {
	if (c->pos[0] == c1 && c->pos[1] == c2 && c->pos[2] == c3 && c->pos[3] == c4 && c->pos[4] == c5 ) {
		c->pos+=5;
		return 1;
	}
	return 0;
}

static int ParserContext_match6(ParserContext *c, const unsigned char c1, const unsigned char c2, const unsigned char c3, const unsigned char c4, const unsigned char c5, const unsigned char c6) {
	if (c->pos[0] == c1 && c->pos[1] == c2 && c->pos[2] == c3 && c->pos[3] == c4 && c->pos[4] == c5 && c->pos[5] == c6 ) {
		c->pos+=6;
		return 1;
	}
	return 0;
}

static int ParserContext_match7(ParserContext *c, const unsigned char c1, const unsigned char c2, const unsigned char c3, const unsigned char c4, const unsigned char c5, const unsigned char c6, const unsigned char c7) {
	if (c->pos[0] == c1 && c->pos[1] == c2 && c->pos[2] == c3 && c->pos[3] == c4 && c->pos[4] == c5 && c->pos[5] == c6 && c->pos[6] == c7) {
		c->pos+=7;
		return 1;
	}
	return 0;
}

static int ParserContext_match8(ParserContext *c, const unsigned char c1, const unsigned char c2, const unsigned char c3, const unsigned char c4, const unsigned char c5, const unsigned char c6, const unsigned char c7, const unsigned char c8) {
	if (c->pos[0] == c1 && c->pos[1] == c2 && c->pos[2] == c3 && c->pos[3] == c4 && c->pos[4] == c5 && c->pos[5] == c6 && c->pos[6] == c7 && c->pos[7] == c8) {
		c->pos+=8;
		return 1;
	}
	return 0;
}

#ifdef CNEZ_SSE
#ifdef _MSC_VER
#include <intrin.h>
#else
#include <x86intrin.h>
#endif
#endif

static const unsigned char *_findchar(
	const unsigned char *p, long size,
	const unsigned char *range, long range_size) {
#ifdef CNEZ_SSE
	const __m128i r = _mm_loadu_si128((const __m128i*)range);
	__m128i v;
	while (size > 0) {
		v = _mm_loadu_si128((const __m128i*)p);
		if (!_mm_cmpestra(r, range_size, v, size, 4)) {
			if (_mm_cmpestrc(r, range_size, v, size, 4)) {
				return p + _mm_cmpestri(r, range_size, v, size, 4);
			}
			break;
		}
		p += 16;
		size -= 16;
	}
#else
	size_t i,j;
	for (i = 0; i < size; i++) {
		const unsigned char c = p[i];
		for (j = 0; j < range_size ; j+=2) {
			if (range[j] <= c && c <= range[j+1]) {
				return p + i;
			}
		}
	}
#endif
	return p + size;
}

static void ParserContext_skipRange(ParserContext *c, const unsigned char *range, size_t range_size)
{
	size_t size = c->length - (c->pos - c->inputs);
	c->pos = _findchar(c->pos, size, range, range_size);	
}

static int ParserContext_checkOneMoreRange(ParserContext *c, const unsigned char *range, size_t range_size)
{
	size_t size = c->length - (c->pos - c->inputs);
	const unsigned char *p = _findchar(c->pos, size, range, range_size);
	if(p > c->pos) {
		c->pos = p;
		return 1;
	}
	return 0;
}

// AST

static
void _log(ParserContext *c, int op, void *value, struct Tree *tree)
{
	if(!(c->unused_log < c->log_size)) {
		TreeLog *newlogs = (TreeLog *)_calloc(c->log_size * 2, sizeof(TreeLog));
		memcpy(newlogs, c->logs, c->log_size * sizeof(TreeLog));
		_free(c->logs);
		c->logs = newlogs;
		c->log_size *= 2;
	}
	TreeLog *l = c->logs + c->unused_log;
	l->op = op;
	l->value = value;
	assert(l->tree == NULL);
	if(op == OpLink) {
		GCSET(c, l->tree, tree);
	}
	l->tree  = tree;
	//printf("LOG[%d] %s %p %p\n", (int)c->unused_log, ops[op], value, tree);
	c->unused_log++;
}

void cnez_dump(void *v, FILE *fp);

static
void DEBUG_dumplog(ParserContext *c) 
{
	long i;
	for(i = c->unused_log-1; i >= 0; i--) {
		TreeLog *l = c->logs + i;
		printf("[%d] %s %p ", (int)i, ops[l->op], l->value);
		if(l->tree != NULL) {
			cnez_dump(l->tree, stdout);
		}
		printf("\n");
	}
}

static void ParserContext_beginTree(ParserContext *c, int shift)
{
    _log(c, OpNew, (void *)(c->pos + shift), NULL);
}

static void ParserContext_linkTree(ParserContext *c, symbol_t label)
{
    _log(c, OpLink, (void*)label, c->left);
}

static void ParserContext_tagTree(ParserContext *c, symbol_t tag)
{
    _log(c, OpTag, (void*)((long)tag), NULL);
}

static void ParserContext_valueTree(ParserContext *c, const unsigned char *text, size_t len)
{
    _log(c, OpReplace, (void*)text, (Tree*)len);
}

static void ParserContext_foldTree(ParserContext *c, int shift, symbol_t label)
{
    _log(c, OpNew, (void*)(c->pos + shift), NULL);
    _log(c, OpLink, (void*)label, c->left);
}

static size_t ParserContext_saveLog(ParserContext *c)
{
    return c->unused_log;
}

static void ParserContext_backLog(ParserContext *c, size_t unused_log)
{
    if (unused_log < c->unused_log) {
  		size_t i;
		for(i = unused_log; i < c->unused_log; i++) {
			TreeLog *l = c->logs + i;
			if(l->op == OpLink) {
				GCDEC(c, l->tree);
			}
			l->op = 0;
			l->value = NULL;
			l->tree = NULL;
		}
		c->unused_log = unused_log;
    }
}

static void ParserContext_endTree(ParserContext *c, int shift, symbol_t tag, const unsigned char *text, size_t len)
{
    int objectSize = 0;
    long i;
    for(i = c->unused_log - 1; i >= 0; i--) {
 	   TreeLog * l = c->logs + i;
 	   if(l->op == OpLink) {
	     objectSize++;
	     continue;
	   }
 	   if(l->op == OpNew) {
 	     break;
 	   }
	   if(l->op == OpTag && tag == 0) {
	     tag = (symbol_t)l->value;
	   }
	   if(l->op == OpReplace) {
	   	 if(text == NULL) {
		     text = (const unsigned char*)l->value;
		     len = (size_t)l->tree;
		 }
	     l->tree = NULL;
	   }
	}
 	TreeLog * start = c->logs + i;
 	if(text == NULL) {
    	text = (const unsigned char*)start->value;
    	len = ((c->pos + shift) - text);
    }
    Tree *t = c->fnew(tag, text, len, objectSize, c->thunk);
	GCSET(c, c->left, t);
    c->left = t;
    if (objectSize > 0) {
        int n = 0;
        size_t j;
        for(j = i; j < c->unused_log; j++) {
 		   TreeLog * cur = c->logs + j;
           if (cur->op == OpLink) {
              c->fsub(c->left, n++, (symbol_t)cur->value, cur->tree, c->thunk);
           }
        }
    }
    ParserContext_backLog(c, i);
}

static size_t ParserContext_saveTree(ParserContext *c)
{
	size_t back = c->unused_stack;
	pushW(c, 0, c->left);
	return back;
}

static void ParserContext_backTree(ParserContext *c, size_t back)
{
	Tree* t = c->stacks[back].tree;
	if(c->left != t) {
		GCSET(c, c->left, t);
		c->left = t;
	}
	c->unused_stack = back;
}

// Symbol Table ---------------------------------------------------------

static const unsigned char NullSymbol[4] = { 0, 0, 0, 0 };

typedef struct SymbolTableEntry {
	int stateValue;
	symbol_t table;
	const unsigned char* symbol;
	size_t      length;
} SymbolTableEntry;


static
void _push(ParserContext *c, symbol_t table, const unsigned char * utf8, size_t length) 
{
	if (!(c->tableSize < c->tableMax)) {
		SymbolTableEntry* newtable = (SymbolTableEntry*)_calloc(sizeof(SymbolTableEntry), (c->tableMax + 256));
		if(c->tables != NULL) {
			memcpy(newtable, c->tables, sizeof(SymbolTableEntry) * (c->tableMax));
			_free(c->tables);
		}
		c->tables = newtable;
		c->tableMax += 256;
	}
	SymbolTableEntry *entry = c->tables + c->tableSize;
	c->tableSize++;
	if (entry->table == table && entry->length == length && memcmp(utf8, entry->symbol, length) == 0) {
		// reuse state value
		c->stateValue = entry->stateValue;
	} else {
		entry->table = table;
		entry->symbol = utf8;
		entry->length = length;
		c->stateValue = c->stateCount++;
		entry->stateValue = c->stateValue;
	}
}

static int ParserContext_saveSymbolPoint(ParserContext *c) 
{
	return c->tableSize;
}

static 
void ParserContext_backSymbolPoint(ParserContext *c, int savePoint) 
{
	if (c->tableSize != savePoint) {
		c->tableSize = savePoint;
		if (c->tableSize == 0) {
			c->stateValue = 0;
		} else {
			c->stateValue = c->tables[savePoint - 1].stateValue;
		}
	}
}

static
void ParserContext_addSymbol(ParserContext *c, symbol_t table, const unsigned char *ppos) {
	size_t length = c->pos - ppos;
	_push(c, table, ppos, length);
}

static
void ParserContext_addSymbolMask(ParserContext *c, symbol_t table) {
	_push(c, table, NullSymbol, 4);
}

static int ParserContext_exists(ParserContext *c, symbol_t table) 
{
	long i;
	for (i = c->tableSize - 1; i >= 0; i--) {
		SymbolTableEntry * entry = c->tables + i;
		if (entry->table == table) {
			return entry->symbol != NullSymbol;
		}
	}
	return 0;
}

static int ParserContext_existsSymbol(ParserContext *c, symbol_t table, const unsigned char *symbol, size_t length) 
{
	long i;
	for (i = c->tableSize - 1; i >= 0; i--) {
		SymbolTableEntry * entry = c->tables + i;
		if (entry->table == table) {
			if (entry->symbol == NullSymbol) {
				return 0; // masked
			}
			if (entry->length == length && memcmp(entry->symbol, symbol, length) == 0) {
				return 1;
			}
		}
	}
	return 0;
}

static int ParserContext_matchSymbol(ParserContext *c, symbol_t table) 
{
	long i;
	for (i = c->tableSize - 1; i >= 0; i--) {
		SymbolTableEntry * entry = c->tables + i;
		if (entry->table == table) {
			if (entry->symbol == NullSymbol) {
				return 0; // masked
			}
			return ParserContext_match(c, entry->symbol, entry->length);
		}
	}
	return 0;
}

static int ParserContext_equals(ParserContext *c, symbol_t table, const unsigned char *ppos) {
	long i;
	size_t length = c->pos - ppos;
	for (i = c->tableSize - 1; i >= 0; i--) {
		SymbolTableEntry * entry = c->tables + i;
		if (entry->table == table) {
			if (entry->symbol == NullSymbol) {
				return 0; // masked
			}
			return (entry->length == length && memcmp(entry->symbol, ppos, length) == 0);
		}
	}
	return 0;
}

static int ParserContext_contains(ParserContext *c, symbol_t table, const unsigned char *ppos) 
{
	long i;
	size_t length = c->pos - ppos;
	for (i = c->tableSize - 1; i >= 0; i--) {
		SymbolTableEntry * entry = c->tables + i;
		if (entry->table == table) {
			if (entry->symbol == NullSymbol) {
				return 0; // masked
			}
			if (length == entry->length && memcmp(ppos, entry->symbol, length) == 0) {
				return 1;
			}
		}
	}
	return 0;
}


// Counter -----------------------------------------------------------------

static void ParserContext_scanCount(ParserContext *c, const unsigned char *ppos, long mask, int shift) 
{
	long i;
	size_t length = c->pos - ppos;
	if (mask == 0) {
		c->count = strtol((const char*)ppos, NULL, 10);
	} else {
		long n = 0;
		const unsigned char *p = ppos;
		while(p < c->pos) {
			n <<= 8;
			n |= (*p & 0xff);
			p++;
		}
		c->count = (n & mask) >> shift;
	}
}

static int ParserContext_decCount(ParserContext *c) 
{
	return (c->count--) > 0;
}

// Memotable ------------------------------------------------------------

static
void ParserContext_initMemo(ParserContext *c, int w, int n)
{
    int i;
    c->memoSize = w * n + 1;
    c->memoArray = (MemoEntry *)_calloc(sizeof(MemoEntry), c->memoSize);
    for (i = 0; i < c->memoSize; i++) {
        c->memoArray[i].key = -1LL;
    }
}

static  uniquekey_t longkey( uniquekey_t pos, int memoPoint) {
    return ((pos << 12) | memoPoint);
}

static
int ParserContext_memoLookup(ParserContext *c, int memoPoint)
{
    uniquekey_t key = longkey((c->pos - c->inputs), memoPoint);
    unsigned int hash = (unsigned int) (key % c->memoSize);
    MemoEntry* m = c->memoArray + hash;
    if (m->key == key) {
        c->pos += m->consumed;
        return m->result;
    }
    return NotFound;
}

static
int ParserContext_memoLookupTree(ParserContext *c, int memoPoint)
{
    uniquekey_t key = longkey((c->pos - c->inputs), memoPoint);
    unsigned int hash = (unsigned int) (key % c->memoSize);
    MemoEntry* m = c->memoArray + hash;
    if (m->key == key) {
        c->pos += m->consumed;
    	GCSET(c, c->left, m->memoTree);
        c->left = m->memoTree;
        return m->result;
    }
    return NotFound;
}

static
void ParserContext_memoSucc(ParserContext *c, int memoPoint, const unsigned char* ppos)
{
    uniquekey_t key = longkey((ppos - c->inputs), memoPoint);
    unsigned int hash = (unsigned int) (key % c->memoSize);
    MemoEntry* m = c->memoArray + hash;
    m->key = key;
    GCSET(c, m->memoTree, c->left);
    m->memoTree = c->left;
    m->consumed = c->pos - ppos;
    m->result = SuccFound;
    m->stateValue = -1;
}

static
void ParserContext_memoTreeSucc(ParserContext *c, int memoPoint, const unsigned char* ppos)
{
    uniquekey_t key = longkey((ppos - c->inputs), memoPoint);
    unsigned int hash = (unsigned int) (key % c->memoSize);
    MemoEntry* m = c->memoArray + hash;
    m->key = key;
    GCSET(c, m->memoTree, c->left);
    m->memoTree = c->left;
    m->consumed = c->pos - ppos;
    m->result = SuccFound;
    m->stateValue = -1;
}

static
void ParserContext_memoFail(ParserContext *c, int memoPoint)
{
	uniquekey_t key = longkey((c->pos - c->inputs), memoPoint);
    unsigned int hash = (unsigned int) (key % c->memoSize);
    MemoEntry* m = c->memoArray + hash;
    m->key = key;
    GCSET(c, m->memoTree, c->left);
    m->memoTree = c->left;
    m->consumed = 0;
    m->result = FailFound;
    m->stateValue = -1;
}

	/* State Version */

//	public final int lookupStateMemo(int memoPoint) {
//		long key = longkey(pos, memoPoint, shift);
//		int hash = (int) (key % memoArray.length);
//		MemoEntry m = c->memoArray[hash];
//		if (m.key == key) {
//			c->pos += m.consumed;
//			return m.result;
//		}
//		return NotFound;
//	}
//
//	public final int lookupStateTreeMemo(int memoPoint) {
//		long key = longkey(pos, memoPoint, shift);
//		int hash = (int) (key % memoArray.length);
//		MemoEntry m = c->memoArray[hash];
//		if (m.key == key && m.stateValue == c->stateValue) {
//			c->pos += m.consumed;
//			c->left = m.memoTree;
//			return m.result;
//		}
//		return NotFound;
//	}
//
//	public void memoStateSucc(int memoPoint, int ppos) {
//		long key = longkey(ppos, memoPoint, shift);
//		int hash = (int) (key % memoArray.length);
//		MemoEntry m = c->memoArray[hash];
//		m.key = key;
//		m.memoTree = left;
//		m.consumed = pos - ppos;
//		m.result = SuccFound;
//		m.stateValue = c->stateValue;
//		// c->CountStored += 1;
//	}
//
//	public void memoStateTreeSucc(int memoPoint, int ppos) {
//		long key = longkey(ppos, memoPoint, shift);
//		int hash = (int) (key % memoArray.length);
//		MemoEntry m = c->memoArray[hash];
//		m.key = key;
//		m.memoTree = left;
//		m.consumed = pos - ppos;
//		m.result = SuccFound;
//		m.stateValue = c->stateValue;
//		// c->CountStored += 1;
//	}
//
//	public void memoStateFail(int memoPoint) {
//		long key = longkey(pos, memoPoint, shift);
//		int hash = (int) (key % memoArray.length);
//		MemoEntry m = c->memoArray[hash];
//		m.key = key;
//		m.memoTree = left;
//		m.consumed = 0;
//		m.result = FailFound;
//		m.stateValue = c->stateValue;
//	}


static void ParserContext_free(ParserContext *c)
{
	size_t i;
    if(c->memoArray != NULL) {
    	for(i = 0; i < c->memoSize; i++) {
    		GCDEC(c, c->memoArray[i].memoTree);
    		c->memoArray[i].memoTree = NULL;
    	}
        _free(c->memoArray);
        c->memoArray = NULL;
    }
    if(c->tables != NULL) {
    	_free(c->tables);
    	c->tables = NULL;
    }
    ParserContext_backLog(c, 0);
    _free(c->logs);
    c->logs = NULL;
    for(i = 0; i < c->stack_size; i++) {
    	GCDEC(c, c->stacks[i].tree);
    	c->stacks[i].tree = NULL;
    }
    _free(c->stacks);
    c->stacks = NULL;
    _free(c);
}

//----------------------------------------------------------------------------

static inline int ParserContext_bitis(ParserContext *c, int *bits, size_t n)
{
	return (bits[n / 32] & (1 << (n % 32))) != 0;
}



long backtrack_length = 0;
long total_backtrack_length = 0;
long backtrack_count = 0;

static int _T = 0;
static int _L = 0;
static int _S = 0;
static const unsigned char _set0[256] = {0,0,0,0,0,0,0,0,0,1,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static const unsigned char _set1[256] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static int _Lname = 1;
static int _TName = 1;
static int _Llist = 2;
static int _TParam = 2;
static int _TList = 3;
static int _Lbody = 3;
static int _TFunctionDecl = 4;
static int _Lexpr = 4;
static int _Lleft = 5;
static int _TInteger = 5;
static int _TTrue = 6;
static int _TFalse = 7;
static const unsigned char _set2[256] = {0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static int _TString = 8;
static int _TNull = 9;
static int _TExpression = 10;
static int _TFunctionExpr = 11;
static int _Lrecv = 6;
static int _TApply = 12;
static int _TNot = 13;
static int _TAssign = 14;
static int _TAssignMul = 15;
static int _TAssignDiv = 16;
static int _TAssignMod = 17;
static int _TAssignAdd = 18;
static int _TAssignSub = 19;
static int _TAssignLeftShift = 20;
static int _TAssignRightShift = 21;
static int _TAssignLogicalRightShift = 22;
static int _TAssignBitwiseAnd = 23;
static int _TAssignBitwiseXOr = 24;
static int _TAssignBitwiseOr = 25;
static int _Lright = 7;
static int _TLessThan = 26;
static int _TLessThanEquals = 27;
static int _TGreaterThan = 28;
static int _TGreaterThanEquals = 29;
static int _TEquals = 30;
static int _TNotEquals = 31;
static int _TAnd = 32;
static int _TOr = 33;
static int _Lcond = 8;
static int _Lthen = 9;
static int _Lelse = 10;
static int _TConditional = 34;
static int _TVarDecl = 35;
static int _TVarList = 36;
static int _TDeclaration = 37;
static int _TBlock = 38;
static int _TIf1 = 39;
static int _TIf2 = 40;
static int _TIf3 = 41;
static int _TIf4 = 42;
static int _TIf5 = 43;
static int _TIf6 = 44;
static int _TIf7 = 45;
static int _TIf8 = 46;
static int _TIf9 = 47;
static int _TIfA = 48;
static int _TIf = 49;
static int _TReturn = 50;
static int _TExpressionStatement = 51;
static int _TSource = 52;
static const char * _tags[53] = {"","Name","Param","List","FunctionDecl","Integer","True","False","String","Null","Expression","FunctionExpr","Apply","Not","Assign","AssignMul","AssignDiv","AssignMod","AssignAdd","AssignSub","AssignLeftShift","AssignRightShift","AssignLogicalRightShift","AssignBitwiseAnd","AssignBitwiseXOr","AssignBitwiseOr","LessThan","LessThanEquals","GreaterThan","GreaterThanEquals","Equals","NotEquals","And","Or","Conditional","VarDecl","VarList","Declaration","Block","If1","If2","If3","If4","If5","If6","If7","If8","If9","IfA","If","Return","ExpressionStatement","Source"};
static const char * _labels[11] = {"","name","list","body","expr","left","recv","right","cond","then","else"};
static const char * _tables[1] = {""};
// Prototypes
int e73(ParserContext *c);
int p_046UnaryExpression(ParserContext *c);
int e9(ParserContext *c);
int p_046Expression(ParserContext *c);
int e43(ParserContext *c);
int p_046Block(ParserContext *c);
int p_046AssignmentExpression(ParserContext *c);
int p_046FunctionCall(ParserContext *c);
int p_046RelationalExpression(ParserContext *c);
int e29(ParserContext *c);
static inline int p_046S(ParserContext * c){
   if (!_set0[ParserContext_read(c)]){
      return 0;
   }
   return 1;
}
static inline int e3(ParserContext * c){
   if (ParserContext_read(c) != 42){
      return 0;
   }
   if (ParserContext_read(c) != 47){
      return 0;
   }
   return 1;
}
static inline int e2(ParserContext * c){
   {
      const unsigned char * pos = c->pos;
      if (e3(c)){
         return 0;
      }
      backtrack_count = backtrack_count + 1;
      long length = c->pos-pos;
      total_backtrack_length = total_backtrack_length + length;
      if (backtrack_length < length){
         backtrack_length = length;
      }
      c->pos = pos;
   }
   if (ParserContext_read(c) == 0){
      return 0;
   }
   return 1;
}
static inline int p_046BLOCKCOMMENT(ParserContext * c){
   if (ParserContext_read(c) != 47){
      return 0;
   }
   if (ParserContext_read(c) != 42){
      return 0;
   }
while (1){
      const unsigned char * pos = c->pos;
      if (!e2(c)){
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         break;
      }
   }
   if (ParserContext_read(c) != 42){
      return 0;
   }
   if (ParserContext_read(c) != 47){
      return 0;
   }
   return 1;
}
static inline int e4(ParserContext * c){
   if (ParserContext_prefetch(c) == 10){
      return 0;
   }
   if (ParserContext_read(c) == 0){
      return 0;
   }
   return 1;
}
static inline int p_046LINECOMMENT(ParserContext * c){
   if (ParserContext_read(c) != 47){
      return 0;
   }
   if (ParserContext_read(c) != 47){
      return 0;
   }
while (1){
      const unsigned char * pos = c->pos;
      if (!e4(c)){
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         break;
      }
   }
   return 1;
}
static inline int e1(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      if (p_046S(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
      }
   }
   if (temp){
      const unsigned char * pos2 = c->pos;
      if (p_046BLOCKCOMMENT(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos2;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos2;
      }
   }
   if (temp){
      const unsigned char * pos3 = c->pos;
      if (p_046LINECOMMENT(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos3;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos3;
      }
   }
   if (temp){
      return 0;
   }
   return 1;
}
static inline int p_046_(ParserContext * c){
while (1){
      const unsigned char * pos = c->pos;
      if (!e1(c)){
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         break;
      }
   }
   return 1;
}
static inline int p_04634336134(ParserContext * c){
   if (ParserContext_read(c) != 33){
      return 0;
   }
   if (ParserContext_read(c) != 61){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int e75(ParserContext * c){
   if (!p_04634336134(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TNotEquals);
   return 1;
}
static inline int p_04634616134(ParserContext * c){
   if (ParserContext_read(c) != 61){
      return 0;
   }
   if (ParserContext_read(c) != 61){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int e74(ParserContext * c){
   if (!p_04634616134(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TEquals);
   return 1;
}
static inline int p_046343334(ParserContext * c){
   if (ParserContext_read(c) != 33){
      return 0;
   }
   if (ParserContext_prefetch(c) == 61){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int e50(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_046343334(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046UnaryExpression(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lexpr);
      ParserContext_backTree(c,left);
   }
   ParserContext_tagTree(c,_TNot);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int p_046344434(ParserContext * c){
   if (ParserContext_read(c) != 44){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_0463412412434(ParserContext * c){
   if (ParserContext_read(c) != 124){
      return 0;
   }
   if (ParserContext_read(c) != 124){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_04634383834(ParserContext * c){
   if (ParserContext_read(c) != 38){
      return 0;
   }
   if (ParserContext_read(c) != 38){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_04634626134(ParserContext * c){
   if (ParserContext_read(c) != 62){
      return 0;
   }
   if (ParserContext_read(c) != 61){
      return 0;
   }
   if (ParserContext_prefetch(c) == 61){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int e72(ParserContext * c){
   if (!p_04634626134(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TGreaterThanEquals);
   return 1;
}
static inline int p_046346034(ParserContext * c){
   if (ParserContext_read(c) != 60){
      return 0;
   }
   if (ParserContext_prefetch(c) == 61){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int e69(ParserContext * c){
   if (!p_046346034(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TLessThan);
   return 1;
}
static inline int p_046346234(ParserContext * c){
   if (ParserContext_read(c) != 62){
      return 0;
   }
   if (ParserContext_prefetch(c) == 61){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int e71(ParserContext * c){
   if (!p_046346234(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TGreaterThan);
   return 1;
}
static inline int p_04634606134(ParserContext * c){
   if (ParserContext_read(c) != 60){
      return 0;
   }
   if (ParserContext_read(c) != 61){
      return 0;
   }
   if (ParserContext_prefetch(c) == 61){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int e70(ParserContext * c){
   if (!p_04634606134(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TLessThanEquals);
   return 1;
}
static inline int e68(ParserContext * c){
   ParserContext_foldTree(c,0,_Lleft);
   {
      int temp = 1;
      if (temp){
         const unsigned char * pos = c->pos;
         size_t left = ParserContext_saveTree(c);
         size_t log = ParserContext_saveLog(c);
         if (e69(c)){
            temp = 0;
         } else{
            backtrack_count = backtrack_count + 1;
            long length = c->pos-pos;
            total_backtrack_length = total_backtrack_length + length;
            if (backtrack_length < length){
               backtrack_length = length;
            }
            c->pos = pos;
            ParserContext_backTree(c,left);
            ParserContext_backLog(c,log);
         }
      }
      if (temp){
         const unsigned char * pos4 = c->pos;
         size_t left5 = ParserContext_saveTree(c);
         size_t log6 = ParserContext_saveLog(c);
         if (e70(c)){
            temp = 0;
         } else{
            backtrack_count = backtrack_count + 1;
            long length = c->pos-pos4;
            total_backtrack_length = total_backtrack_length + length;
            if (backtrack_length < length){
               backtrack_length = length;
            }
            c->pos = pos4;
            ParserContext_backTree(c,left5);
            ParserContext_backLog(c,log6);
         }
      }
      if (temp){
         const unsigned char * pos7 = c->pos;
         size_t left8 = ParserContext_saveTree(c);
         size_t log9 = ParserContext_saveLog(c);
         if (e71(c)){
            temp = 0;
         } else{
            backtrack_count = backtrack_count + 1;
            long length = c->pos-pos7;
            total_backtrack_length = total_backtrack_length + length;
            if (backtrack_length < length){
               backtrack_length = length;
            }
            c->pos = pos7;
            ParserContext_backTree(c,left8);
            ParserContext_backLog(c,log9);
         }
      }
      if (temp){
         const unsigned char * pos10 = c->pos;
         size_t left11 = ParserContext_saveTree(c);
         size_t log12 = ParserContext_saveLog(c);
         if (e72(c)){
            temp = 0;
         } else{
            backtrack_count = backtrack_count + 1;
            long length = c->pos-pos10;
            total_backtrack_length = total_backtrack_length + length;
            if (backtrack_length < length){
               backtrack_length = length;
            }
            c->pos = pos10;
            ParserContext_backTree(c,left11);
            ParserContext_backLog(c,log12);
         }
      }
      if (temp){
         return 0;
      }
   }
   {
      size_t left13 = ParserContext_saveTree(c);
      if (!p_046UnaryExpression(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lright);
      ParserContext_backTree(c,left13);
   }
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e66(ParserContext * c){
   if (!p_046RelationalExpression(c)){
      return 0;
   }
while (1){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!e73(c)){
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         break;
      }
   }
   return 1;
}
static inline int p_046EqualityExpression(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,27);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e66(c)){
         ParserContext_memoTreeSucc(c,27,pos);
         return 1;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         ParserContext_memoFail(c,27);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e76(ParserContext * c){
   ParserContext_foldTree(c,0,_Lleft);
   if (!p_04634383834(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046EqualityExpression(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lright);
      ParserContext_backTree(c,left);
   }
   ParserContext_tagTree(c,_TAnd);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e65(ParserContext * c){
   if (!p_046EqualityExpression(c)){
      return 0;
   }
while (1){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!e76(c)){
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         break;
      }
   }
   return 1;
}
static inline int p_046LogicalAndExpression(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,26);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e65(c)){
         ParserContext_memoTreeSucc(c,26,pos);
         return 1;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         ParserContext_memoFail(c,26);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e77(ParserContext * c){
   ParserContext_foldTree(c,0,_Lleft);
   if (!p_0463412412434(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046LogicalAndExpression(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lright);
      ParserContext_backTree(c,left);
   }
   ParserContext_tagTree(c,_TOr);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e64(ParserContext * c){
   if (!p_046LogicalAndExpression(c)){
      return 0;
   }
while (1){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!e77(c)){
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         break;
      }
   }
   return 1;
}
static inline int p_046LogicalOrExpression(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,25);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e64(c)){
         ParserContext_memoTreeSucc(c,25,pos);
         return 1;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         ParserContext_memoFail(c,25);
         return 0;
      }
   }
   return memo == 1;
}
static inline int p_046345834(ParserContext * c){
   if (ParserContext_read(c) != 58){
      return 0;
   }
   if (ParserContext_prefetch(c) == 62){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_046346334(ParserContext * c){
   if (ParserContext_read(c) != 63){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int e78(ParserContext * c){
   ParserContext_foldTree(c,0,_Lcond);
   if (!p_046346334(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046Expression(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lthen);
      ParserContext_backTree(c,left);
   }
   if (!p_046345834(c)){
      return 0;
   }
   {
      size_t left1 = ParserContext_saveTree(c);
      if (!p_046LogicalOrExpression(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lelse);
      ParserContext_backTree(c,left1);
   }
   ParserContext_tagTree(c,_TConditional);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e63(ParserContext * c){
   if (!p_046LogicalOrExpression(c)){
      return 0;
   }
while (1){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!e78(c)){
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         break;
      }
   }
   return 1;
}
static inline int p_046ConditionalExpression(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,24);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e63(c)){
         ParserContext_memoTreeSucc(c,24,pos);
         return 1;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         ParserContext_memoFail(c,24);
         return 0;
      }
   }
   return memo == 1;
}
static inline int p_046346134(ParserContext * c){
   if (ParserContext_read(c) != 61){
      return 0;
   }
   if (ParserContext_prefetch(c) == 61){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int e51(ParserContext * c){
   if (!p_046346134(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TAssign);
   return 1;
}
static inline int p_046341246134(ParserContext * c){
   if (ParserContext_read(c) != 124){
      return 0;
   }
   if (ParserContext_read(c) != 61){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int e62(ParserContext * c){
   if (!p_046341246134(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TAssignBitwiseOr);
   return 1;
}
static inline int p_04634946134(ParserContext * c){
   if (ParserContext_read(c) != 94){
      return 0;
   }
   if (ParserContext_read(c) != 61){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int e61(ParserContext * c){
   if (!p_04634946134(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TAssignBitwiseXOr);
   return 1;
}
static inline int p_04634476134(ParserContext * c){
   if (ParserContext_read(c) != 47){
      return 0;
   }
   if (ParserContext_read(c) != 61){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int e53(ParserContext * c){
   if (!p_04634476134(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TAssignDiv);
   return 1;
}
static inline int p_04634426134(ParserContext * c){
   if (ParserContext_read(c) != 42){
      return 0;
   }
   if (ParserContext_read(c) != 61){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int e52(ParserContext * c){
   if (!p_04634426134(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TAssignMul);
   return 1;
}
static inline int p_04634436134(ParserContext * c){
   if (ParserContext_read(c) != 43){
      return 0;
   }
   if (ParserContext_read(c) != 61){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int e55(ParserContext * c){
   if (!p_04634436134(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TAssignAdd);
   return 1;
}
static inline int p_04634376134(ParserContext * c){
   if (ParserContext_read(c) != 37){
      return 0;
   }
   if (ParserContext_read(c) != 61){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int e54(ParserContext * c){
   if (!p_04634376134(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TAssignMod);
   return 1;
}
static inline int p_0463460606134(ParserContext * c){
   if (ParserContext_read(c) != 60){
      return 0;
   }
   if (ParserContext_read(c) != 60){
      return 0;
   }
   if (ParserContext_read(c) != 61){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int e57(ParserContext * c){
   if (!p_0463460606134(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TAssignLeftShift);
   return 1;
}
static inline int p_04634456134(ParserContext * c){
   if (ParserContext_read(c) != 45){
      return 0;
   }
   if (ParserContext_read(c) != 61){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int e56(ParserContext * c){
   if (!p_04634456134(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TAssignSub);
   return 1;
}
static inline int p_046346262626134(ParserContext * c){
   if (ParserContext_read(c) != 62){
      return 0;
   }
   if (ParserContext_read(c) != 62){
      return 0;
   }
   if (ParserContext_read(c) != 62){
      return 0;
   }
   if (ParserContext_read(c) != 61){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int e59(ParserContext * c){
   if (!p_046346262626134(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TAssignLogicalRightShift);
   return 1;
}
static inline int p_0463462626134(ParserContext * c){
   if (ParserContext_read(c) != 62){
      return 0;
   }
   if (ParserContext_read(c) != 62){
      return 0;
   }
   if (ParserContext_read(c) != 61){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int e58(ParserContext * c){
   if (!p_0463462626134(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TAssignRightShift);
   return 1;
}
static inline int p_04634386134(ParserContext * c){
   if (ParserContext_read(c) != 38){
      return 0;
   }
   if (ParserContext_read(c) != 61){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int e60(ParserContext * c){
   if (!p_04634386134(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TAssignBitwiseAnd);
   return 1;
}
static inline int p_046_AssignmentOperator(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e51(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
      }
   }
   if (temp){
      const unsigned char * pos4 = c->pos;
      size_t left5 = ParserContext_saveTree(c);
      size_t log6 = ParserContext_saveLog(c);
      if (e52(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos4;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos4;
         ParserContext_backTree(c,left5);
         ParserContext_backLog(c,log6);
      }
   }
   if (temp){
      const unsigned char * pos7 = c->pos;
      size_t left8 = ParserContext_saveTree(c);
      size_t log9 = ParserContext_saveLog(c);
      if (e53(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos7;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos7;
         ParserContext_backTree(c,left8);
         ParserContext_backLog(c,log9);
      }
   }
   if (temp){
      const unsigned char * pos10 = c->pos;
      size_t left11 = ParserContext_saveTree(c);
      size_t log12 = ParserContext_saveLog(c);
      if (e54(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos10;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos10;
         ParserContext_backTree(c,left11);
         ParserContext_backLog(c,log12);
      }
   }
   if (temp){
      const unsigned char * pos13 = c->pos;
      size_t left14 = ParserContext_saveTree(c);
      size_t log15 = ParserContext_saveLog(c);
      if (e55(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos13;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos13;
         ParserContext_backTree(c,left14);
         ParserContext_backLog(c,log15);
      }
   }
   if (temp){
      const unsigned char * pos16 = c->pos;
      size_t left17 = ParserContext_saveTree(c);
      size_t log18 = ParserContext_saveLog(c);
      if (e56(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos16;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos16;
         ParserContext_backTree(c,left17);
         ParserContext_backLog(c,log18);
      }
   }
   if (temp){
      const unsigned char * pos19 = c->pos;
      size_t left20 = ParserContext_saveTree(c);
      size_t log21 = ParserContext_saveLog(c);
      if (e57(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos19;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos19;
         ParserContext_backTree(c,left20);
         ParserContext_backLog(c,log21);
      }
   }
   if (temp){
      const unsigned char * pos22 = c->pos;
      size_t left23 = ParserContext_saveTree(c);
      size_t log24 = ParserContext_saveLog(c);
      if (e58(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos22;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos22;
         ParserContext_backTree(c,left23);
         ParserContext_backLog(c,log24);
      }
   }
   if (temp){
      const unsigned char * pos25 = c->pos;
      size_t left26 = ParserContext_saveTree(c);
      size_t log27 = ParserContext_saveLog(c);
      if (e59(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos25;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos25;
         ParserContext_backTree(c,left26);
         ParserContext_backLog(c,log27);
      }
   }
   if (temp){
      const unsigned char * pos28 = c->pos;
      size_t left29 = ParserContext_saveTree(c);
      size_t log30 = ParserContext_saveLog(c);
      if (e60(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos28;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos28;
         ParserContext_backTree(c,left29);
         ParserContext_backLog(c,log30);
      }
   }
   if (temp){
      const unsigned char * pos31 = c->pos;
      size_t left32 = ParserContext_saveTree(c);
      size_t log33 = ParserContext_saveLog(c);
      if (e61(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos31;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos31;
         ParserContext_backTree(c,left32);
         ParserContext_backLog(c,log33);
      }
   }
   if (temp){
      const unsigned char * pos34 = c->pos;
      size_t left35 = ParserContext_saveTree(c);
      size_t log36 = ParserContext_saveLog(c);
      if (e62(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos34;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos34;
         ParserContext_backTree(c,left35);
         ParserContext_backLog(c,log36);
      }
   }
   if (temp){
      return 0;
   }
   return 1;
}
static inline int e49(ParserContext * c){
   if (!p_046344434(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046AssignmentExpression(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
   return 1;
}
static inline int p_046_ArgumentExpressionList(ParserContext * c){
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046AssignmentExpression(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
while (1){
      const unsigned char * pos = c->pos;
      size_t left2 = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!e49(c)){
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         ParserContext_backTree(c,left2);
         ParserContext_backLog(c,log);
         break;
      }
   }
   return 1;
}
static inline int p_046344034(ParserContext * c){
   if (ParserContext_read(c) != 40){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_046344134(ParserContext * c){
   if (ParserContext_read(c) != 41){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_046_FunctionCall(ParserContext * c){
   {
      size_t left = ParserContext_saveTree(c);
      ParserContext_beginTree(c,0);
      if (!p_046344034(c)){
         return 0;
      }
      const unsigned char * pos = c->pos;
      size_t left2 = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!p_046_ArgumentExpressionList(c)){
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         ParserContext_backTree(c,left2);
         ParserContext_backLog(c,log);
      }
      if (!p_046344134(c)){
         return 0;
      }
      ParserContext_tagTree(c,_TList);
      ParserContext_endTree(c,0,_T,NULL, 0);
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
   ParserContext_tagTree(c,_TApply);
   return 1;
}
static inline int e48(ParserContext * c){
   ParserContext_foldTree(c,0,_Lrecv);
   if (!p_046_FunctionCall(c)){
      return 0;
   }
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int p_046STRING_CONTENT(ParserContext * c){
   if (_set2[ParserContext_prefetch(c)]){
      return 0;
   }
   if (ParserContext_read(c) == 0){
      return 0;
   }
   return 1;
}
static inline int e41(ParserContext * c){
   if (ParserContext_read(c) != 34){
      return 0;
   }
   ParserContext_beginTree(c,0);
while (1){
      const unsigned char * pos = c->pos;
      if (!p_046STRING_CONTENT(c)){
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         break;
      }
   }
   ParserContext_tagTree(c,_TString);
   ParserContext_endTree(c,0,_T,NULL, 0);
   if (ParserContext_read(c) != 34){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_046StringLiteral(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,20);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e41(c)){
         ParserContext_memoTreeSucc(c,20,pos);
         return 1;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         ParserContext_memoFail(c,20);
         return 0;
      }
   }
   return memo == 1;
}
static inline int p_046DIGIT(ParserContext * c){
   if (!(48 <= ParserContext_prefetch(c) && ParserContext_read(c) < 58)){
      return 0;
   }
   return 1;
}
static inline int p_046W(ParserContext * c){
   if (!_set1[ParserContext_read(c)]){
      return 0;
   }
   return 1;
}
static inline int e15(ParserContext * c){
   if (ParserContext_read(c) != 101){
      return 0;
   }
   if (ParserContext_read(c) != 108){
      return 0;
   }
   if (ParserContext_read(c) != 115){
      return 0;
   }
   if (ParserContext_read(c) != 101){
      return 0;
   }
   return 1;
}
static inline int e17(ParserContext * c){
   if (ParserContext_read(c) != 114){
      return 0;
   }
   if (ParserContext_read(c) != 101){
      return 0;
   }
   if (ParserContext_read(c) != 116){
      return 0;
   }
   if (ParserContext_read(c) != 117){
      return 0;
   }
   if (ParserContext_read(c) != 114){
      return 0;
   }
   if (ParserContext_read(c) != 110){
      return 0;
   }
   return 1;
}
static inline int e16(ParserContext * c){
   if (ParserContext_read(c) != 105){
      return 0;
   }
   if (ParserContext_read(c) != 102){
      return 0;
   }
   return 1;
}
static inline int e19(ParserContext * c){
   if (ParserContext_read(c) != 118){
      return 0;
   }
   if (ParserContext_read(c) != 97){
      return 0;
   }
   if (ParserContext_read(c) != 114){
      return 0;
   }
   return 1;
}
static inline int e18(ParserContext * c){
   if (ParserContext_read(c) != 102){
      return 0;
   }
   if (ParserContext_read(c) != 117){
      return 0;
   }
   if (ParserContext_read(c) != 110){
      return 0;
   }
   if (ParserContext_read(c) != 99){
      return 0;
   }
   if (ParserContext_read(c) != 116){
      return 0;
   }
   if (ParserContext_read(c) != 105){
      return 0;
   }
   if (ParserContext_read(c) != 111){
      return 0;
   }
   if (ParserContext_read(c) != 110){
      return 0;
   }
   return 1;
}
static inline int p_046KEYWORD(ParserContext * c){
   {
      int temp = 1;
      if (temp){
         const unsigned char * pos = c->pos;
         if (e15(c)){
            temp = 0;
         } else{
            backtrack_count = backtrack_count + 1;
            long length = c->pos-pos;
            total_backtrack_length = total_backtrack_length + length;
            if (backtrack_length < length){
               backtrack_length = length;
            }
            c->pos = pos;
         }
      }
      if (temp){
         const unsigned char * pos2 = c->pos;
         if (e16(c)){
            temp = 0;
         } else{
            backtrack_count = backtrack_count + 1;
            long length = c->pos-pos2;
            total_backtrack_length = total_backtrack_length + length;
            if (backtrack_length < length){
               backtrack_length = length;
            }
            c->pos = pos2;
         }
      }
      if (temp){
         const unsigned char * pos3 = c->pos;
         if (e17(c)){
            temp = 0;
         } else{
            backtrack_count = backtrack_count + 1;
            long length = c->pos-pos3;
            total_backtrack_length = total_backtrack_length + length;
            if (backtrack_length < length){
               backtrack_length = length;
            }
            c->pos = pos3;
         }
      }
      if (temp){
         const unsigned char * pos4 = c->pos;
         if (e18(c)){
            temp = 0;
         } else{
            backtrack_count = backtrack_count + 1;
            long length = c->pos-pos4;
            total_backtrack_length = total_backtrack_length + length;
            if (backtrack_length < length){
               backtrack_length = length;
            }
            c->pos = pos4;
         }
      }
      if (temp){
         const unsigned char * pos5 = c->pos;
         if (e19(c)){
            temp = 0;
         } else{
            backtrack_count = backtrack_count + 1;
            long length = c->pos-pos5;
            total_backtrack_length = total_backtrack_length + length;
            if (backtrack_length < length){
               backtrack_length = length;
            }
            c->pos = pos5;
         }
      }
      if (temp){
         return 0;
      }
   }
   {
      const unsigned char * pos6 = c->pos;
      if (p_046W(c)){
         return 0;
      }
      backtrack_count = backtrack_count + 1;
      long length = c->pos-pos6;
      total_backtrack_length = total_backtrack_length + length;
      if (backtrack_length < length){
         backtrack_length = length;
      }
      c->pos = pos6;
   }
   return 1;
}
static inline int p_046NAME(ParserContext * c){
   {
      const unsigned char * pos = c->pos;
      if (p_046DIGIT(c)){
         return 0;
      }
      backtrack_count = backtrack_count + 1;
      long length = c->pos-pos;
      total_backtrack_length = total_backtrack_length + length;
      if (backtrack_length < length){
         backtrack_length = length;
      }
      c->pos = pos;
   }
   {
      const unsigned char * pos1 = c->pos;
      if (p_046KEYWORD(c)){
         return 0;
      }
      backtrack_count = backtrack_count + 1;
      long length = c->pos-pos1;
      total_backtrack_length = total_backtrack_length + length;
      if (backtrack_length < length){
         backtrack_length = length;
      }
      c->pos = pos1;
   }
   if (!p_046W(c)){
      return 0;
   }
while (1){
      const unsigned char * pos2 = c->pos;
      if (!p_046W(c)){
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos2;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos2;
         break;
      }
   }
   return 1;
}
static inline int e14(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_046NAME(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TName);
   ParserContext_endTree(c,0,_T,NULL, 0);
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_046Identifier(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,5);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e14(c)){
         ParserContext_memoTreeSucc(c,5,pos);
         return 1;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         ParserContext_memoFail(c,5);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e22(ParserContext * c){
   ParserContext_beginTree(c,0);
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046Identifier(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lname);
      ParserContext_backTree(c,left);
   }
   ParserContext_tagTree(c,_TParam);
   ParserContext_endTree(c,0,_T,NULL, 0);
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_046FunctionParam(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,7);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e22(c)){
         ParserContext_memoTreeSucc(c,7,pos);
         return 1;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         ParserContext_memoFail(c,7);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e23(ParserContext * c){
   if (!p_046344434(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046FunctionParam(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
   return 1;
}
static inline int e21(ParserContext * c){
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046FunctionParam(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
while (1){
      const unsigned char * pos = c->pos;
      size_t left2 = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!e23(c)){
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         ParserContext_backTree(c,left2);
         ParserContext_backLog(c,log);
         break;
      }
   }
   return 1;
}
static inline int e20(ParserContext * c){
   ParserContext_beginTree(c,0);
   const unsigned char * pos = c->pos;
   size_t left = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e21(c)){
      backtrack_count = backtrack_count + 1;
      long length = c->pos-pos;
      total_backtrack_length = total_backtrack_length + length;
      if (backtrack_length < length){
         backtrack_length = length;
      }
      c->pos = pos;
      ParserContext_backTree(c,left);
      ParserContext_backLog(c,log);
   }
   ParserContext_tagTree(c,_TList);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int p_046FunctionParamList(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,6);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e20(c)){
         ParserContext_memoTreeSucc(c,6,pos);
         return 1;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         ParserContext_memoFail(c,6);
         return 0;
      }
   }
   return memo == 1;
}
static inline int p_04634585834(ParserContext * c){
   if (ParserContext_read(c) != 58){
      return 0;
   }
   if (ParserContext_read(c) != 58){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_046Initializer(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,11);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (p_046AssignmentExpression(c)){
         ParserContext_memoTreeSucc(c,11,pos);
         return 1;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         ParserContext_memoFail(c,11);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e27(ParserContext * c){
   if (!p_046346134(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046Initializer(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lexpr);
      ParserContext_backTree(c,left);
   }
   return 1;
}
static inline int e26(ParserContext * c){
   ParserContext_beginTree(c,0);
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046Identifier(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lname);
      ParserContext_backTree(c,left);
   }
   const unsigned char * pos = c->pos;
   size_t left2 = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e27(c)){
      backtrack_count = backtrack_count + 1;
      long length = c->pos-pos;
      total_backtrack_length = total_backtrack_length + length;
      if (backtrack_length < length){
         backtrack_length = length;
      }
      c->pos = pos;
      ParserContext_backTree(c,left2);
      ParserContext_backLog(c,log);
   }
   ParserContext_tagTree(c,_TVarDecl);
   ParserContext_endTree(c,0,_T,NULL, 0);
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_046InitDecl(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,10);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e26(c)){
         ParserContext_memoTreeSucc(c,10,pos);
         return 1;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         ParserContext_memoFail(c,10);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e79(ParserContext * c){
   if (!p_046344434(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046InitDecl(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
   return 1;
}
static inline int p_046345934(ParserContext * c){
   if (ParserContext_read(c) != 59){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int e25(ParserContext * c){
   ParserContext_beginTree(c,0);
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046InitDecl(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
while (1){
      const unsigned char * pos = c->pos;
      size_t left2 = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!e79(c)){
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         ParserContext_backTree(c,left2);
         ParserContext_backLog(c,log);
         break;
      }
   }
   ParserContext_tagTree(c,_TVarList);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int p_046VariableList(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,9);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e25(c)){
         ParserContext_memoTreeSucc(c,9,pos);
         return 1;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         ParserContext_memoFail(c,9);
         return 0;
      }
   }
   return memo == 1;
}
static inline int p_04634var34(ParserContext * c){
   if (ParserContext_read(c) != 118){
      return 0;
   }
   if (ParserContext_read(c) != 97){
      return 0;
   }
   if (ParserContext_read(c) != 114){
      return 0;
   }
   {
      const unsigned char * pos = c->pos;
      if (p_046W(c)){
         return 0;
      }
      backtrack_count = backtrack_count + 1;
      long length = c->pos-pos;
      total_backtrack_length = total_backtrack_length + length;
      if (backtrack_length < length){
         backtrack_length = length;
      }
      c->pos = pos;
   }
   return 1;
}
static inline int e24(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_04634var34(c)){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046VariableList(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Llist);
      ParserContext_backTree(c,left);
   }
   if (!p_046345934(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TDeclaration);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int p_046VariableDeclaration(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,8);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e24(c)){
         ParserContext_memoTreeSucc(c,8,pos);
         return 1;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         ParserContext_memoFail(c,8);
         return 0;
      }
   }
   return memo == 1;
}
static inline int p_04634function34(ParserContext * c){
   if (ParserContext_read(c) != 102){
      return 0;
   }
   if (ParserContext_read(c) != 117){
      return 0;
   }
   if (ParserContext_read(c) != 110){
      return 0;
   }
   if (ParserContext_read(c) != 99){
      return 0;
   }
   if (ParserContext_read(c) != 116){
      return 0;
   }
   if (ParserContext_read(c) != 105){
      return 0;
   }
   if (ParserContext_read(c) != 111){
      return 0;
   }
   if (ParserContext_read(c) != 110){
      return 0;
   }
   {
      const unsigned char * pos = c->pos;
      if (p_046W(c)){
         return 0;
      }
      backtrack_count = backtrack_count + 1;
      long length = c->pos-pos;
      total_backtrack_length = total_backtrack_length + length;
      if (backtrack_length < length){
         backtrack_length = length;
      }
      c->pos = pos;
   }
   return 1;
}
static inline int e13(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_04634function34(c)){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046Identifier(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lname);
      ParserContext_backTree(c,left);
   }
   if (!p_046_(c)){
      return 0;
   }
   if (!p_046344034(c)){
      return 0;
   }
   {
      size_t left1 = ParserContext_saveTree(c);
      if (!p_046FunctionParamList(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Llist);
      ParserContext_backTree(c,left1);
   }
   if (!p_046344134(c)){
      return 0;
   }
   {
      size_t left2 = ParserContext_saveTree(c);
      if (!p_046Block(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lbody);
      ParserContext_backTree(c,left2);
   }
   ParserContext_tagTree(c,_TFunctionDecl);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int p_046FunctionDeclaration(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,4);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e13(c)){
         ParserContext_memoTreeSucc(c,4,pos);
         return 1;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         ParserContext_memoFail(c,4);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e12(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (p_046FunctionDeclaration(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
      }
   }
   if (temp){
      const unsigned char * pos4 = c->pos;
      size_t left5 = ParserContext_saveTree(c);
      size_t log6 = ParserContext_saveLog(c);
      if (p_046VariableDeclaration(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos4;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos4;
         ParserContext_backTree(c,left5);
         ParserContext_backLog(c,log6);
      }
   }
   if (temp){
      return 0;
   }
   return 1;
}
static inline int p_046Declaration(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,3);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e12(c)){
         ParserContext_memoTreeSucc(c,3,pos);
         return 1;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         ParserContext_memoFail(c,3);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e11(ParserContext * c){
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046Declaration(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_04634if34(ParserContext * c){
   if (ParserContext_read(c) != 105){
      return 0;
   }
   if (ParserContext_read(c) != 102){
      return 0;
   }
   {
      const unsigned char * pos = c->pos;
      if (p_046W(c)){
         return 0;
      }
      backtrack_count = backtrack_count + 1;
      long length = c->pos-pos;
      total_backtrack_length = total_backtrack_length + length;
      if (backtrack_length < length){
         backtrack_length = length;
      }
      c->pos = pos;
   }
   return 1;
}
static inline int e84(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_04634if34(c)){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   if (!p_046344034(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046Expression(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lcond);
      ParserContext_backTree(c,left);
   }
   if (!p_046344134(c)){
      return 0;
   }
   {
      size_t left1 = ParserContext_saveTree(c);
      if (!p_046Block(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lthen);
      ParserContext_backTree(c,left1);
   }
   if (ParserContext_read(c) != 101){
      return 0;
   }
   if (ParserContext_read(c) != 108){
      return 0;
   }
   if (ParserContext_read(c) != 115){
      return 0;
   }
   if (ParserContext_read(c) != 101){
      return 0;
   }
   if (ParserContext_read(c) != 53){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   {
      size_t left2 = ParserContext_saveTree(c);
      if (!p_046Block(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lelse);
      ParserContext_backTree(c,left2);
   }
   ParserContext_tagTree(c,_TIf5);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e83(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_04634if34(c)){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   if (!p_046344034(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046Expression(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lcond);
      ParserContext_backTree(c,left);
   }
   if (!p_046344134(c)){
      return 0;
   }
   {
      size_t left1 = ParserContext_saveTree(c);
      if (!p_046Block(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lthen);
      ParserContext_backTree(c,left1);
   }
   if (ParserContext_read(c) != 101){
      return 0;
   }
   if (ParserContext_read(c) != 108){
      return 0;
   }
   if (ParserContext_read(c) != 115){
      return 0;
   }
   if (ParserContext_read(c) != 101){
      return 0;
   }
   if (ParserContext_read(c) != 52){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   {
      size_t left2 = ParserContext_saveTree(c);
      if (!p_046Block(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lelse);
      ParserContext_backTree(c,left2);
   }
   ParserContext_tagTree(c,_TIf4);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e94(ParserContext * c){
   ParserContext_beginTree(c,0);
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046Expression(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lexpr);
      ParserContext_backTree(c,left);
   }
   if (!p_046345934(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TExpressionStatement);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e86(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_04634if34(c)){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   if (!p_046344034(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046Expression(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lcond);
      ParserContext_backTree(c,left);
   }
   if (!p_046344134(c)){
      return 0;
   }
   {
      size_t left1 = ParserContext_saveTree(c);
      if (!p_046Block(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lthen);
      ParserContext_backTree(c,left1);
   }
   if (ParserContext_read(c) != 101){
      return 0;
   }
   if (ParserContext_read(c) != 108){
      return 0;
   }
   if (ParserContext_read(c) != 115){
      return 0;
   }
   if (ParserContext_read(c) != 101){
      return 0;
   }
   if (ParserContext_read(c) != 55){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   {
      size_t left2 = ParserContext_saveTree(c);
      if (!p_046Block(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lelse);
      ParserContext_backTree(c,left2);
   }
   ParserContext_tagTree(c,_TIf7);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e85(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_04634if34(c)){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   if (!p_046344034(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046Expression(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lcond);
      ParserContext_backTree(c,left);
   }
   if (!p_046344134(c)){
      return 0;
   }
   {
      size_t left1 = ParserContext_saveTree(c);
      if (!p_046Block(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lthen);
      ParserContext_backTree(c,left1);
   }
   if (ParserContext_read(c) != 101){
      return 0;
   }
   if (ParserContext_read(c) != 108){
      return 0;
   }
   if (ParserContext_read(c) != 115){
      return 0;
   }
   if (ParserContext_read(c) != 101){
      return 0;
   }
   if (ParserContext_read(c) != 54){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   {
      size_t left2 = ParserContext_saveTree(c);
      if (!p_046Block(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lelse);
      ParserContext_backTree(c,left2);
   }
   ParserContext_tagTree(c,_TIf6);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e88(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_04634if34(c)){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   if (!p_046344034(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046Expression(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lcond);
      ParserContext_backTree(c,left);
   }
   if (!p_046344134(c)){
      return 0;
   }
   {
      size_t left1 = ParserContext_saveTree(c);
      if (!p_046Block(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lthen);
      ParserContext_backTree(c,left1);
   }
   if (ParserContext_read(c) != 101){
      return 0;
   }
   if (ParserContext_read(c) != 108){
      return 0;
   }
   if (ParserContext_read(c) != 115){
      return 0;
   }
   if (ParserContext_read(c) != 101){
      return 0;
   }
   if (ParserContext_read(c) != 57){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   {
      size_t left2 = ParserContext_saveTree(c);
      if (!p_046Block(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lelse);
      ParserContext_backTree(c,left2);
   }
   ParserContext_tagTree(c,_TIf9);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e87(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_04634if34(c)){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   if (!p_046344034(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046Expression(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lcond);
      ParserContext_backTree(c,left);
   }
   if (!p_046344134(c)){
      return 0;
   }
   {
      size_t left1 = ParserContext_saveTree(c);
      if (!p_046Block(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lthen);
      ParserContext_backTree(c,left1);
   }
   if (ParserContext_read(c) != 101){
      return 0;
   }
   if (ParserContext_read(c) != 108){
      return 0;
   }
   if (ParserContext_read(c) != 115){
      return 0;
   }
   if (ParserContext_read(c) != 101){
      return 0;
   }
   if (ParserContext_read(c) != 56){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   {
      size_t left2 = ParserContext_saveTree(c);
      if (!p_046Block(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lelse);
      ParserContext_backTree(c,left2);
   }
   ParserContext_tagTree(c,_TIf8);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e89(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_04634if34(c)){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   if (!p_046344034(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046Expression(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lcond);
      ParserContext_backTree(c,left);
   }
   if (!p_046344134(c)){
      return 0;
   }
   {
      size_t left1 = ParserContext_saveTree(c);
      if (!p_046Block(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lthen);
      ParserContext_backTree(c,left1);
   }
   if (ParserContext_read(c) != 101){
      return 0;
   }
   if (ParserContext_read(c) != 108){
      return 0;
   }
   if (ParserContext_read(c) != 115){
      return 0;
   }
   if (ParserContext_read(c) != 101){
      return 0;
   }
   if (ParserContext_read(c) != 65){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   {
      size_t left2 = ParserContext_saveTree(c);
      if (!p_046Block(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lelse);
      ParserContext_backTree(c,left2);
   }
   ParserContext_tagTree(c,_TIfA);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int p_0463412334(ParserContext * c){
   if (ParserContext_read(c) != 123){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_0463412534(ParserContext * c){
   if (ParserContext_read(c) != 125){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int e80(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_04634if34(c)){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   if (!p_046344034(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046Expression(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lcond);
      ParserContext_backTree(c,left);
   }
   if (!p_046344134(c)){
      return 0;
   }
   {
      size_t left1 = ParserContext_saveTree(c);
      if (!p_046Block(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lthen);
      ParserContext_backTree(c,left1);
   }
   if (ParserContext_read(c) != 101){
      return 0;
   }
   if (ParserContext_read(c) != 108){
      return 0;
   }
   if (ParserContext_read(c) != 115){
      return 0;
   }
   if (ParserContext_read(c) != 101){
      return 0;
   }
   if (ParserContext_read(c) != 49){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   {
      size_t left2 = ParserContext_saveTree(c);
      if (!p_046Block(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lelse);
      ParserContext_backTree(c,left2);
   }
   ParserContext_tagTree(c,_TIf1);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int p_04634else34(ParserContext * c){
   if (ParserContext_read(c) != 101){
      return 0;
   }
   if (ParserContext_read(c) != 108){
      return 0;
   }
   if (ParserContext_read(c) != 115){
      return 0;
   }
   if (ParserContext_read(c) != 101){
      return 0;
   }
   {
      const unsigned char * pos = c->pos;
      if (p_046W(c)){
         return 0;
      }
      backtrack_count = backtrack_count + 1;
      long length = c->pos-pos;
      total_backtrack_length = total_backtrack_length + length;
      if (backtrack_length < length){
         backtrack_length = length;
      }
      c->pos = pos;
   }
   return 1;
}
static inline int e91(ParserContext * c){
   if (!p_04634else34(c)){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046Block(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lelse);
      ParserContext_backTree(c,left);
   }
   return 1;
}
static inline int e90(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_04634if34(c)){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   if (!p_046344034(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046Expression(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lcond);
      ParserContext_backTree(c,left);
   }
   if (!p_046344134(c)){
      return 0;
   }
   {
      size_t left1 = ParserContext_saveTree(c);
      if (!p_046Block(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lthen);
      ParserContext_backTree(c,left1);
   }
   const unsigned char * pos = c->pos;
   size_t left3 = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e91(c)){
      backtrack_count = backtrack_count + 1;
      long length = c->pos-pos;
      total_backtrack_length = total_backtrack_length + length;
      if (backtrack_length < length){
         backtrack_length = length;
      }
      c->pos = pos;
      ParserContext_backTree(c,left3);
      ParserContext_backLog(c,log);
   }
   ParserContext_tagTree(c,_TIf);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e82(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_04634if34(c)){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   if (!p_046344034(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046Expression(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lcond);
      ParserContext_backTree(c,left);
   }
   if (!p_046344134(c)){
      return 0;
   }
   {
      size_t left1 = ParserContext_saveTree(c);
      if (!p_046Block(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lthen);
      ParserContext_backTree(c,left1);
   }
   if (ParserContext_read(c) != 101){
      return 0;
   }
   if (ParserContext_read(c) != 108){
      return 0;
   }
   if (ParserContext_read(c) != 115){
      return 0;
   }
   if (ParserContext_read(c) != 101){
      return 0;
   }
   if (ParserContext_read(c) != 51){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   {
      size_t left2 = ParserContext_saveTree(c);
      if (!p_046Block(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lelse);
      ParserContext_backTree(c,left2);
   }
   ParserContext_tagTree(c,_TIf3);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e81(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_04634if34(c)){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   if (!p_046344034(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046Expression(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lcond);
      ParserContext_backTree(c,left);
   }
   if (!p_046344134(c)){
      return 0;
   }
   {
      size_t left1 = ParserContext_saveTree(c);
      if (!p_046Block(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lthen);
      ParserContext_backTree(c,left1);
   }
   if (ParserContext_read(c) != 101){
      return 0;
   }
   if (ParserContext_read(c) != 108){
      return 0;
   }
   if (ParserContext_read(c) != 115){
      return 0;
   }
   if (ParserContext_read(c) != 101){
      return 0;
   }
   if (ParserContext_read(c) != 50){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   {
      size_t left2 = ParserContext_saveTree(c);
      if (!p_046Block(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lelse);
      ParserContext_backTree(c,left2);
   }
   ParserContext_tagTree(c,_TIf2);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int p_04634return34(ParserContext * c){
   if (ParserContext_read(c) != 114){
      return 0;
   }
   if (ParserContext_read(c) != 101){
      return 0;
   }
   if (ParserContext_read(c) != 116){
      return 0;
   }
   if (ParserContext_read(c) != 117){
      return 0;
   }
   if (ParserContext_read(c) != 114){
      return 0;
   }
   if (ParserContext_read(c) != 110){
      return 0;
   }
   {
      const unsigned char * pos = c->pos;
      if (p_046W(c)){
         return 0;
      }
      backtrack_count = backtrack_count + 1;
      long length = c->pos-pos;
      total_backtrack_length = total_backtrack_length + length;
      if (backtrack_length < length){
         backtrack_length = length;
      }
      c->pos = pos;
   }
   return 1;
}
static inline int e93(ParserContext * c){
   size_t left = ParserContext_saveTree(c);
   if (!p_046Expression(c)){
      return 0;
   }
   ParserContext_linkTree(c,_Lexpr);
   ParserContext_backTree(c,left);
   return 1;
}
static inline int e92(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_04634return34(c)){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   const unsigned char * pos = c->pos;
   size_t left = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e93(c)){
      backtrack_count = backtrack_count + 1;
      long length = c->pos-pos;
      total_backtrack_length = total_backtrack_length + length;
      if (backtrack_length < length){
         backtrack_length = length;
      }
      c->pos = pos;
      ParserContext_backTree(c,left);
      ParserContext_backLog(c,log);
   }
   if (!p_046345934(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TReturn);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e7(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (p_046Block(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
      }
   }
   if (temp){
      const unsigned char * pos4 = c->pos;
      size_t left5 = ParserContext_saveTree(c);
      size_t log6 = ParserContext_saveLog(c);
      if (e80(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos4;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos4;
         ParserContext_backTree(c,left5);
         ParserContext_backLog(c,log6);
      }
   }
   if (temp){
      const unsigned char * pos7 = c->pos;
      size_t left8 = ParserContext_saveTree(c);
      size_t log9 = ParserContext_saveLog(c);
      if (e81(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos7;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos7;
         ParserContext_backTree(c,left8);
         ParserContext_backLog(c,log9);
      }
   }
   if (temp){
      const unsigned char * pos10 = c->pos;
      size_t left11 = ParserContext_saveTree(c);
      size_t log12 = ParserContext_saveLog(c);
      if (e82(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos10;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos10;
         ParserContext_backTree(c,left11);
         ParserContext_backLog(c,log12);
      }
   }
   if (temp){
      const unsigned char * pos13 = c->pos;
      size_t left14 = ParserContext_saveTree(c);
      size_t log15 = ParserContext_saveLog(c);
      if (e83(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos13;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos13;
         ParserContext_backTree(c,left14);
         ParserContext_backLog(c,log15);
      }
   }
   if (temp){
      const unsigned char * pos16 = c->pos;
      size_t left17 = ParserContext_saveTree(c);
      size_t log18 = ParserContext_saveLog(c);
      if (e84(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos16;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos16;
         ParserContext_backTree(c,left17);
         ParserContext_backLog(c,log18);
      }
   }
   if (temp){
      const unsigned char * pos19 = c->pos;
      size_t left20 = ParserContext_saveTree(c);
      size_t log21 = ParserContext_saveLog(c);
      if (e85(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos19;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos19;
         ParserContext_backTree(c,left20);
         ParserContext_backLog(c,log21);
      }
   }
   if (temp){
      const unsigned char * pos22 = c->pos;
      size_t left23 = ParserContext_saveTree(c);
      size_t log24 = ParserContext_saveLog(c);
      if (e86(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos22;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos22;
         ParserContext_backTree(c,left23);
         ParserContext_backLog(c,log24);
      }
   }
   if (temp){
      const unsigned char * pos25 = c->pos;
      size_t left26 = ParserContext_saveTree(c);
      size_t log27 = ParserContext_saveLog(c);
      if (e87(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos25;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos25;
         ParserContext_backTree(c,left26);
         ParserContext_backLog(c,log27);
      }
   }
   if (temp){
      const unsigned char * pos28 = c->pos;
      size_t left29 = ParserContext_saveTree(c);
      size_t log30 = ParserContext_saveLog(c);
      if (e88(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos28;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos28;
         ParserContext_backTree(c,left29);
         ParserContext_backLog(c,log30);
      }
   }
   if (temp){
      const unsigned char * pos31 = c->pos;
      size_t left32 = ParserContext_saveTree(c);
      size_t log33 = ParserContext_saveLog(c);
      if (e89(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos31;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos31;
         ParserContext_backTree(c,left32);
         ParserContext_backLog(c,log33);
      }
   }
   if (temp){
      const unsigned char * pos34 = c->pos;
      size_t left35 = ParserContext_saveTree(c);
      size_t log36 = ParserContext_saveLog(c);
      if (e90(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos34;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos34;
         ParserContext_backTree(c,left35);
         ParserContext_backLog(c,log36);
      }
   }
   if (temp){
      const unsigned char * pos37 = c->pos;
      size_t left38 = ParserContext_saveTree(c);
      size_t log39 = ParserContext_saveLog(c);
      if (e92(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos37;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos37;
         ParserContext_backTree(c,left38);
         ParserContext_backLog(c,log39);
      }
   }
   if (temp){
      const unsigned char * pos40 = c->pos;
      size_t left41 = ParserContext_saveTree(c);
      size_t log42 = ParserContext_saveLog(c);
      if (e94(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos40;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos40;
         ParserContext_backTree(c,left41);
         ParserContext_backLog(c,log42);
      }
   }
   if (temp){
      return 0;
   }
   return 1;
}
static inline int p_046Statement(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,1);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e7(c)){
         ParserContext_memoTreeSucc(c,1,pos);
         return 1;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         ParserContext_memoFail(c,1);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e10(ParserContext * c){
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046Statement(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
int e9(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e10(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
      }
   }
   if (temp){
      const unsigned char * pos4 = c->pos;
      size_t left5 = ParserContext_saveTree(c);
      size_t log6 = ParserContext_saveLog(c);
      if (e11(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos4;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos4;
         ParserContext_backTree(c,left5);
         ParserContext_backLog(c,log6);
      }
   }
   if (temp){
      return 0;
   }
   return 1;
}
static inline int e8(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_0463412334(c)){
      return 0;
   }
while (1){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!e9(c)){
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         break;
      }
   }
   if (!p_0463412534(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TBlock);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
int p_046Block(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,2);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e8(c)){
         ParserContext_memoTreeSucc(c,2,pos);
         return 1;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         ParserContext_memoFail(c,2);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e47(ParserContext * c){
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046Identifier(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lname);
      ParserContext_backTree(c,left);
   }
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int e46(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_04634function34(c)){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   const unsigned char * pos = c->pos;
   size_t left = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e47(c)){
      backtrack_count = backtrack_count + 1;
      long length = c->pos-pos;
      total_backtrack_length = total_backtrack_length + length;
      if (backtrack_length < length){
         backtrack_length = length;
      }
      c->pos = pos;
      ParserContext_backTree(c,left);
      ParserContext_backLog(c,log);
   }
   if (!p_046344034(c)){
      return 0;
   }
   {
      size_t left3 = ParserContext_saveTree(c);
      if (!p_046FunctionParamList(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Llist);
      ParserContext_backTree(c,left3);
   }
   if (!p_046344134(c)){
      return 0;
   }
   {
      size_t left4 = ParserContext_saveTree(c);
      if (!p_046Block(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lbody);
      ParserContext_backTree(c,left4);
   }
   if (!p_04634585834(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TFunctionExpr);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int p_046FunctionExpression(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,23);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e46(c)){
         ParserContext_memoTreeSucc(c,23,pos);
         return 1;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         ParserContext_memoFail(c,23);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e37(ParserContext * c){
   if (ParserContext_read(c) != 48){
      return 0;
   }
   return 1;
}
static inline int e36(ParserContext * c){
   if (!(49 <= ParserContext_prefetch(c) && ParserContext_read(c) < 58)){
      return 0;
   }
while (1){
      const unsigned char * pos = c->pos;
      if (!p_046DIGIT(c)){
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         break;
      }
   }
   return 1;
}
static inline int p_046DECIMAL(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      if (e36(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
      }
   }
   if (temp){
      const unsigned char * pos2 = c->pos;
      if (e37(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos2;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos2;
      }
   }
   if (temp){
      return 0;
   }
   return 1;
}
static inline int e35(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_046DECIMAL(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TInteger);
   ParserContext_endTree(c,0,_T,NULL, 0);
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_046IntegerLiteral(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,18);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e35(c)){
         ParserContext_memoTreeSucc(c,18,pos);
         return 1;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         ParserContext_memoFail(c,18);
         return 0;
      }
   }
   return memo == 1;
}
static inline int p_04634false34(ParserContext * c){
   if (ParserContext_read(c) != 102){
      return 0;
   }
   if (ParserContext_read(c) != 97){
      return 0;
   }
   if (ParserContext_read(c) != 108){
      return 0;
   }
   if (ParserContext_read(c) != 115){
      return 0;
   }
   if (ParserContext_read(c) != 101){
      return 0;
   }
   {
      const unsigned char * pos = c->pos;
      if (p_046W(c)){
         return 0;
      }
      backtrack_count = backtrack_count + 1;
      long length = c->pos-pos;
      total_backtrack_length = total_backtrack_length + length;
      if (backtrack_length < length){
         backtrack_length = length;
      }
      c->pos = pos;
   }
   return 1;
}
static inline int e40(ParserContext * c){
   if (!p_04634false34(c)){
      return 0;
   }
   ParserContext_beginTree(c,0);
   ParserContext_tagTree(c,_TFalse);
   ParserContext_endTree(c,0,_T,NULL, 0);
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_04634true34(ParserContext * c){
   if (ParserContext_read(c) != 116){
      return 0;
   }
   if (ParserContext_read(c) != 114){
      return 0;
   }
   if (ParserContext_read(c) != 117){
      return 0;
   }
   if (ParserContext_read(c) != 101){
      return 0;
   }
   {
      const unsigned char * pos = c->pos;
      if (p_046W(c)){
         return 0;
      }
      backtrack_count = backtrack_count + 1;
      long length = c->pos-pos;
      total_backtrack_length = total_backtrack_length + length;
      if (backtrack_length < length){
         backtrack_length = length;
      }
      c->pos = pos;
   }
   return 1;
}
static inline int e39(ParserContext * c){
   if (!p_04634true34(c)){
      return 0;
   }
   ParserContext_beginTree(c,0);
   ParserContext_tagTree(c,_TTrue);
   ParserContext_endTree(c,0,_T,NULL, 0);
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int e38(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e39(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
      }
   }
   if (temp){
      const unsigned char * pos4 = c->pos;
      size_t left5 = ParserContext_saveTree(c);
      size_t log6 = ParserContext_saveLog(c);
      if (e40(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos4;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos4;
         ParserContext_backTree(c,left5);
         ParserContext_backLog(c,log6);
      }
   }
   if (temp){
      return 0;
   }
   return 1;
}
static inline int p_046BooleanLiteral(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,19);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e38(c)){
         ParserContext_memoTreeSucc(c,19,pos);
         return 1;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         ParserContext_memoFail(c,19);
         return 0;
      }
   }
   return memo == 1;
}
static inline int p_04634null34(ParserContext * c){
   if (ParserContext_read(c) != 110){
      return 0;
   }
   if (ParserContext_read(c) != 117){
      return 0;
   }
   if (ParserContext_read(c) != 108){
      return 0;
   }
   if (ParserContext_read(c) != 108){
      return 0;
   }
   {
      const unsigned char * pos = c->pos;
      if (p_046W(c)){
         return 0;
      }
      backtrack_count = backtrack_count + 1;
      long length = c->pos-pos;
      total_backtrack_length = total_backtrack_length + length;
      if (backtrack_length < length){
         backtrack_length = length;
      }
      c->pos = pos;
   }
   return 1;
}
static inline int e42(ParserContext * c){
   if (!p_04634null34(c)){
      return 0;
   }
   ParserContext_beginTree(c,0);
   ParserContext_tagTree(c,_TNull);
   ParserContext_endTree(c,0,_T,NULL, 0);
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_046NullLiteral(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,21);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e42(c)){
         ParserContext_memoTreeSucc(c,21,pos);
         return 1;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         ParserContext_memoFail(c,21);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e34(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (p_046IntegerLiteral(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
      }
   }
   if (temp){
      const unsigned char * pos4 = c->pos;
      size_t left5 = ParserContext_saveTree(c);
      size_t log6 = ParserContext_saveLog(c);
      if (p_046BooleanLiteral(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos4;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos4;
         ParserContext_backTree(c,left5);
         ParserContext_backLog(c,log6);
      }
   }
   if (temp){
      const unsigned char * pos7 = c->pos;
      size_t left8 = ParserContext_saveTree(c);
      size_t log9 = ParserContext_saveLog(c);
      if (p_046StringLiteral(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos7;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos7;
         ParserContext_backTree(c,left8);
         ParserContext_backLog(c,log9);
      }
   }
   if (temp){
      const unsigned char * pos10 = c->pos;
      size_t left11 = ParserContext_saveTree(c);
      size_t log12 = ParserContext_saveLog(c);
      if (p_046NullLiteral(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos10;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos10;
         ParserContext_backTree(c,left11);
         ParserContext_backLog(c,log12);
      }
   }
   if (temp){
      return 0;
   }
   return 1;
}
static inline int p_046Constant(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,17);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e34(c)){
         ParserContext_memoTreeSucc(c,17,pos);
         return 1;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         ParserContext_memoFail(c,17);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e33(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (p_046Constant(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
      }
   }
   if (temp){
      const unsigned char * pos4 = c->pos;
      size_t left5 = ParserContext_saveTree(c);
      size_t log6 = ParserContext_saveLog(c);
      if (e43(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos4;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos4;
         ParserContext_backTree(c,left5);
         ParserContext_backLog(c,log6);
      }
   }
   if (temp){
      const unsigned char * pos7 = c->pos;
      size_t left8 = ParserContext_saveTree(c);
      size_t log9 = ParserContext_saveLog(c);
      if (p_046FunctionExpression(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos7;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos7;
         ParserContext_backTree(c,left8);
         ParserContext_backLog(c,log9);
      }
   }
   if (temp){
      const unsigned char * pos10 = c->pos;
      size_t left11 = ParserContext_saveTree(c);
      size_t log12 = ParserContext_saveLog(c);
      if (p_046Identifier(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos10;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos10;
         ParserContext_backTree(c,left11);
         ParserContext_backLog(c,log12);
      }
   }
   if (temp){
      return 0;
   }
   return 1;
}
static inline int p_046PrimaryExpression(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,16);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e33(c)){
         ParserContext_memoTreeSucc(c,16,pos);
         return 1;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         ParserContext_memoFail(c,16);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e31(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (p_046FunctionCall(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
      }
   }
   if (temp){
      const unsigned char * pos4 = c->pos;
      size_t left5 = ParserContext_saveTree(c);
      size_t log6 = ParserContext_saveLog(c);
      if (p_046PrimaryExpression(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos4;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos4;
         ParserContext_backTree(c,left5);
         ParserContext_backLog(c,log6);
      }
   }
   if (temp){
      return 0;
   }
   return 1;
}
static inline int p_046PostfixExpression(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,14);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e31(c)){
         ParserContext_memoTreeSucc(c,14,pos);
         return 1;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         ParserContext_memoFail(c,14);
         return 0;
      }
   }
   return memo == 1;
}
int e29(ParserContext * c){
   ParserContext_beginTree(c,0);
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046UnaryExpression(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lleft);
      ParserContext_backTree(c,left);
   }
   if (!p_046_AssignmentOperator(c)){
      return 0;
   }
   {
      size_t left1 = ParserContext_saveTree(c);
      if (!p_046AssignmentExpression(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lright);
      ParserContext_backTree(c,left1);
   }
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e28(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e29(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
      }
   }
   if (temp){
      const unsigned char * pos4 = c->pos;
      size_t left5 = ParserContext_saveTree(c);
      size_t log6 = ParserContext_saveLog(c);
      if (p_046ConditionalExpression(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos4;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos4;
         ParserContext_backTree(c,left5);
         ParserContext_backLog(c,log6);
      }
   }
   if (temp){
      return 0;
   }
   return 1;
}
int p_046AssignmentExpression(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,12);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e28(c)){
         ParserContext_memoTreeSucc(c,12,pos);
         return 1;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         ParserContext_memoFail(c,12);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e45(ParserContext * c){
   ParserContext_foldTree(c,0,_L);
   if (!p_046344434(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046AssignmentExpression(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
   ParserContext_tagTree(c,_TExpression);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e44(ParserContext * c){
   if (!p_046AssignmentExpression(c)){
      return 0;
   }
while (1){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!e45(c)){
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         break;
      }
   }
   return 1;
}
int p_046Expression(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,22);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e44(c)){
         ParserContext_memoTreeSucc(c,22,pos);
         return 1;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         ParserContext_memoFail(c,22);
         return 0;
      }
   }
   return memo == 1;
}
int e43(ParserContext * c){
   if (!p_046344034(c)){
      return 0;
   }
   if (!p_046Expression(c)){
      return 0;
   }
   if (!p_046344134(c)){
      return 0;
   }
   return 1;
}
static inline int e32(ParserContext * c){
   if (!p_046PrimaryExpression(c)){
      return 0;
   }
   if (!e48(c)){
      return 0;
   }
while (1){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!e48(c)){
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         break;
      }
   }
   return 1;
}
int p_046FunctionCall(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,15);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e32(c)){
         ParserContext_memoTreeSucc(c,15,pos);
         return 1;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         ParserContext_memoFail(c,15);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e30(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (p_046PostfixExpression(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
      }
   }
   if (temp){
      const unsigned char * pos4 = c->pos;
      size_t left5 = ParserContext_saveTree(c);
      size_t log6 = ParserContext_saveLog(c);
      if (e50(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos4;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos4;
         ParserContext_backTree(c,left5);
         ParserContext_backLog(c,log6);
      }
   }
   if (temp){
      return 0;
   }
   return 1;
}
int p_046UnaryExpression(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,13);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e30(c)){
         ParserContext_memoTreeSucc(c,13,pos);
         return 1;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         ParserContext_memoFail(c,13);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e67(ParserContext * c){
   if (!p_046UnaryExpression(c)){
      return 0;
   }
while (1){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!e68(c)){
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         break;
      }
   }
   return 1;
}
int p_046RelationalExpression(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,28);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e67(c)){
         ParserContext_memoTreeSucc(c,28,pos);
         return 1;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         ParserContext_memoFail(c,28);
         return 0;
      }
   }
   return memo == 1;
}
int e73(ParserContext * c){
   ParserContext_foldTree(c,0,_Lleft);
   {
      int temp = 1;
      if (temp){
         const unsigned char * pos = c->pos;
         size_t left = ParserContext_saveTree(c);
         size_t log = ParserContext_saveLog(c);
         if (e74(c)){
            temp = 0;
         } else{
            backtrack_count = backtrack_count + 1;
            long length = c->pos-pos;
            total_backtrack_length = total_backtrack_length + length;
            if (backtrack_length < length){
               backtrack_length = length;
            }
            c->pos = pos;
            ParserContext_backTree(c,left);
            ParserContext_backLog(c,log);
         }
      }
      if (temp){
         const unsigned char * pos4 = c->pos;
         size_t left5 = ParserContext_saveTree(c);
         size_t log6 = ParserContext_saveLog(c);
         if (e75(c)){
            temp = 0;
         } else{
            backtrack_count = backtrack_count + 1;
            long length = c->pos-pos4;
            total_backtrack_length = total_backtrack_length + length;
            if (backtrack_length < length){
               backtrack_length = length;
            }
            c->pos = pos4;
            ParserContext_backTree(c,left5);
            ParserContext_backLog(c,log6);
         }
      }
      if (temp){
         return 0;
      }
   }
   {
      size_t left7 = ParserContext_saveTree(c);
      if (!p_046RelationalExpression(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lright);
      ParserContext_backTree(c,left7);
   }
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e6(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (p_046Statement(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
      }
   }
   if (temp){
      const unsigned char * pos4 = c->pos;
      size_t left5 = ParserContext_saveTree(c);
      size_t log6 = ParserContext_saveLog(c);
      if (p_046Declaration(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos4;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos4;
         ParserContext_backTree(c,left5);
         ParserContext_backLog(c,log6);
      }
   }
   if (temp){
      return 0;
   }
   return 1;
}
static inline int p_046TopLevel(ParserContext * c){
   if (!e6(c)){
      return 0;
   }
while (1){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!e6(c)){
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         break;
      }
   }
   return 1;
}
static inline int e5(ParserContext * c){
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046TopLevel(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_046EOT(ParserContext * c){
   if (!ParserContext_eof(c)){
      return 0;
   }
   return 1;
}
static inline int e0(ParserContext * c){
   if (!p_046_(c)){
      return 0;
   }
   ParserContext_beginTree(c,0);
while (1){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!e5(c)){
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         break;
      }
   }
   ParserContext_tagTree(c,_TSource);
   ParserContext_endTree(c,0,_T,NULL, 0);
   if (!p_046EOT(c)){
      return 0;
   }
   return 1;
}
static inline int p_046File(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,0);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e0(c)){
         ParserContext_memoTreeSucc(c,0,pos);
         return 1;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         ParserContext_memoFail(c,0);
         return 0;
      }
   }
   return memo == 1;
}
void cnez_dump(void *v, FILE *fp)
{
	size_t i;
	Tree *t = (Tree*)v;
	if(t == NULL) {
		fputs("null", fp);
		return;
	}
//	if(t->refc != 1) {
//		fprintf(fp, "@%ld", t->refc);
//	}
	fputs("[#", fp);
	fputs(_tags[t->tag], fp);
	if(t->size == 0) {
		fputs(" '", fp);
		for(i = 0; i < t->len; i++) {
			fputc(t->text[i], fp);
		}
		fputs("'", fp);
	}
	else {
		for(i = 0; i < t->size; i++) {
			fputs(" ", fp);
			if(t->labels[i] != 0) {
				fputs("$", fp);
				fputs(_labels[t->labels[i]], fp);
				fputs("=", fp);
			}
			cnez_dump(t->childs[i], fp);
		}
	}
	fputs("]", fp);
}

#ifndef UNUSE_MAIN
#include<sys/time.h> // for using gettimeofday

static const char *get_input(const char *path, size_t *size)
{
	FILE *fp = fopen(path, "rb");
    if(fp != NULL) {		
		size_t len;
		fseek(fp, 0, SEEK_END);
		len = (size_t) ftell(fp);
		fseek(fp, 0, SEEK_SET);
		char *buf = (char *) calloc(1, len + 1);
		size_t readed = fread(buf, 1, len, fp);
		if(readed != len) {
			fprintf(stderr, "read error: %s\n", path);
			exit(1);
		}
		fclose(fp);
		size[0] = len;
		return (const char*)buf;
	}
	size[0] = strlen(path);
	return path;
}

static double timediff(struct timeval *s, struct timeval *e)
{
	double t1 = (e->tv_sec - s->tv_sec) * 1000.0;
	double t2 = (e->tv_usec - s->tv_usec) / 1000.0;
	return t1 + t2; /* ms */
}

int cnez_main(int ac, const char **av, void* (*parse)(const char *input, size_t len))
{
	int j;
	size_t len;
	if(ac == 1) {
		fprintf(stdout, "Usage: %s file [or input-text]\n", av[0]);
		return 1;
	}
	for(j = 1; j < ac; j++) {
		const char *input = get_input(av[j], &len);
		if(getenv("BENCH") != NULL) {
			double tsum = 0.0;
			double t[5];
			int i = 0;
			for(i = 0; i < 5; i++) {
				struct timeval s, e;
				gettimeofday(&s, NULL);
				void *data = parse(input, len);
				gettimeofday(&e, NULL);
				if(data == NULL) {
					fprintf(stdout, "%s FAIL %f[ms]\n", av[j], timediff(&s, &e));
					break;
				}
				t[i] = timediff(&s, &e);
				tsum += t[i];
				cnez_free(data);
			}
			if(tsum != 0.0) {
				fprintf(stdout, "%s OK %.4f[ms] %.3f %.3f %.3f %.3f %.3f\n", av[j], tsum / 5, t[0], t[1], t[2], t[3], t[4]);
			}
		}
		else {
			void *data = parse(input, len);
			cnez_dump(data, stdout);
			fprintf(stdout, "\n");
			if(getenv("MEM") != NULL) {
				cnez_dump_memory("Memory Usage", data);
			}
			cnez_free(data);
		}
	}
	return 0;
}
#endif/*UNUSE_MAIN*/
void* epsilon10_parse(const char *text, size_t len, void *thunk, void* (*fnew)(symbol_t, const unsigned char *, size_t, size_t, void *), void  (*fset)(void *, size_t, symbol_t, void *, void *), void  (*fgc)(void *, int, void *)){
   void* result = NULL;
   ParserContext * c = ParserContext_new((const unsigned char*)text, len);
   ParserContext_initTreeFunc(c,thunk,fnew,fset,fgc);
   ParserContext_initMemo(c,64/*FIXME*/,29);
   if (p_046File(c)){
      printf("total backtrack count: %ld\n", backtrack_count);
      printf("longest backtrack length: %ld\n", backtrack_length);
      printf("total backtrack length: %ld\n", total_backtrack_length);
      result = c->left;
      if (result == NULL){
         result = c->fnew(0, (const unsigned char*)text, (c->pos - (const unsigned char*)text), 0, c->thunk);
      }
   }
   ParserContext_free(c);
   return result;
}
static void* cnez_parse(const char *text, size_t len){
   return epsilon10_parse(text, len, NULL, NULL, NULL, NULL);
}
long epsilon10_match(const char *text, size_t len){
   long result = -1;
   ParserContext * c = ParserContext_new((const unsigned char*)text, len);
   ParserContext_initNoTreeFunc(c);
   ParserContext_initMemo(c,64/*FIXME*/,29);
   if (p_046File(c)){
      result = c->pos-c->inputs;
   }
   ParserContext_free(c);
   return result;
}
const char* epsilon10_tag(symbol_t n){
   return _tags[n];
}
const char* epsilon10_label(symbol_t n){
   return _labels[n];
}
#ifndef UNUSE_MAIN
int main(int ac, const char **argv){
   return cnez_main(ac, argv, cnez_parse);
}
#endif/*MAIN*/
// End of File