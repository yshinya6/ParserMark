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
static const unsigned char _set0[256] = {0,0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static const unsigned char _set1[256] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static int _TName = 1;
static int _TQualifiedName = 2;
static int _TList = 3;
static int _Ltype = 1;
static int _Lanno = 2;
static int _TAnnotated = 4;
static int _Lbase = 3;
static int _Lparam = 4;
static int _TTWildCard = 5;
static int _TUpperBound = 6;
static int _TLowerBound = 7;
static int _TTypeArguments = 8;
static int _TGenericType = 9;
static int _Lprefix = 5;
static int _Lname = 6;
static int _TJavaQualifiedNamedType = 10;
static int _TArrayType = 11;
static int _Lexpr = 7;
static int _TCast = 12;
static const unsigned char _set2[256] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static const unsigned char _set3[256] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static const unsigned char _set4[256] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static const unsigned char _set5[256] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static const unsigned char _set6[256] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static const unsigned char _set7[256] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static int _TFloat = 13;
static const unsigned char _set8[256] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static int _TDouble = 14;
static const unsigned char _set9[256] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static const unsigned char _set10[256] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static const unsigned char _set11[256] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static int _TLong = 15;
static int _TInteger = 16;
static int _TTrue = 17;
static int _TFalse = 18;
static const unsigned char _set12[256] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,1,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static const unsigned char _set13[256] = {0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static int _TCharacter = 19;
static const unsigned char _set14[256] = {0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static int _TString = 20;
static int _TNull = 21;
static int _TThis = 22;
static int _TSuper = 23;
static int _TFinal = 24;
static int _TModifiers = 25;
static int _TArrayName = 26;
static int _TParam = 27;
static int _TVarParam = 28;
static int _Lbody = 8;
static int _TReturn = 29;
static int _TBlock = 30;
static int _Lcond = 9;
static const unsigned char _set15[256] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static int _Lmsg = 10;
static int _TAssert = 31;
static int _Lthen = 11;
static int _Lelse = 12;
static int _TIf = 32;
static int _TSwitchCase = 33;
static int _TSwitchDefault = 34;
static int _TSwitch = 35;
static int _TWhile = 36;
static int _TDoWhile = 37;
static int _Linit = 13;
static int _Liter = 14;
static int _TFor = 38;
static int _TArray = 39;
static int _TVarDecl = 40;
static int _Llist = 15;
static int _TMultiVarDecl = 41;
static int _TForEach = 42;
static int _Llabel = 16;
static const unsigned char _text16[0] = {};
static int _TContinue = 43;
static int _TBreak = 44;
static int _TJavaResourceList = 45;
static int _Ltry = 17;
static int _Lcatch = 18;
static const unsigned char _set17[256] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static int _TJavaMultiExceptions = 46;
static int _TCatch = 47;
static int _Lfinally = 19;
static int _TJavaTryWithResource = 48;
static int _TTry = 49;
static int _TThrow = 50;
static int _TSynchronized = 51;
static int _TLabel = 52;
static int _TExprStatement = 53;
static int _TEmpty = 54;
static int _TPublic = 55;
static int _TProtected = 56;
static int _TPrivate = 57;
static int _TAbstract = 58;
static int _TStatic = 59;
static int _TStrictfp = 60;
static int _L_ = 20;
static int _Lextends = 21;
static const unsigned char _set18[256] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static int _TTypeBound = 61;
static int _TTypeLists = 62;
static int _Lsuper = 22;
static int _Limpl = 23;
static int _TTransient = 63;
static int _TVolatile = 64;
static int _TFieldDecl = 65;
static int _TNative = 66;
static int _TVoidType = 67;
static int _Lthrows = 24;
static int _TThrows = 68;
static int _TMethodDecl = 69;
static int _TConstantDecl = 70;
static int _TDefault = 71;
static int _TInterfaceDecl = 72;
static int _TJavaAnnotationTypeElement = 73;
static int _TJavaAnnotationTypeDecl = 74;
static int _TJavaInstanceInisializer = 75;
static int _TJavaStaticInitializer = 76;
static int _TExplicitConstructorInvocation = 77;
static int _TConstructor = 78;
static int _TClassDecl = 79;
static int _TJavaEnumerator = 80;
static int _TEnumerator = 81;
static int _TJavaEnumMemberList = 82;
static int _TEnumDecl = 83;
static int _TLambda = 84;
static int _Lleft = 25;
static int _TAssign = 85;
static int _TAssignMul = 86;
static int _TAssignDiv = 87;
static int _TAssignMod = 88;
static int _TAssignAdd = 89;
static int _TAssignSub = 90;
static int _TAssignLeftShift = 91;
static int _TAssignRightShift = 92;
static int _TAssignLogicalRightShift = 93;
static int _TAssignBitwiseAnd = 94;
static int _TAssignBitwiseXOr = 95;
static int _TAssignBitwiseOr = 96;
static int _Lright = 26;
static int _TJavaClassLiteral = 97;
static int _TJavaThis = 98;
static int _TJavaSuper = 99;
static int _TApply = 100;
static int _TJavaTypeArgs = 101;
static int _TNew = 102;
static int _Lsize = 27;
static int _TNewArray = 103;
static int _TJavaMethodReference = 104;
static int _Lrecv = 28;
static int _TMethodApply = 105;
static int _TJavaInstanceCreation = 106;
static int _TIndexer = 107;
static int _TField = 108;
static int _TInc = 109;
static int _TDec = 110;
static int _TPreInc = 111;
static int _TPreDec = 112;
static const unsigned char _set19[256] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static int _TPlus = 113;
static const unsigned char _set20[256] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static int _TMinus = 114;
static int _TCompl = 115;
static int _TNot = 116;
static int _TMul = 117;
static int _TDiv = 118;
static int _TMod = 119;
static int _TAdd = 120;
static int _TSub = 121;
static int _TLeftShift = 122;
static int _TRightShift = 123;
static int _TLogicalRightShift = 124;
static int _TLessThanEquals = 125;
static int _TGreaterThanEquals = 126;
static int _TLessThan = 127;
static int _TGreaterThan = 128;
static int _TInstanceOf = 129;
static int _TEquals = 130;
static int _TNotEquals = 131;
static int _TBitwiseAnd = 132;
static int _TBitwiseXor = 133;
static int _TBitwiseOr = 134;
static int _TAnd = 135;
static int _TOr = 136;
static int _TConditional = 137;
static int _TKeyValue = 138;
static int _TCommaList = 139;
static int _TAnnotation = 140;
static int _TJavaPackageDeclaration = 141;
static int _TJavaStaticImport = 142;
static int _TImport = 143;
static int _TWildCardName = 144;
static int _TSource = 145;
static const char * _tags[146] = {"","Name","QualifiedName","List","Annotated","TWildCard","UpperBound","LowerBound","TypeArguments","GenericType","JavaQualifiedNamedType","ArrayType","Cast","Float","Double","Long","Integer","True","False","Character","String","Null","This","Super","Final","Modifiers","ArrayName","Param","VarParam","Return","Block","Assert","If","SwitchCase","SwitchDefault","Switch","While","DoWhile","For","Array","VarDecl","MultiVarDecl","ForEach","Continue","Break","JavaResourceList","JavaMultiExceptions","Catch","JavaTryWithResource","Try","Throw","Synchronized","Label","ExprStatement","Empty","Public","Protected","Private","Abstract","Static","Strictfp","TypeBound","TypeLists","Transient","Volatile","FieldDecl","Native","VoidType","Throws","MethodDecl","ConstantDecl","Default","InterfaceDecl","JavaAnnotationTypeElement","JavaAnnotationTypeDecl","JavaInstanceInisializer","JavaStaticInitializer","ExplicitConstructorInvocation","Constructor","ClassDecl","JavaEnumerator","Enumerator","JavaEnumMemberList","EnumDecl","Lambda","Assign","AssignMul","AssignDiv","AssignMod","AssignAdd","AssignSub","AssignLeftShift","AssignRightShift","AssignLogicalRightShift","AssignBitwiseAnd","AssignBitwiseXOr","AssignBitwiseOr","JavaClassLiteral","JavaThis","JavaSuper","Apply","JavaTypeArgs","New","NewArray","JavaMethodReference","MethodApply","JavaInstanceCreation","Indexer","Field","Inc","Dec","PreInc","PreDec","Plus","Minus","Compl","Not","Mul","Div","Mod","Add","Sub","LeftShift","RightShift","LogicalRightShift","LessThanEquals","GreaterThanEquals","LessThan","GreaterThan","InstanceOf","Equals","NotEquals","BitwiseAnd","BitwiseXor","BitwiseOr","And","Or","Conditional","KeyValue","CommaList","Annotation","JavaPackageDeclaration","JavaStaticImport","Import","WildCardName","Source"};
static const char * _labels[29] = {"","type","anno","base","param","prefix","name","expr","body","cond","msg","then","else","init","iter","list","label","try","catch","finally","_","extends","super","impl","throws","left","right","size","recv"};
static const char * _tables[1] = {""};
// Prototypes
int e124(ParserContext *c);
int e300(ParserContext *c);
int e168(ParserContext *c);
int p_046Annotations(ParserContext *c);
int p_046InterfaceType(ParserContext *c);
int e268(ParserContext *c);
int e364(ParserContext *c);
int e222(ParserContext *c);
int e164(ParserContext *c);
int e220(ParserContext *c);
int p_046CastNewExpression(ParserContext *c);
int p_046ConstantExpression(ParserContext *c);
int p_046ConditionalExpression(ParserContext *c);
int p_046InterfaceDeclaration(ParserContext *c);
int p_046LogicalAndExpression(ParserContext *c);
int p_046Statement(ParserContext *c);
int p_046Annotation(ParserContext *c);
int e333(ParserContext *c);
int e136(ParserContext *c);
int e158(ParserContext *c);
int e334(ParserContext *c);
int p_046VarName(ParserContext *c);
int p_046Expression(ParserContext *c);
int p_046ClassType(ParserContext *c);
int p_046ClassBody(ParserContext *c);
int p_046InclusiveOrExpression(ParserContext *c);
int p_046UnaryExpression(ParserContext *c);
int e9(ParserContext *c);
int p_046ClassOrInterfaceTypes(ParserContext *c);
int p_046ArrayInitializer(ParserContext *c);
int p_046ClassOrInterfaceType(ParserContext *c);
int p_046ElementValue(ParserContext *c);
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
static inline int p_046343834(ParserContext * c){
   if (ParserContext_read(c) != 38){
      return 0;
   }
   if (_set18[ParserContext_prefetch(c)]){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
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
static inline int p_04634case34(ParserContext * c){
   if (ParserContext_read(c) != 99){
      return 0;
   }
   if (ParserContext_read(c) != 97){
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
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_04634default34(ParserContext * c){
   if (ParserContext_read(c) != 100){
      return 0;
   }
   if (ParserContext_read(c) != 101){
      return 0;
   }
   if (ParserContext_read(c) != 102){
      return 0;
   }
   if (ParserContext_read(c) != 97){
      return 0;
   }
   if (ParserContext_read(c) != 117){
      return 0;
   }
   if (ParserContext_read(c) != 108){
      return 0;
   }
   if (ParserContext_read(c) != 116){
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
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_04634protected34(ParserContext * c){
   if (ParserContext_read(c) != 112){
      return 0;
   }
   if (ParserContext_read(c) != 114){
      return 0;
   }
   if (ParserContext_read(c) != 111){
      return 0;
   }
   if (ParserContext_read(c) != 116){
      return 0;
   }
   if (ParserContext_read(c) != 101){
      return 0;
   }
   if (ParserContext_read(c) != 99){
      return 0;
   }
   if (ParserContext_read(c) != 116){
      return 0;
   }
   if (ParserContext_read(c) != 101){
      return 0;
   }
   if (ParserContext_read(c) != 100){
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
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_04634boolean34(ParserContext * c){
   if (ParserContext_read(c) != 98){
      return 0;
   }
   if (ParserContext_read(c) != 111){
      return 0;
   }
   if (ParserContext_read(c) != 111){
      return 0;
   }
   if (ParserContext_read(c) != 108){
      return 0;
   }
   if (ParserContext_read(c) != 101){
      return 0;
   }
   if (ParserContext_read(c) != 97){
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
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_04634native34(ParserContext * c){
   if (ParserContext_read(c) != 110){
      return 0;
   }
   if (ParserContext_read(c) != 97){
      return 0;
   }
   if (ParserContext_read(c) != 116){
      return 0;
   }
   if (ParserContext_read(c) != 105){
      return 0;
   }
   if (ParserContext_read(c) != 118){
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
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_04634enum34(ParserContext * c){
   if (ParserContext_read(c) != 101){
      return 0;
   }
   if (ParserContext_read(c) != 110){
      return 0;
   }
   if (ParserContext_read(c) != 117){
      return 0;
   }
   if (ParserContext_read(c) != 109){
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
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_04634static34(ParserContext * c){
   if (ParserContext_read(c) != 115){
      return 0;
   }
   if (ParserContext_read(c) != 116){
      return 0;
   }
   if (ParserContext_read(c) != 97){
      return 0;
   }
   if (ParserContext_read(c) != 116){
      return 0;
   }
   if (ParserContext_read(c) != 105){
      return 0;
   }
   if (ParserContext_read(c) != 99){
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
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_04634float34(ParserContext * c){
   if (ParserContext_read(c) != 102){
      return 0;
   }
   if (ParserContext_read(c) != 108){
      return 0;
   }
   if (ParserContext_read(c) != 111){
      return 0;
   }
   if (ParserContext_read(c) != 97){
      return 0;
   }
   if (ParserContext_read(c) != 116){
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
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_04634short34(ParserContext * c){
   if (ParserContext_read(c) != 115){
      return 0;
   }
   if (ParserContext_read(c) != 104){
      return 0;
   }
   if (ParserContext_read(c) != 111){
      return 0;
   }
   if (ParserContext_read(c) != 114){
      return 0;
   }
   if (ParserContext_read(c) != 116){
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
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_04634abstract34(ParserContext * c){
   if (ParserContext_read(c) != 97){
      return 0;
   }
   if (ParserContext_read(c) != 98){
      return 0;
   }
   if (ParserContext_read(c) != 115){
      return 0;
   }
   if (ParserContext_read(c) != 116){
      return 0;
   }
   if (ParserContext_read(c) != 114){
      return 0;
   }
   if (ParserContext_read(c) != 97){
      return 0;
   }
   if (ParserContext_read(c) != 99){
      return 0;
   }
   if (ParserContext_read(c) != 116){
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
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_04634try34(ParserContext * c){
   if (ParserContext_read(c) != 116){
      return 0;
   }
   if (ParserContext_read(c) != 114){
      return 0;
   }
   if (ParserContext_read(c) != 121){
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
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_04634break34(ParserContext * c){
   if (ParserContext_read(c) != 98){
      return 0;
   }
   if (ParserContext_read(c) != 114){
      return 0;
   }
   if (ParserContext_read(c) != 101){
      return 0;
   }
   if (ParserContext_read(c) != 97){
      return 0;
   }
   if (ParserContext_read(c) != 107){
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
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_04634strictfp34(ParserContext * c){
   if (ParserContext_read(c) != 115){
      return 0;
   }
   if (ParserContext_read(c) != 116){
      return 0;
   }
   if (ParserContext_read(c) != 114){
      return 0;
   }
   if (ParserContext_read(c) != 105){
      return 0;
   }
   if (ParserContext_read(c) != 99){
      return 0;
   }
   if (ParserContext_read(c) != 116){
      return 0;
   }
   if (ParserContext_read(c) != 102){
      return 0;
   }
   if (ParserContext_read(c) != 112){
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
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_04634super34(ParserContext * c){
   if (ParserContext_read(c) != 115){
      return 0;
   }
   if (ParserContext_read(c) != 117){
      return 0;
   }
   if (ParserContext_read(c) != 112){
      return 0;
   }
   if (ParserContext_read(c) != 101){
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
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_04634catch34(ParserContext * c){
   if (ParserContext_read(c) != 99){
      return 0;
   }
   if (ParserContext_read(c) != 97){
      return 0;
   }
   if (ParserContext_read(c) != 116){
      return 0;
   }
   if (ParserContext_read(c) != 99){
      return 0;
   }
   if (ParserContext_read(c) != 104){
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
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_04634do34(ParserContext * c){
   if (ParserContext_read(c) != 100){
      return 0;
   }
   if (ParserContext_read(c) != 111){
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
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_04634synchronized34(ParserContext * c){
   if (ParserContext_read(c) != 115){
      return 0;
   }
   if (ParserContext_read(c) != 121){
      return 0;
   }
   if (ParserContext_read(c) != 110){
      return 0;
   }
   if (ParserContext_read(c) != 99){
      return 0;
   }
   if (ParserContext_read(c) != 104){
      return 0;
   }
   if (ParserContext_read(c) != 114){
      return 0;
   }
   if (ParserContext_read(c) != 111){
      return 0;
   }
   if (ParserContext_read(c) != 110){
      return 0;
   }
   if (ParserContext_read(c) != 105){
      return 0;
   }
   if (ParserContext_read(c) != 122){
      return 0;
   }
   if (ParserContext_read(c) != 101){
      return 0;
   }
   if (ParserContext_read(c) != 100){
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
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_04634double34(ParserContext * c){
   if (ParserContext_read(c) != 100){
      return 0;
   }
   if (ParserContext_read(c) != 111){
      return 0;
   }
   if (ParserContext_read(c) != 117){
      return 0;
   }
   if (ParserContext_read(c) != 98){
      return 0;
   }
   if (ParserContext_read(c) != 108){
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
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_04634goto34(ParserContext * c){
   if (ParserContext_read(c) != 103){
      return 0;
   }
   if (ParserContext_read(c) != 111){
      return 0;
   }
   if (ParserContext_read(c) != 116){
      return 0;
   }
   if (ParserContext_read(c) != 111){
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
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_04634switch34(ParserContext * c){
   if (ParserContext_read(c) != 115){
      return 0;
   }
   if (ParserContext_read(c) != 119){
      return 0;
   }
   if (ParserContext_read(c) != 105){
      return 0;
   }
   if (ParserContext_read(c) != 116){
      return 0;
   }
   if (ParserContext_read(c) != 99){
      return 0;
   }
   if (ParserContext_read(c) != 104){
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
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_04634finally34(ParserContext * c){
   if (ParserContext_read(c) != 102){
      return 0;
   }
   if (ParserContext_read(c) != 105){
      return 0;
   }
   if (ParserContext_read(c) != 110){
      return 0;
   }
   if (ParserContext_read(c) != 97){
      return 0;
   }
   if (ParserContext_read(c) != 108){
      return 0;
   }
   if (ParserContext_read(c) != 108){
      return 0;
   }
   if (ParserContext_read(c) != 121){
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
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_04634void34(ParserContext * c){
   if (ParserContext_read(c) != 118){
      return 0;
   }
   if (ParserContext_read(c) != 111){
      return 0;
   }
   if (ParserContext_read(c) != 105){
      return 0;
   }
   if (ParserContext_read(c) != 100){
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
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_04634final34(ParserContext * c){
   if (ParserContext_read(c) != 102){
      return 0;
   }
   if (ParserContext_read(c) != 105){
      return 0;
   }
   if (ParserContext_read(c) != 110){
      return 0;
   }
   if (ParserContext_read(c) != 97){
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
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_04634instanceof34(ParserContext * c){
   if (ParserContext_read(c) != 105){
      return 0;
   }
   if (ParserContext_read(c) != 110){
      return 0;
   }
   if (ParserContext_read(c) != 115){
      return 0;
   }
   if (ParserContext_read(c) != 116){
      return 0;
   }
   if (ParserContext_read(c) != 97){
      return 0;
   }
   if (ParserContext_read(c) != 110){
      return 0;
   }
   if (ParserContext_read(c) != 99){
      return 0;
   }
   if (ParserContext_read(c) != 101){
      return 0;
   }
   if (ParserContext_read(c) != 111){
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
   if (!p_046_(c)){
      return 0;
   }
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
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_04634import34(ParserContext * c){
   if (ParserContext_read(c) != 105){
      return 0;
   }
   if (ParserContext_read(c) != 109){
      return 0;
   }
   if (ParserContext_read(c) != 112){
      return 0;
   }
   if (ParserContext_read(c) != 111){
      return 0;
   }
   if (ParserContext_read(c) != 114){
      return 0;
   }
   if (ParserContext_read(c) != 116){
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
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_04634transient34(ParserContext * c){
   if (ParserContext_read(c) != 116){
      return 0;
   }
   if (ParserContext_read(c) != 114){
      return 0;
   }
   if (ParserContext_read(c) != 97){
      return 0;
   }
   if (ParserContext_read(c) != 110){
      return 0;
   }
   if (ParserContext_read(c) != 115){
      return 0;
   }
   if (ParserContext_read(c) != 105){
      return 0;
   }
   if (ParserContext_read(c) != 101){
      return 0;
   }
   if (ParserContext_read(c) != 110){
      return 0;
   }
   if (ParserContext_read(c) != 116){
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
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_04634char34(ParserContext * c){
   if (ParserContext_read(c) != 99){
      return 0;
   }
   if (ParserContext_read(c) != 104){
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
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_04634for34(ParserContext * c){
   if (ParserContext_read(c) != 102){
      return 0;
   }
   if (ParserContext_read(c) != 111){
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
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_04634volatile34(ParserContext * c){
   if (ParserContext_read(c) != 118){
      return 0;
   }
   if (ParserContext_read(c) != 111){
      return 0;
   }
   if (ParserContext_read(c) != 108){
      return 0;
   }
   if (ParserContext_read(c) != 97){
      return 0;
   }
   if (ParserContext_read(c) != 116){
      return 0;
   }
   if (ParserContext_read(c) != 105){
      return 0;
   }
   if (ParserContext_read(c) != 108){
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
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_04634throw34(ParserContext * c){
   if (ParserContext_read(c) != 116){
      return 0;
   }
   if (ParserContext_read(c) != 104){
      return 0;
   }
   if (ParserContext_read(c) != 114){
      return 0;
   }
   if (ParserContext_read(c) != 111){
      return 0;
   }
   if (ParserContext_read(c) != 119){
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
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_04634private34(ParserContext * c){
   if (ParserContext_read(c) != 112){
      return 0;
   }
   if (ParserContext_read(c) != 114){
      return 0;
   }
   if (ParserContext_read(c) != 105){
      return 0;
   }
   if (ParserContext_read(c) != 118){
      return 0;
   }
   if (ParserContext_read(c) != 97){
      return 0;
   }
   if (ParserContext_read(c) != 116){
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
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_04634assert34(ParserContext * c){
   if (ParserContext_read(c) != 97){
      return 0;
   }
   if (ParserContext_read(c) != 115){
      return 0;
   }
   if (ParserContext_read(c) != 115){
      return 0;
   }
   if (ParserContext_read(c) != 101){
      return 0;
   }
   if (ParserContext_read(c) != 114){
      return 0;
   }
   if (ParserContext_read(c) != 116){
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
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_04634long34(ParserContext * c){
   if (ParserContext_read(c) != 108){
      return 0;
   }
   if (ParserContext_read(c) != 111){
      return 0;
   }
   if (ParserContext_read(c) != 110){
      return 0;
   }
   if (ParserContext_read(c) != 103){
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
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_04634throws34(ParserContext * c){
   if (ParserContext_read(c) != 116){
      return 0;
   }
   if (ParserContext_read(c) != 104){
      return 0;
   }
   if (ParserContext_read(c) != 114){
      return 0;
   }
   if (ParserContext_read(c) != 111){
      return 0;
   }
   if (ParserContext_read(c) != 119){
      return 0;
   }
   if (ParserContext_read(c) != 115){
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
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_04634byte34(ParserContext * c){
   if (ParserContext_read(c) != 98){
      return 0;
   }
   if (ParserContext_read(c) != 121){
      return 0;
   }
   if (ParserContext_read(c) != 116){
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
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_04634interface34(ParserContext * c){
   if (ParserContext_read(c) != 105){
      return 0;
   }
   if (ParserContext_read(c) != 110){
      return 0;
   }
   if (ParserContext_read(c) != 116){
      return 0;
   }
   if (ParserContext_read(c) != 101){
      return 0;
   }
   if (ParserContext_read(c) != 114){
      return 0;
   }
   if (ParserContext_read(c) != 102){
      return 0;
   }
   if (ParserContext_read(c) != 97){
      return 0;
   }
   if (ParserContext_read(c) != 99){
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
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_04634int34(ParserContext * c){
   if (ParserContext_read(c) != 105){
      return 0;
   }
   if (ParserContext_read(c) != 110){
      return 0;
   }
   if (ParserContext_read(c) != 116){
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
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_04634public34(ParserContext * c){
   if (ParserContext_read(c) != 112){
      return 0;
   }
   if (ParserContext_read(c) != 117){
      return 0;
   }
   if (ParserContext_read(c) != 98){
      return 0;
   }
   if (ParserContext_read(c) != 108){
      return 0;
   }
   if (ParserContext_read(c) != 105){
      return 0;
   }
   if (ParserContext_read(c) != 99){
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
   if (!p_046_(c)){
      return 0;
   }
   return 1;
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
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_04634implements34(ParserContext * c){
   if (ParserContext_read(c) != 105){
      return 0;
   }
   if (ParserContext_read(c) != 109){
      return 0;
   }
   if (ParserContext_read(c) != 112){
      return 0;
   }
   if (ParserContext_read(c) != 108){
      return 0;
   }
   if (ParserContext_read(c) != 101){
      return 0;
   }
   if (ParserContext_read(c) != 109){
      return 0;
   }
   if (ParserContext_read(c) != 101){
      return 0;
   }
   if (ParserContext_read(c) != 110){
      return 0;
   }
   if (ParserContext_read(c) != 116){
      return 0;
   }
   if (ParserContext_read(c) != 115){
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
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_04634new34(ParserContext * c){
   if (ParserContext_read(c) != 110){
      return 0;
   }
   if (ParserContext_read(c) != 101){
      return 0;
   }
   if (ParserContext_read(c) != 119){
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
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_04634extends34(ParserContext * c){
   if (ParserContext_read(c) != 101){
      return 0;
   }
   if (ParserContext_read(c) != 120){
      return 0;
   }
   if (ParserContext_read(c) != 116){
      return 0;
   }
   if (ParserContext_read(c) != 101){
      return 0;
   }
   if (ParserContext_read(c) != 110){
      return 0;
   }
   if (ParserContext_read(c) != 100){
      return 0;
   }
   if (ParserContext_read(c) != 115){
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
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_04634package34(ParserContext * c){
   if (ParserContext_read(c) != 112){
      return 0;
   }
   if (ParserContext_read(c) != 97){
      return 0;
   }
   if (ParserContext_read(c) != 99){
      return 0;
   }
   if (ParserContext_read(c) != 107){
      return 0;
   }
   if (ParserContext_read(c) != 97){
      return 0;
   }
   if (ParserContext_read(c) != 103){
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
   if (!p_046_(c)){
      return 0;
   }
   return 1;
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
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_04634while34(ParserContext * c){
   if (ParserContext_read(c) != 119){
      return 0;
   }
   if (ParserContext_read(c) != 104){
      return 0;
   }
   if (ParserContext_read(c) != 105){
      return 0;
   }
   if (ParserContext_read(c) != 108){
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
   if (!p_046_(c)){
      return 0;
   }
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
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_04634continue34(ParserContext * c){
   if (ParserContext_read(c) != 99){
      return 0;
   }
   if (ParserContext_read(c) != 111){
      return 0;
   }
   if (ParserContext_read(c) != 110){
      return 0;
   }
   if (ParserContext_read(c) != 116){
      return 0;
   }
   if (ParserContext_read(c) != 105){
      return 0;
   }
   if (ParserContext_read(c) != 110){
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
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_04634this34(ParserContext * c){
   if (ParserContext_read(c) != 116){
      return 0;
   }
   if (ParserContext_read(c) != 104){
      return 0;
   }
   if (ParserContext_read(c) != 105){
      return 0;
   }
   if (ParserContext_read(c) != 115){
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
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_04634class34(ParserContext * c){
   if (ParserContext_read(c) != 99){
      return 0;
   }
   if (ParserContext_read(c) != 108){
      return 0;
   }
   if (ParserContext_read(c) != 97){
      return 0;
   }
   if (ParserContext_read(c) != 115){
      return 0;
   }
   if (ParserContext_read(c) != 115){
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
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_04634const34(ParserContext * c){
   if (ParserContext_read(c) != 99){
      return 0;
   }
   if (ParserContext_read(c) != 111){
      return 0;
   }
   if (ParserContext_read(c) != 110){
      return 0;
   }
   if (ParserContext_read(c) != 115){
      return 0;
   }
   if (ParserContext_read(c) != 116){
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
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_046KEYWORD(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      if (p_04634abstract34(c)){
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
      if (p_04634assert34(c)){
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
      if (p_04634boolean34(c)){
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
      if (p_04634break34(c)){
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
      if (p_04634byte34(c)){
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
      const unsigned char * pos6 = c->pos;
      if (p_04634case34(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos6;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos6;
      }
   }
   if (temp){
      const unsigned char * pos7 = c->pos;
      if (p_04634catch34(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos7;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos7;
      }
   }
   if (temp){
      const unsigned char * pos8 = c->pos;
      if (p_04634char34(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos8;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos8;
      }
   }
   if (temp){
      const unsigned char * pos9 = c->pos;
      if (p_04634class34(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos9;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos9;
      }
   }
   if (temp){
      const unsigned char * pos10 = c->pos;
      if (p_04634const34(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos10;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos10;
      }
   }
   if (temp){
      const unsigned char * pos11 = c->pos;
      if (p_04634continue34(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos11;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos11;
      }
   }
   if (temp){
      const unsigned char * pos12 = c->pos;
      if (p_04634default34(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos12;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos12;
      }
   }
   if (temp){
      const unsigned char * pos13 = c->pos;
      if (p_04634double34(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos13;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos13;
      }
   }
   if (temp){
      const unsigned char * pos14 = c->pos;
      if (p_04634do34(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos14;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos14;
      }
   }
   if (temp){
      const unsigned char * pos15 = c->pos;
      if (p_04634else34(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos15;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos15;
      }
   }
   if (temp){
      const unsigned char * pos16 = c->pos;
      if (p_04634enum34(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos16;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos16;
      }
   }
   if (temp){
      const unsigned char * pos17 = c->pos;
      if (p_04634extends34(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos17;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos17;
      }
   }
   if (temp){
      const unsigned char * pos18 = c->pos;
      if (p_04634false34(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos18;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos18;
      }
   }
   if (temp){
      const unsigned char * pos19 = c->pos;
      if (p_04634final34(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos19;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos19;
      }
   }
   if (temp){
      const unsigned char * pos20 = c->pos;
      if (p_04634finally34(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos20;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos20;
      }
   }
   if (temp){
      const unsigned char * pos21 = c->pos;
      if (p_04634float34(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos21;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos21;
      }
   }
   if (temp){
      const unsigned char * pos22 = c->pos;
      if (p_04634for34(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos22;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos22;
      }
   }
   if (temp){
      const unsigned char * pos23 = c->pos;
      if (p_04634goto34(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos23;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos23;
      }
   }
   if (temp){
      const unsigned char * pos24 = c->pos;
      if (p_04634if34(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos24;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos24;
      }
   }
   if (temp){
      const unsigned char * pos25 = c->pos;
      if (p_04634implements34(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos25;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos25;
      }
   }
   if (temp){
      const unsigned char * pos26 = c->pos;
      if (p_04634import34(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos26;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos26;
      }
   }
   if (temp){
      const unsigned char * pos27 = c->pos;
      if (p_04634instanceof34(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos27;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos27;
      }
   }
   if (temp){
      const unsigned char * pos28 = c->pos;
      if (p_04634interface34(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos28;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos28;
      }
   }
   if (temp){
      const unsigned char * pos29 = c->pos;
      if (p_04634int34(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos29;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos29;
      }
   }
   if (temp){
      const unsigned char * pos30 = c->pos;
      if (p_04634long34(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos30;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos30;
      }
   }
   if (temp){
      const unsigned char * pos31 = c->pos;
      if (p_04634native34(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos31;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos31;
      }
   }
   if (temp){
      const unsigned char * pos32 = c->pos;
      if (p_04634new34(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos32;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos32;
      }
   }
   if (temp){
      const unsigned char * pos33 = c->pos;
      if (p_04634null34(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos33;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos33;
      }
   }
   if (temp){
      const unsigned char * pos34 = c->pos;
      if (p_04634package34(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos34;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos34;
      }
   }
   if (temp){
      const unsigned char * pos35 = c->pos;
      if (p_04634private34(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos35;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos35;
      }
   }
   if (temp){
      const unsigned char * pos36 = c->pos;
      if (p_04634protected34(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos36;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos36;
      }
   }
   if (temp){
      const unsigned char * pos37 = c->pos;
      if (p_04634public34(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos37;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos37;
      }
   }
   if (temp){
      const unsigned char * pos38 = c->pos;
      if (p_04634return34(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos38;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos38;
      }
   }
   if (temp){
      const unsigned char * pos39 = c->pos;
      if (p_04634short34(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos39;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos39;
      }
   }
   if (temp){
      const unsigned char * pos40 = c->pos;
      if (p_04634static34(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos40;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos40;
      }
   }
   if (temp){
      const unsigned char * pos41 = c->pos;
      if (p_04634strictfp34(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos41;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos41;
      }
   }
   if (temp){
      const unsigned char * pos42 = c->pos;
      if (p_04634super34(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos42;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos42;
      }
   }
   if (temp){
      const unsigned char * pos43 = c->pos;
      if (p_04634switch34(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos43;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos43;
      }
   }
   if (temp){
      const unsigned char * pos44 = c->pos;
      if (p_04634synchronized34(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos44;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos44;
      }
   }
   if (temp){
      const unsigned char * pos45 = c->pos;
      if (p_04634this34(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos45;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos45;
      }
   }
   if (temp){
      const unsigned char * pos46 = c->pos;
      if (p_04634throws34(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos46;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos46;
      }
   }
   if (temp){
      const unsigned char * pos47 = c->pos;
      if (p_04634throw34(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos47;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos47;
      }
   }
   if (temp){
      const unsigned char * pos48 = c->pos;
      if (p_04634transient34(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos48;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos48;
      }
   }
   if (temp){
      const unsigned char * pos49 = c->pos;
      if (p_04634true34(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos49;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos49;
      }
   }
   if (temp){
      const unsigned char * pos50 = c->pos;
      if (p_04634try34(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos50;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos50;
      }
   }
   if (temp){
      const unsigned char * pos51 = c->pos;
      if (p_04634void34(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos51;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos51;
      }
   }
   if (temp){
      const unsigned char * pos52 = c->pos;
      if (p_04634volatile34(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos52;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos52;
      }
   }
   if (temp){
      const unsigned char * pos53 = c->pos;
      if (p_04634while34(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos53;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos53;
      }
   }
   if (temp){
      return 0;
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
static inline int e13(ParserContext * c){
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
      if (e13(c)){
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
static inline int p344634(ParserContext * c){
   if (ParserContext_read(c) != 46){
      return 0;
   }
   if (ParserContext_prefetch(c) == 46){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int e14(ParserContext * c){
   ParserContext_foldTree(c,0,_L);
   if (!p344634(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046Identifier(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
   ParserContext_tagTree(c,_TQualifiedName);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e12(ParserContext * c){
   if (!p_046Identifier(c)){
      return 0;
   }
while (1){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!e14(c)){
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
static inline int p_046QualifiedName(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,4);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e12(c)){
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
static inline int e363(ParserContext * c){
   ParserContext_foldTree(c,0,_Lleft);
   if (!p_04634383834(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046InclusiveOrExpression(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lright);
      ParserContext_backTree(c,left);
   }
   ParserContext_tagTree(c,_TAnd);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e22(ParserContext * c){
   if (!p_046LogicalAndExpression(c)){
      return 0;
   }
while (1){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!e364(c)){
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
   int memo = ParserContext_memoLookupTree(c,11);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e22(c)){
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
static inline int e67(ParserContext * c){
   ParserContext_beginTree(c,0);
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046Annotations(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lanno);
      ParserContext_backTree(c,left);
   }
   {
      size_t left1 = ParserContext_saveTree(c);
      if (!p_046Identifier(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left1);
   }
   ParserContext_tagTree(c,_TAnnotated);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e66(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e67(c)){
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
      if (p_046Identifier(c)){
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
static inline int p_046TypeVariable(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,38);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e66(c)){
         ParserContext_memoTreeSucc(c,38,pos);
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
         ParserContext_memoFail(c,38);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e42(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_04634byte34(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TName);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e44(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_04634short34(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TName);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e43(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_04634char34(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TName);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e46(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_04634long34(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TName);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e45(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_04634int34(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TName);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e41(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e42(c)){
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
      if (e44(c)){
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
      if (e45(c)){
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
      if (e46(c)){
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
      return 0;
   }
   return 1;
}
static inline int p_046IntegralType(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,28);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e41(c)){
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
static inline int e48(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_04634float34(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TName);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e49(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_04634double34(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TName);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e47(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e48(c)){
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
      if (e49(c)){
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
static inline int p_046FloatingPointType(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,29);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e47(c)){
         ParserContext_memoTreeSucc(c,29,pos);
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
         ParserContext_memoFail(c,29);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e40(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (p_046IntegralType(c)){
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
      if (p_046FloatingPointType(c)){
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
static inline int p_046NumericType(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,27);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e40(c)){
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
static inline int e50(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_04634boolean34(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TName);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int p_046BooleanType(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,30);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e50(c)){
         ParserContext_memoTreeSucc(c,30,pos);
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
         ParserContext_memoFail(c,30);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e39(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (p_046NumericType(c)){
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
      if (p_046BooleanType(c)){
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
static inline int p_046UnannoPrimitiveType(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,26);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e39(c)){
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
static inline int e38(ParserContext * c){
   ParserContext_beginTree(c,0);
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046Annotations(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lanno);
      ParserContext_backTree(c,left);
   }
   {
      size_t left1 = ParserContext_saveTree(c);
      if (!p_046UnannoPrimitiveType(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left1);
   }
   ParserContext_tagTree(c,_TAnnotated);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e37(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e38(c)){
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
      if (p_046UnannoPrimitiveType(c)){
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
static inline int p_046PrimitiveType(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,25);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e37(c)){
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
static inline int e7(ParserContext * c){
   size_t left = ParserContext_saveTree(c);
   if (!p_046Annotations(c)){
      return 0;
   }
   ParserContext_linkTree(c,_L);
   ParserContext_backTree(c,left);
   return 1;
}
static inline int p_046349134(ParserContext * c){
   if (ParserContext_read(c) != 91){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_046349334(ParserContext * c){
   if (ParserContext_read(c) != 93){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int e68(ParserContext * c){
   ParserContext_foldTree(c,0,_Lbase);
   const unsigned char * pos = c->pos;
   size_t left = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e7(c)){
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
   if (!p_046349134(c)){
      return 0;
   }
   if (!p_046349334(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TArrayType);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e36(ParserContext * c){
   {
      int temp = 1;
      if (temp){
         const unsigned char * pos = c->pos;
         size_t left = ParserContext_saveTree(c);
         size_t log = ParserContext_saveLog(c);
         if (p_046PrimitiveType(c)){
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
         if (p_046ClassOrInterfaceType(c)){
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
         if (p_046TypeVariable(c)){
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
         return 0;
      }
   }
   if (!e68(c)){
      return 0;
   }
while (1){
      const unsigned char * pos10 = c->pos;
      size_t left11 = ParserContext_saveTree(c);
      size_t log12 = ParserContext_saveLog(c);
      if (!e68(c)){
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos10;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos10;
         ParserContext_backTree(c,left11);
         ParserContext_backLog(c,log12);
         break;
      }
   }
   return 1;
}
static inline int p_046ArrayType(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,24);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e36(c)){
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
static inline int e35(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (p_046ArrayType(c)){
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
      if (p_046ClassOrInterfaceType(c)){
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
      if (p_046TypeVariable(c)){
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
      return 0;
   }
   return 1;
}
static inline int p_046ReferenceType(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,23);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e35(c)){
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
static inline int e34(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (p_046ReferenceType(c)){
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
      if (p_046PrimitiveType(c)){
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
static inline int p_046Type(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,22);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e34(c)){
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
static inline int p3446464634(ParserContext * c){
   if (ParserContext_read(c) != 46){
      return 0;
   }
   if (ParserContext_read(c) != 46){
      return 0;
   }
   if (ParserContext_read(c) != 46){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int e122(ParserContext * c){
   if (!p_04634final34(c)){
      return 0;
   }
   ParserContext_beginTree(c,0);
   ParserContext_tagTree(c,_TFinal);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e121(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (p_046Annotation(c)){
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
      if (e122(c)){
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
static inline int p_046VariableModifier(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,58);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e121(c)){
         ParserContext_memoTreeSucc(c,58,pos);
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
         ParserContext_memoFail(c,58);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e120(ParserContext * c){
   size_t left = ParserContext_saveTree(c);
   if (!p_046VariableModifier(c)){
      return 0;
   }
   ParserContext_linkTree(c,_L);
   ParserContext_backTree(c,left);
   return 1;
}
static inline int e119(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!e120(c)){
      return 0;
   }
while (1){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!e120(c)){
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
   ParserContext_tagTree(c,_TModifiers);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int p_046VariableModifiers(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,57);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e119(c)){
         ParserContext_memoTreeSucc(c,57,pos);
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
         ParserContext_memoFail(c,57);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e118(ParserContext * c){
   size_t left = ParserContext_saveTree(c);
   if (!p_046VariableModifiers(c)){
      return 0;
   }
   ParserContext_linkTree(c,_Lanno);
   ParserContext_backTree(c,left);
   return 1;
}
static inline int e126(ParserContext * c){
   ParserContext_beginTree(c,0);
   const unsigned char * pos = c->pos;
   size_t left = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e118(c)){
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
   {
      size_t left3 = ParserContext_saveTree(c);
      if (!p_046Type(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left3);
   }
   if (!p3446464634(c)){
      return 0;
   }
   {
      size_t left4 = ParserContext_saveTree(c);
      if (!p_046VarName(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left4);
   }
   ParserContext_tagTree(c,_TVarParam);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int p_046VarParam(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,61);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e126(c)){
         ParserContext_memoTreeSucc(c,61,pos);
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
         ParserContext_memoFail(c,61);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e117(ParserContext * c){
   ParserContext_beginTree(c,0);
   const unsigned char * pos = c->pos;
   size_t left = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e118(c)){
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
   {
      size_t left3 = ParserContext_saveTree(c);
      if (!p_046Type(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Ltype);
      ParserContext_backTree(c,left3);
   }
   {
      size_t left4 = ParserContext_saveTree(c);
      if (!p_046VarName(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lname);
      ParserContext_backTree(c,left4);
   }
   ParserContext_tagTree(c,_TParam);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int p_046MethodParam(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,56);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e117(c)){
         ParserContext_memoTreeSucc(c,56,pos);
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
         ParserContext_memoFail(c,56);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e116(ParserContext * c){
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046MethodParam(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
   if (!p_046344434(c)){
      return 0;
   }
   return 1;
}
static inline int e115(ParserContext * c){
   ParserContext_beginTree(c,0);
while (1){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!e116(c)){
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
   {
      size_t left3 = ParserContext_saveTree(c);
      if (!p_046VarParam(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left3);
   }
   ParserContext_tagTree(c,_TList);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e129(ParserContext * c){
   if (!p_046344434(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046MethodParam(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
   return 1;
}
static inline int e128(ParserContext * c){
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046MethodParam(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
while (1){
      const unsigned char * pos = c->pos;
      size_t left2 = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!e129(c)){
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
static inline int e127(ParserContext * c){
   ParserContext_beginTree(c,0);
   const unsigned char * pos = c->pos;
   size_t left = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e128(c)){
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
static inline int e114(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e115(c)){
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
      if (e127(c)){
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
static inline int p_046MethodParamList(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,55);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e114(c)){
         ParserContext_memoTreeSucc(c,55,pos);
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
         ParserContext_memoFail(c,55);
         return 0;
      }
   }
   return memo == 1;
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
static inline int e113(ParserContext * c){
   if (!p_046344034(c)){
      return 0;
   }
   if (!p_046MethodParamList(c)){
      return 0;
   }
   if (!p_046344134(c)){
      return 0;
   }
   return 1;
}
static inline int e132(ParserContext * c){
   if (!p_046344434(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046Identifier(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
   return 1;
}
static inline int e131(ParserContext * c){
   ParserContext_beginTree(c,0);
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046Identifier(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
while (1){
      const unsigned char * pos = c->pos;
      size_t left2 = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!e132(c)){
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
   ParserContext_tagTree(c,_TList);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int p_046InferredParamList(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,62);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e131(c)){
         ParserContext_memoTreeSucc(c,62,pos);
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
         ParserContext_memoFail(c,62);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e130(ParserContext * c){
   if (!p_046344034(c)){
      return 0;
   }
   if (!p_046InferredParamList(c)){
      return 0;
   }
   if (!p_046344134(c)){
      return 0;
   }
   return 1;
}
static inline int e112(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (p_046Identifier(c)){
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
      if (e113(c)){
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
      if (e130(c)){
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
      return 0;
   }
   return 1;
}
static inline int p_046LambdaParameters(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,54);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e112(c)){
         ParserContext_memoTreeSucc(c,54,pos);
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
         ParserContext_memoFail(c,54);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e201(ParserContext * c){
   if (!p_04634public34(c)){
      return 0;
   }
   ParserContext_beginTree(c,0);
   ParserContext_tagTree(c,_TPublic);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e202(ParserContext * c){
   if (!p_04634protected34(c)){
      return 0;
   }
   ParserContext_beginTree(c,0);
   ParserContext_tagTree(c,_TProtected);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e205(ParserContext * c){
   if (!p_04634static34(c)){
      return 0;
   }
   ParserContext_beginTree(c,0);
   ParserContext_tagTree(c,_TStatic);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e206(ParserContext * c){
   if (!p_04634strictfp34(c)){
      return 0;
   }
   ParserContext_beginTree(c,0);
   ParserContext_tagTree(c,_TStrictfp);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e203(ParserContext * c){
   if (!p_04634private34(c)){
      return 0;
   }
   ParserContext_beginTree(c,0);
   ParserContext_tagTree(c,_TPrivate);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e204(ParserContext * c){
   if (!p_04634abstract34(c)){
      return 0;
   }
   ParserContext_beginTree(c,0);
   ParserContext_tagTree(c,_TAbstract);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e200(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (p_046Annotation(c)){
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
      if (e201(c)){
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
      if (e202(c)){
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
      if (e203(c)){
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
      if (e204(c)){
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
      if (e122(c)){
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
      if (e205(c)){
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
      if (e206(c)){
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
      return 0;
   }
   return 1;
}
static inline int p_046ClassModifier(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,83);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e200(c)){
         ParserContext_memoTreeSucc(c,83,pos);
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
         ParserContext_memoFail(c,83);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e199(ParserContext * c){
   size_t left = ParserContext_saveTree(c);
   if (!p_046ClassModifier(c)){
      return 0;
   }
   ParserContext_linkTree(c,_L);
   ParserContext_backTree(c,left);
   return 1;
}
static inline int e198(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!e199(c)){
      return 0;
   }
while (1){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!e199(c)){
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
   ParserContext_tagTree(c,_TModifiers);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int p_046ClassModifiers(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,82);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e198(c)){
         ParserContext_memoTreeSucc(c,82,pos);
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
         ParserContext_memoFail(c,82);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e197(ParserContext * c){
   size_t left = ParserContext_saveTree(c);
   if (!p_046ClassModifiers(c)){
      return 0;
   }
   ParserContext_linkTree(c,_Lanno);
   ParserContext_backTree(c,left);
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
static inline int e240(ParserContext * c){
   size_t left = ParserContext_saveTree(c);
   if (!p_046Annotations(c)){
      return 0;
   }
   ParserContext_linkTree(c,_Lanno);
   ParserContext_backTree(c,left);
   return 1;
}
static inline int e301(ParserContext * c){
   ParserContext_beginTree(c,0);
   const unsigned char * pos = c->pos;
   size_t left = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e240(c)){
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
   {
      size_t left3 = ParserContext_saveTree(c);
      if (!p_046Identifier(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lname);
      ParserContext_backTree(c,left3);
   }
   {
      size_t left4 = ParserContext_saveTree(c);
      if (!p_046ClassBody(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left4);
   }
   ParserContext_tagTree(c,_TJavaEnumerator);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e155(ParserContext * c){
   if (!p_046344434(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046Expression(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
   return 1;
}
static inline int p_046addArgumentExpressionList(ParserContext * c){
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046Expression(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
while (1){
      const unsigned char * pos = c->pos;
      size_t left2 = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!e155(c)){
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
static inline int e299(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_046344034(c)){
      return 0;
   }
   const unsigned char * pos = c->pos;
   size_t left = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!p_046addArgumentExpressionList(c)){
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
   if (!p_046344134(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TList);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int p_046ArgumentExpressionList(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,127);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e299(c)){
         ParserContext_memoTreeSucc(c,127,pos);
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
         ParserContext_memoFail(c,127);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e298(ParserContext * c){
   ParserContext_beginTree(c,0);
   const unsigned char * pos = c->pos;
   size_t left = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e240(c)){
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
   {
      size_t left3 = ParserContext_saveTree(c);
      if (!p_046Identifier(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lname);
      ParserContext_backTree(c,left3);
   }
   {
      size_t left4 = ParserContext_saveTree(c);
      if (!p_046ArgumentExpressionList(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left4);
   }
   const unsigned char * pos5 = c->pos;
   size_t left6 = ParserContext_saveTree(c);
   size_t log7 = ParserContext_saveLog(c);
   if (!e300(c)){
      backtrack_count = backtrack_count + 1;
      long length = c->pos-pos5;
      total_backtrack_length = total_backtrack_length + length;
      if (backtrack_length < length){
         backtrack_length = length;
      }
      c->pos = pos5;
      ParserContext_backTree(c,left6);
      ParserContext_backLog(c,log7);
   }
   ParserContext_tagTree(c,_TJavaEnumerator);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e302(ParserContext * c){
   ParserContext_beginTree(c,0);
   const unsigned char * pos = c->pos;
   size_t left = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e240(c)){
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
   {
      size_t left3 = ParserContext_saveTree(c);
      if (!p_046Identifier(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lname);
      ParserContext_backTree(c,left3);
   }
   ParserContext_tagTree(c,_TEnumerator);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e297(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e298(c)){
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
      if (e301(c)){
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
      if (e302(c)){
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
      return 0;
   }
   return 1;
}
static inline int p_046Enumerator(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,126);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e297(c)){
         ParserContext_memoTreeSucc(c,126,pos);
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
         ParserContext_memoFail(c,126);
         return 0;
      }
   }
   return memo == 1;
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
static inline int p_0463412334(ParserContext * c){
   if (ParserContext_read(c) != 123){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int e303(ParserContext * c){
   if (!p_046344434(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046Enumerator(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
   return 1;
}
static inline int p_046addEnumeratorList(ParserContext * c){
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046Enumerator(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
while (1){
      const unsigned char * pos = c->pos;
      size_t left2 = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!e303(c)){
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
static inline int e296(ParserContext * c){
   if (!p_0463412334(c)){
      return 0;
   }
   ParserContext_beginTree(c,0);
   const unsigned char * pos = c->pos;
   size_t left = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!p_046addEnumeratorList(c)){
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
   const unsigned char * pos3 = c->pos;
   if (!p_046344434(c)){
      backtrack_count = backtrack_count + 1;
      long length = c->pos-pos3;
      total_backtrack_length = total_backtrack_length + length;
      if (backtrack_length < length){
         backtrack_length = length;
      }
      c->pos = pos3;
   }
   if (!p_046345934(c)){
      return 0;
   }
   {
      size_t left4 = ParserContext_saveTree(c);
      ParserContext_beginTree(c,0);
      if (!e222(c)){
         return 0;
      }
while (1){
         const unsigned char * pos5 = c->pos;
         size_t left6 = ParserContext_saveTree(c);
         size_t log7 = ParserContext_saveLog(c);
         if (!e222(c)){
            backtrack_count = backtrack_count + 1;
            long length = c->pos-pos5;
            total_backtrack_length = total_backtrack_length + length;
            if (backtrack_length < length){
               backtrack_length = length;
            }
            c->pos = pos5;
            ParserContext_backTree(c,left6);
            ParserContext_backLog(c,log7);
            break;
         }
      }
      ParserContext_tagTree(c,_TJavaEnumMemberList);
      ParserContext_endTree(c,0,_T,NULL, 0);
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left4);
   }
   ParserContext_tagTree(c,_TList);
   ParserContext_endTree(c,0,_T,NULL, 0);
   if (!p_0463412534(c)){
      return 0;
   }
   return 1;
}
static inline int e304(ParserContext * c){
   if (!p_0463412334(c)){
      return 0;
   }
   ParserContext_beginTree(c,0);
   const unsigned char * pos = c->pos;
   size_t left = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!p_046addEnumeratorList(c)){
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
   const unsigned char * pos3 = c->pos;
   if (!p_046344434(c)){
      backtrack_count = backtrack_count + 1;
      long length = c->pos-pos3;
      total_backtrack_length = total_backtrack_length + length;
      if (backtrack_length < length){
         backtrack_length = length;
      }
      c->pos = pos3;
   }
   const unsigned char * pos4 = c->pos;
   if (!p_046345934(c)){
      backtrack_count = backtrack_count + 1;
      long length = c->pos-pos4;
      total_backtrack_length = total_backtrack_length + length;
      if (backtrack_length < length){
         backtrack_length = length;
      }
      c->pos = pos4;
   }
   ParserContext_tagTree(c,_TList);
   ParserContext_endTree(c,0,_T,NULL, 0);
   if (!p_0463412534(c)){
      return 0;
   }
   return 1;
}
static inline int e295(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e296(c)){
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
      if (e304(c)){
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
static inline int p_046EnumBody(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,125);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e295(c)){
         ParserContext_memoTreeSucc(c,125,pos);
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
         ParserContext_memoFail(c,125);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e218(ParserContext * c){
   if (!p_04634implements34(c)){
      return 0;
   }
   if (!p_046ClassOrInterfaceTypes(c)){
      return 0;
   }
   return 1;
}
static inline int p_046SuperInterfaces(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,88);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e218(c)){
         ParserContext_memoTreeSucc(c,88,pos);
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
         ParserContext_memoFail(c,88);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e217(ParserContext * c){
   size_t left = ParserContext_saveTree(c);
   if (!p_046SuperInterfaces(c)){
      return 0;
   }
   ParserContext_linkTree(c,_Limpl);
   ParserContext_backTree(c,left);
   return 1;
}
static inline int e294(ParserContext * c){
   ParserContext_beginTree(c,0);
   const unsigned char * pos = c->pos;
   size_t left = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e197(c)){
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
   if (!p_04634enum34(c)){
      return 0;
   }
   {
      size_t left3 = ParserContext_saveTree(c);
      if (!p_046Identifier(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lname);
      ParserContext_backTree(c,left3);
   }
   const unsigned char * pos4 = c->pos;
   size_t left5 = ParserContext_saveTree(c);
   size_t log6 = ParserContext_saveLog(c);
   if (!e217(c)){
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
   {
      size_t left7 = ParserContext_saveTree(c);
      if (!p_046EnumBody(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lbody);
      ParserContext_backTree(c,left7);
   }
   ParserContext_tagTree(c,_TEnumDecl);
   ParserContext_endTree(c,0,_T,NULL, 0);
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
static inline int e213(ParserContext * c){
   if (!p_046343834(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046InterfaceType(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
   return 1;
}
static inline int e212(ParserContext * c){
   ParserContext_foldTree(c,0,_Lbase);
   if (!p_04634extends34(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046ClassOrInterfaceType(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lextends);
      ParserContext_backTree(c,left);
   }
while (1){
      const unsigned char * pos = c->pos;
      size_t left2 = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!e213(c)){
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
   ParserContext_tagTree(c,_TTypeBound);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e211(ParserContext * c){
   if (!p_046Identifier(c)){
      return 0;
   }
   const unsigned char * pos = c->pos;
   size_t left = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e212(c)){
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
   return 1;
}
static inline int p_046UnannoTypeParameter(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,86);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e211(c)){
         ParserContext_memoTreeSucc(c,86,pos);
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
         ParserContext_memoFail(c,86);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e210(ParserContext * c){
   ParserContext_beginTree(c,0);
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046Annotations(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lanno);
      ParserContext_backTree(c,left);
   }
   {
      size_t left1 = ParserContext_saveTree(c);
      if (!p_046UnannoTypeParameter(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left1);
   }
   ParserContext_tagTree(c,_TAnnotated);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e209(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e210(c)){
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
      if (p_046UnannoTypeParameter(c)){
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
static inline int p_046TypeParameter(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,85);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e209(c)){
         ParserContext_memoTreeSucc(c,85,pos);
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
         ParserContext_memoFail(c,85);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e214(ParserContext * c){
   if (!p_046344434(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046TypeParameter(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
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
static inline int e208(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_046346034(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046TypeParameter(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
while (1){
      const unsigned char * pos = c->pos;
      size_t left2 = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!e214(c)){
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
   if (!p_046346234(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TTypeLists);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int p_046TypeParameters(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,84);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e208(c)){
         ParserContext_memoTreeSucc(c,84,pos);
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
         ParserContext_memoFail(c,84);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e207(ParserContext * c){
   size_t left = ParserContext_saveTree(c);
   if (!p_046TypeParameters(c)){
      return 0;
   }
   ParserContext_linkTree(c,_L_);
   ParserContext_backTree(c,left);
   return 1;
}
static inline int e216(ParserContext * c){
   if (!p_04634extends34(c)){
      return 0;
   }
   if (!p_046ClassOrInterfaceType(c)){
      return 0;
   }
   return 1;
}
static inline int p_046SuperClass(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,87);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e216(c)){
         ParserContext_memoTreeSucc(c,87,pos);
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
         ParserContext_memoFail(c,87);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e215(ParserContext * c){
   size_t left = ParserContext_saveTree(c);
   if (!p_046SuperClass(c)){
      return 0;
   }
   ParserContext_linkTree(c,_Lsuper);
   ParserContext_backTree(c,left);
   return 1;
}
static inline int e196(ParserContext * c){
   ParserContext_beginTree(c,0);
   const unsigned char * pos = c->pos;
   size_t left = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e197(c)){
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
   if (!p_04634class34(c)){
      return 0;
   }
   {
      size_t left3 = ParserContext_saveTree(c);
      if (!p_046Identifier(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lname);
      ParserContext_backTree(c,left3);
   }
   const unsigned char * pos4 = c->pos;
   size_t left5 = ParserContext_saveTree(c);
   size_t log6 = ParserContext_saveLog(c);
   if (!e207(c)){
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
   const unsigned char * pos7 = c->pos;
   size_t left8 = ParserContext_saveTree(c);
   size_t log9 = ParserContext_saveLog(c);
   if (!e215(c)){
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
   const unsigned char * pos10 = c->pos;
   size_t left11 = ParserContext_saveTree(c);
   size_t log12 = ParserContext_saveLog(c);
   if (!e217(c)){
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
   {
      size_t left13 = ParserContext_saveTree(c);
      if (!p_046ClassBody(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lbody);
      ParserContext_backTree(c,left13);
   }
   ParserContext_tagTree(c,_TClassDecl);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e195(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e196(c)){
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
      if (e294(c)){
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
static inline int p_046ClassDeclaration(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,81);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e195(c)){
         ParserContext_memoTreeSucc(c,81,pos);
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
         ParserContext_memoFail(c,81);
         return 0;
      }
   }
   return memo == 1;
}
static inline int p_046InitDecl(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,76);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e168(c)){
         ParserContext_memoTreeSucc(c,76,pos);
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
         ParserContext_memoFail(c,76);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e169(ParserContext * c){
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
static inline int e162(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (p_046Expression(c)){
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
      if (p_046ArrayInitializer(c)){
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
static inline int p_046Initializer(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,73);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e162(c)){
         ParserContext_memoTreeSucc(c,73,pos);
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
         ParserContext_memoFail(c,73);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e165(ParserContext * c){
   if (!p_046344434(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046Initializer(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
   return 1;
}
int e164(ParserContext * c){
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046Initializer(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
while (1){
      const unsigned char * pos = c->pos;
      size_t left2 = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!e165(c)){
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
static inline int e163(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_0463412334(c)){
      return 0;
   }
   const unsigned char * pos = c->pos;
   size_t left = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e164(c)){
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
   const unsigned char * pos3 = c->pos;
   if (!p_046344434(c)){
      backtrack_count = backtrack_count + 1;
      long length = c->pos-pos3;
      total_backtrack_length = total_backtrack_length + length;
      if (backtrack_length < length){
         backtrack_length = length;
      }
      c->pos = pos3;
   }
   if (!p_0463412534(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TArray);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
int p_046ArrayInitializer(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,74);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e163(c)){
         ParserContext_memoTreeSucc(c,74,pos);
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
         ParserContext_memoFail(c,74);
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
static inline int e161(ParserContext * c){
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
static inline int e167(ParserContext * c){
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
      if (!e169(c)){
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
   ParserContext_tagTree(c,_TList);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int p_046InitDeclList(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,75);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e167(c)){
         ParserContext_memoTreeSucc(c,75,pos);
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
         ParserContext_memoFail(c,75);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e166(ParserContext * c){
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046InitDeclList(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Llist);
      ParserContext_backTree(c,left);
   }
   ParserContext_tagTree(c,_TMultiVarDecl);
   return 1;
}
static inline int e160(ParserContext * c){
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046VarName(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lname);
      ParserContext_backTree(c,left);
   }
   const unsigned char * pos = c->pos;
   size_t left2 = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e161(c)){
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
   if (ParserContext_prefetch(c) == 44){
      return 0;
   }
   ParserContext_tagTree(c,_TVarDecl);
   return 1;
}
static inline int p_046addVariableDeclarations(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e160(c)){
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
      if (e166(c)){
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
static inline int e159(ParserContext * c){
   ParserContext_beginTree(c,0);
   const unsigned char * pos = c->pos;
   size_t left = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e118(c)){
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
   {
      size_t left3 = ParserContext_saveTree(c);
      if (!p_046Type(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Ltype);
      ParserContext_backTree(c,left3);
   }
   if (!p_046addVariableDeclarations(c)){
      return 0;
   }
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int p_046VariableDeclaration(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,72);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e159(c)){
         ParserContext_memoTreeSucc(c,72,pos);
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
         ParserContext_memoFail(c,72);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e194(ParserContext * c){
   if (!p_046VariableDeclaration(c)){
      return 0;
   }
   if (!p_046345934(c)){
      return 0;
   }
   return 1;
}
static inline int e154(ParserContext * c){
   ParserContext_beginTree(c,0);
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046Expression(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
while (1){
      const unsigned char * pos = c->pos;
      size_t left2 = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!e155(c)){
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
   ParserContext_tagTree(c,_TList);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int p_046Expressions(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,71);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e154(c)){
         ParserContext_memoTreeSucc(c,71,pos);
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
         ParserContext_memoFail(c,71);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e157(ParserContext * c){
   size_t left = ParserContext_saveTree(c);
   if (!p_046Expressions(c)){
      return 0;
   }
   ParserContext_linkTree(c,_Liter);
   ParserContext_backTree(c,left);
   return 1;
}
static inline int e156(ParserContext * c){
   size_t left = ParserContext_saveTree(c);
   if (!p_046Expression(c)){
      return 0;
   }
   ParserContext_linkTree(c,_Lcond);
   ParserContext_backTree(c,left);
   return 1;
}
static inline int e135(ParserContext * c){
   if (!p_0463412334(c)){
      return 0;
   }
   ParserContext_beginTree(c,0);
while (1){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!e136(c)){
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
   ParserContext_tagTree(c,_TBlock);
   ParserContext_endTree(c,0,_T,NULL, 0);
   if (!p_0463412534(c)){
      return 0;
   }
   return 1;
}
static inline int p_046Block(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,64);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e135(c)){
         ParserContext_memoTreeSucc(c,64,pos);
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
         ParserContext_memoFail(c,64);
         return 0;
      }
   }
   return memo == 1;
}
static inline int p_0463412434(ParserContext * c){
   if (ParserContext_read(c) != 124){
      return 0;
   }
   if (_set17[ParserContext_prefetch(c)]){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int e185(ParserContext * c){
   if (!p_0463412434(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046ClassOrInterfaceType(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
   return 1;
}
static inline int e184(ParserContext * c){
   const unsigned char * pos = c->pos;
   size_t left = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e118(c)){
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
   {
      size_t left3 = ParserContext_saveTree(c);
      ParserContext_beginTree(c,0);
      {
         size_t left4 = ParserContext_saveTree(c);
         if (!p_046ClassOrInterfaceType(c)){
            return 0;
         }
         ParserContext_linkTree(c,_L);
         ParserContext_backTree(c,left4);
      }
      if (!e185(c)){
         return 0;
      }
while (1){
         const unsigned char * pos5 = c->pos;
         size_t left6 = ParserContext_saveTree(c);
         size_t log7 = ParserContext_saveLog(c);
         if (!e185(c)){
            backtrack_count = backtrack_count + 1;
            long length = c->pos-pos5;
            total_backtrack_length = total_backtrack_length + length;
            if (backtrack_length < length){
               backtrack_length = length;
            }
            c->pos = pos5;
            ParserContext_backTree(c,left6);
            ParserContext_backLog(c,log7);
            break;
         }
      }
      ParserContext_tagTree(c,_TJavaMultiExceptions);
      ParserContext_endTree(c,0,_T,NULL, 0);
      ParserContext_linkTree(c,_Ltype);
      ParserContext_backTree(c,left3);
   }
   {
      size_t left8 = ParserContext_saveTree(c);
      if (!p_046Identifier(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lname);
      ParserContext_backTree(c,left8);
   }
   return 1;
}
static inline int e183(ParserContext * c){
   const unsigned char * pos = c->pos;
   size_t left = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e118(c)){
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
   {
      size_t left3 = ParserContext_saveTree(c);
      if (!p_046ClassOrInterfaceType(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Ltype);
      ParserContext_backTree(c,left3);
   }
   {
      size_t left4 = ParserContext_saveTree(c);
      if (!p_046Identifier(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lname);
      ParserContext_backTree(c,left4);
   }
   return 1;
}
static inline int p_046addCatchParameter(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e183(c)){
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
      if (e184(c)){
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
static inline int e182(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_04634catch34(c)){
      return 0;
   }
   if (!p_046344034(c)){
      return 0;
   }
   if (!p_046addCatchParameter(c)){
      return 0;
   }
   if (!p_046344134(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046Block(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lbody);
      ParserContext_backTree(c,left);
   }
   ParserContext_tagTree(c,_TCatch);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int p_046Catch(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,80);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e182(c)){
         ParserContext_memoTreeSucc(c,80,pos);
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
         ParserContext_memoFail(c,80);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e181(ParserContext * c){
   size_t left = ParserContext_saveTree(c);
   if (!p_046Catch(c)){
      return 0;
   }
   ParserContext_linkTree(c,_L);
   ParserContext_backTree(c,left);
   return 1;
}
static inline int e188(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_04634try34(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046Block(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Ltry);
      ParserContext_backTree(c,left);
   }
   {
      size_t left1 = ParserContext_saveTree(c);
      ParserContext_beginTree(c,0);
      if (!e181(c)){
         return 0;
      }
while (1){
         const unsigned char * pos = c->pos;
         size_t left3 = ParserContext_saveTree(c);
         size_t log = ParserContext_saveLog(c);
         if (!e181(c)){
            backtrack_count = backtrack_count + 1;
            long length = c->pos-pos;
            total_backtrack_length = total_backtrack_length + length;
            if (backtrack_length < length){
               backtrack_length = length;
            }
            c->pos = pos;
            ParserContext_backTree(c,left3);
            ParserContext_backLog(c,log);
            break;
         }
      }
      ParserContext_tagTree(c,_TList);
      ParserContext_endTree(c,0,_T,NULL, 0);
      ParserContext_linkTree(c,_Lcatch);
      ParserContext_backTree(c,left1);
   }
   ParserContext_tagTree(c,_TTry);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e189(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_04634throw34(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046Expression(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lexpr);
      ParserContext_backTree(c,left);
   }
   ParserContext_tagTree(c,_TThrow);
   ParserContext_endTree(c,0,_T,NULL, 0);
   if (!p_046345934(c)){
      return 0;
   }
   return 1;
}
static inline int e187(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_04634try34(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046Block(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Ltry);
      ParserContext_backTree(c,left);
   }
   {
      size_t left1 = ParserContext_saveTree(c);
      ParserContext_beginTree(c,0);
while (1){
         const unsigned char * pos = c->pos;
         size_t left3 = ParserContext_saveTree(c);
         size_t log = ParserContext_saveLog(c);
         if (!e181(c)){
            backtrack_count = backtrack_count + 1;
            long length = c->pos-pos;
            total_backtrack_length = total_backtrack_length + length;
            if (backtrack_length < length){
               backtrack_length = length;
            }
            c->pos = pos;
            ParserContext_backTree(c,left3);
            ParserContext_backLog(c,log);
            break;
         }
      }
      ParserContext_tagTree(c,_TList);
      ParserContext_endTree(c,0,_T,NULL, 0);
      ParserContext_linkTree(c,_Lcatch);
      ParserContext_backTree(c,left1);
   }
   if (!p_04634finally34(c)){
      return 0;
   }
   {
      size_t left5 = ParserContext_saveTree(c);
      if (!p_046Block(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lfinally);
      ParserContext_backTree(c,left5);
   }
   ParserContext_tagTree(c,_TTry);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e142(ParserContext * c){
   if (!p_04634else34(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046Statement(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lelse);
      ParserContext_backTree(c,left);
   }
   return 1;
}
static inline int e141(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_04634if34(c)){
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
      if (!p_046Statement(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lthen);
      ParserContext_backTree(c,left1);
   }
   const unsigned char * pos = c->pos;
   size_t left3 = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e142(c)){
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
static inline int e179(ParserContext * c){
   ParserContext_beginTree(c,0);
   const unsigned char * pos = c->pos;
   size_t left = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e118(c)){
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
   {
      size_t left3 = ParserContext_saveTree(c);
      if (!p_046Type(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Ltype);
      ParserContext_backTree(c,left3);
   }
   {
      size_t left4 = ParserContext_saveTree(c);
      if (!p_046Identifier(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lname);
      ParserContext_backTree(c,left4);
   }
   if (!p_046346134(c)){
      return 0;
   }
   {
      size_t left5 = ParserContext_saveTree(c);
      if (!p_046Expression(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lexpr);
      ParserContext_backTree(c,left5);
   }
   ParserContext_tagTree(c,_TVarDecl);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int p_046Resource(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,79);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e179(c)){
         ParserContext_memoTreeSucc(c,79,pos);
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
         ParserContext_memoFail(c,79);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e186(ParserContext * c){
   if (!p_04634finally34(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046Block(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lfinally);
      ParserContext_backTree(c,left);
   }
   return 1;
}
static inline int e180(ParserContext * c){
   if (!p_046345934(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046Resource(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
   return 1;
}
static inline int e178(ParserContext * c){
   if (!p_046344034(c)){
      return 0;
   }
   ParserContext_beginTree(c,0);
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046Resource(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
while (1){
      const unsigned char * pos = c->pos;
      size_t left2 = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!e180(c)){
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
   ParserContext_tagTree(c,_TJavaResourceList);
   ParserContext_endTree(c,0,_T,NULL, 0);
   const unsigned char * pos4 = c->pos;
   if (!p_046345934(c)){
      backtrack_count = backtrack_count + 1;
      long length = c->pos-pos4;
      total_backtrack_length = total_backtrack_length + length;
      if (backtrack_length < length){
         backtrack_length = length;
      }
      c->pos = pos4;
   }
   if (!p_046344134(c)){
      return 0;
   }
   return 1;
}
static inline int p_046ResourceList(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,78);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e178(c)){
         ParserContext_memoTreeSucc(c,78,pos);
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
         ParserContext_memoFail(c,78);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e177(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_04634try34(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046ResourceList(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
   {
      size_t left1 = ParserContext_saveTree(c);
      if (!p_046Block(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Ltry);
      ParserContext_backTree(c,left1);
   }
   {
      size_t left2 = ParserContext_saveTree(c);
      ParserContext_beginTree(c,0);
while (1){
         const unsigned char * pos = c->pos;
         size_t left4 = ParserContext_saveTree(c);
         size_t log = ParserContext_saveLog(c);
         if (!e181(c)){
            backtrack_count = backtrack_count + 1;
            long length = c->pos-pos;
            total_backtrack_length = total_backtrack_length + length;
            if (backtrack_length < length){
               backtrack_length = length;
            }
            c->pos = pos;
            ParserContext_backTree(c,left4);
            ParserContext_backLog(c,log);
            break;
         }
      }
      ParserContext_tagTree(c,_TList);
      ParserContext_endTree(c,0,_T,NULL, 0);
      ParserContext_linkTree(c,_Lcatch);
      ParserContext_backTree(c,left2);
   }
   const unsigned char * pos6 = c->pos;
   size_t left7 = ParserContext_saveTree(c);
   size_t log8 = ParserContext_saveLog(c);
   if (!e186(c)){
      backtrack_count = backtrack_count + 1;
      long length = c->pos-pos6;
      total_backtrack_length = total_backtrack_length + length;
      if (backtrack_length < length){
         backtrack_length = length;
      }
      c->pos = pos6;
      ParserContext_backTree(c,left7);
      ParserContext_backLog(c,log8);
   }
   ParserContext_tagTree(c,_TJavaTryWithResource);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e151(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_04634do34(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046Statement(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lbody);
      ParserContext_backTree(c,left);
   }
   if (!p_04634while34(c)){
      return 0;
   }
   if (!p_046344034(c)){
      return 0;
   }
   {
      size_t left1 = ParserContext_saveTree(c);
      if (!p_046Expression(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lcond);
      ParserContext_backTree(c,left1);
   }
   if (!p_046344134(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TDoWhile);
   ParserContext_endTree(c,0,_T,NULL, 0);
   if (!p_046345934(c)){
      return 0;
   }
   return 1;
}
static inline int e172(ParserContext * c){
   size_t left = ParserContext_saveTree(c);
   if (!p_046Identifier(c)){
      return 0;
   }
   ParserContext_linkTree(c,_Llabel);
   ParserContext_backTree(c,left);
   return 1;
}
static inline int e173(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_04634break34(c)){
      return 0;
   }
   const unsigned char * pos = c->pos;
   size_t left = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e172(c)){
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
   ParserContext_valueTree(c,_text16, 0);
   ParserContext_tagTree(c,_TBreak);
   ParserContext_endTree(c,0,_T,NULL, 0);
   if (!p_046345934(c)){
      return 0;
   }
   return 1;
}
static inline int e153(ParserContext * c){
   size_t left = ParserContext_saveTree(c);
   if (!p_046Expressions(c)){
      return 0;
   }
   ParserContext_linkTree(c,_Linit);
   ParserContext_backTree(c,left);
   return 1;
}
static inline int e152(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_04634for34(c)){
      return 0;
   }
   if (!p_046344034(c)){
      return 0;
   }
   const unsigned char * pos = c->pos;
   size_t left = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e153(c)){
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
   const unsigned char * pos3 = c->pos;
   size_t left4 = ParserContext_saveTree(c);
   size_t log5 = ParserContext_saveLog(c);
   if (!e156(c)){
      backtrack_count = backtrack_count + 1;
      long length = c->pos-pos3;
      total_backtrack_length = total_backtrack_length + length;
      if (backtrack_length < length){
         backtrack_length = length;
      }
      c->pos = pos3;
      ParserContext_backTree(c,left4);
      ParserContext_backLog(c,log5);
   }
   if (!p_046345934(c)){
      return 0;
   }
   const unsigned char * pos6 = c->pos;
   size_t left7 = ParserContext_saveTree(c);
   size_t log8 = ParserContext_saveLog(c);
   if (!e157(c)){
      backtrack_count = backtrack_count + 1;
      long length = c->pos-pos6;
      total_backtrack_length = total_backtrack_length + length;
      if (backtrack_length < length){
         backtrack_length = length;
      }
      c->pos = pos6;
      ParserContext_backTree(c,left7);
      ParserContext_backLog(c,log8);
   }
   if (!p_046344134(c)){
      return 0;
   }
   {
      size_t left9 = ParserContext_saveTree(c);
      if (!p_046Statement(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lbody);
      ParserContext_backTree(c,left9);
   }
   ParserContext_tagTree(c,_TFor);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e175(ParserContext * c){
   size_t left = ParserContext_saveTree(c);
   if (!p_046Expression(c)){
      return 0;
   }
   ParserContext_linkTree(c,_Lexpr);
   ParserContext_backTree(c,left);
   return 1;
}
static inline int e174(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_04634return34(c)){
      return 0;
   }
   const unsigned char * pos = c->pos;
   size_t left = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e175(c)){
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
   ParserContext_tagTree(c,_TReturn);
   ParserContext_endTree(c,0,_T,NULL, 0);
   if (!p_046345934(c)){
      return 0;
   }
   return 1;
}
static inline int e171(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_04634continue34(c)){
      return 0;
   }
   const unsigned char * pos = c->pos;
   size_t left = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e172(c)){
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
   ParserContext_valueTree(c,_text16, 0);
   ParserContext_tagTree(c,_TContinue);
   ParserContext_endTree(c,0,_T,NULL, 0);
   if (!p_046345934(c)){
      return 0;
   }
   return 1;
}
static inline int e193(ParserContext * c){
   if (!p_046345934(c)){
      return 0;
   }
   ParserContext_beginTree(c,0);
   ParserContext_tagTree(c,_TEmpty);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e148(ParserContext * c){
   ParserContext_beginTree(c,0);
while (1){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!e136(c)){
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
   ParserContext_tagTree(c,_TBlock);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int p_046CaseBlock(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,70);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e148(c)){
         ParserContext_memoTreeSucc(c,70,pos);
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
         ParserContext_memoFail(c,70);
         return 0;
      }
   }
   return memo == 1;
}
static inline int p_046345834(ParserContext * c){
   if (ParserContext_read(c) != 58){
      return 0;
   }
   if (_set15[ParserContext_prefetch(c)]){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int e147(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_04634case34(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046ConstantExpression(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lcond);
      ParserContext_backTree(c,left);
   }
   if (!p_046345834(c)){
      return 0;
   }
   {
      size_t left1 = ParserContext_saveTree(c);
      if (!p_046CaseBlock(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lbody);
      ParserContext_backTree(c,left1);
   }
   ParserContext_tagTree(c,_TSwitchCase);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e149(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_04634default34(c)){
      return 0;
   }
   if (!p_046345834(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046CaseBlock(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lbody);
      ParserContext_backTree(c,left);
   }
   ParserContext_tagTree(c,_TSwitchDefault);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e146(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e147(c)){
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
      if (e149(c)){
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
static inline int p_046SwitchCondition(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,69);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e146(c)){
         ParserContext_memoTreeSucc(c,69,pos);
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
         ParserContext_memoFail(c,69);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e145(ParserContext * c){
   size_t left = ParserContext_saveTree(c);
   if (!p_046SwitchCondition(c)){
      return 0;
   }
   ParserContext_linkTree(c,_L);
   ParserContext_backTree(c,left);
   return 1;
}
static inline int e144(ParserContext * c){
   if (!p_0463412334(c)){
      return 0;
   }
   ParserContext_beginTree(c,0);
   if (!e145(c)){
      return 0;
   }
while (1){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!e145(c)){
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
   ParserContext_tagTree(c,_TBlock);
   ParserContext_endTree(c,0,_T,NULL, 0);
   if (!p_0463412534(c)){
      return 0;
   }
   return 1;
}
static inline int p_046SwitchBlock(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,68);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e144(c)){
         ParserContext_memoTreeSucc(c,68,pos);
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
         ParserContext_memoFail(c,68);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e143(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_04634switch34(c)){
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
      if (!p_046SwitchBlock(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lbody);
      ParserContext_backTree(c,left1);
   }
   ParserContext_tagTree(c,_TSwitch);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int p_046SwitchStatement(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,67);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e143(c)){
         ParserContext_memoTreeSucc(c,67,pos);
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
         ParserContext_memoFail(c,67);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e150(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_04634while34(c)){
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
      if (!p_046Statement(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lbody);
      ParserContext_backTree(c,left1);
   }
   ParserContext_tagTree(c,_TWhile);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e191(ParserContext * c){
   ParserContext_beginTree(c,0);
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046Identifier(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Llabel);
      ParserContext_backTree(c,left);
   }
   if (!p_046345834(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TLabel);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e170(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_04634for34(c)){
      return 0;
   }
   if (!p_046344034(c)){
      return 0;
   }
   const unsigned char * pos = c->pos;
   size_t left = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e118(c)){
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
   {
      size_t left3 = ParserContext_saveTree(c);
      if (!p_046Type(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Ltype);
      ParserContext_backTree(c,left3);
   }
   {
      size_t left4 = ParserContext_saveTree(c);
      if (!p_046Identifier(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lname);
      ParserContext_backTree(c,left4);
   }
   if (!p_046345834(c)){
      return 0;
   }
   {
      size_t left5 = ParserContext_saveTree(c);
      if (!p_046Expression(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Liter);
      ParserContext_backTree(c,left5);
   }
   if (!p_046344134(c)){
      return 0;
   }
   {
      size_t left6 = ParserContext_saveTree(c);
      if (!p_046Statement(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lbody);
      ParserContext_backTree(c,left6);
   }
   ParserContext_tagTree(c,_TForEach);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e192(ParserContext * c){
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
   ParserContext_tagTree(c,_TExprStatement);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e190(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_04634synchronized34(c)){
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
      ParserContext_linkTree(c,_Lbody);
      ParserContext_backTree(c,left1);
   }
   ParserContext_tagTree(c,_TSynchronized);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e140(ParserContext * c){
   if (!p_046345834(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046Expression(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lmsg);
      ParserContext_backTree(c,left);
   }
   return 1;
}
static inline int e139(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_04634assert34(c)){
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
   const unsigned char * pos = c->pos;
   size_t left2 = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e140(c)){
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
   ParserContext_tagTree(c,_TAssert);
   ParserContext_endTree(c,0,_T,NULL, 0);
   if (!p_046345934(c)){
      return 0;
   }
   return 1;
}
static inline int e176(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e177(c)){
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
      if (e187(c)){
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
      if (e188(c)){
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
      return 0;
   }
   return 1;
}
static inline int p_046TryStatement(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,77);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e176(c)){
         ParserContext_memoTreeSucc(c,77,pos);
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
         ParserContext_memoFail(c,77);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e138(ParserContext * c){
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
      if (e139(c)){
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
      if (e141(c)){
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
      if (p_046SwitchStatement(c)){
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
      if (e150(c)){
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
      if (e151(c)){
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
      if (e152(c)){
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
      if (e158(c)){
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
      if (e170(c)){
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
      if (e171(c)){
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
      if (e173(c)){
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
      if (e174(c)){
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
      if (p_046TryStatement(c)){
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
      if (e189(c)){
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
      const unsigned char * pos43 = c->pos;
      size_t left44 = ParserContext_saveTree(c);
      size_t log45 = ParserContext_saveLog(c);
      if (e190(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos43;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos43;
         ParserContext_backTree(c,left44);
         ParserContext_backLog(c,log45);
      }
   }
   if (temp){
      const unsigned char * pos46 = c->pos;
      size_t left47 = ParserContext_saveTree(c);
      size_t log48 = ParserContext_saveLog(c);
      if (e191(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos46;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos46;
         ParserContext_backTree(c,left47);
         ParserContext_backLog(c,log48);
      }
   }
   if (temp){
      const unsigned char * pos49 = c->pos;
      size_t left50 = ParserContext_saveTree(c);
      size_t log51 = ParserContext_saveLog(c);
      if (e192(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos49;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos49;
         ParserContext_backTree(c,left50);
         ParserContext_backLog(c,log51);
      }
   }
   if (temp){
      const unsigned char * pos52 = c->pos;
      size_t left53 = ParserContext_saveTree(c);
      size_t log54 = ParserContext_saveLog(c);
      if (e193(c)){
         temp = 0;
      } else{
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos52;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos52;
         ParserContext_backTree(c,left53);
         ParserContext_backLog(c,log54);
      }
   }
   if (temp){
      return 0;
   }
   return 1;
}
int p_046Statement(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,66);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e138(c)){
         ParserContext_memoTreeSucc(c,66,pos);
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
         ParserContext_memoFail(c,66);
         return 0;
      }
   }
   return memo == 1;
}
int e158(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_04634for34(c)){
      return 0;
   }
   if (!p_046344034(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046VariableDeclaration(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Linit);
      ParserContext_backTree(c,left);
   }
   if (!p_046345934(c)){
      return 0;
   }
   const unsigned char * pos = c->pos;
   size_t left2 = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e156(c)){
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
   if (!p_046345934(c)){
      return 0;
   }
   const unsigned char * pos4 = c->pos;
   size_t left5 = ParserContext_saveTree(c);
   size_t log6 = ParserContext_saveLog(c);
   if (!e157(c)){
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
   if (!p_046344134(c)){
      return 0;
   }
   {
      size_t left7 = ParserContext_saveTree(c);
      if (!p_046Statement(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lbody);
      ParserContext_backTree(c,left7);
   }
   ParserContext_tagTree(c,_TFor);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e137(ParserContext * c){
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
      if (e194(c)){
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
      if (p_046ClassDeclaration(c)){
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
      return 0;
   }
   return 1;
}
static inline int p_046BlockStatement(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,65);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e137(c)){
         ParserContext_memoTreeSucc(c,65,pos);
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
         ParserContext_memoFail(c,65);
         return 0;
      }
   }
   return memo == 1;
}
int e136(ParserContext * c){
   size_t left = ParserContext_saveTree(c);
   if (!p_046BlockStatement(c)){
      return 0;
   }
   ParserContext_linkTree(c,_L);
   ParserContext_backTree(c,left);
   return 1;
}
static inline int e134(ParserContext * c){
   if (ParserContext_prefetch(c) == 123){
      return 0;
   }
   ParserContext_beginTree(c,0);
   {
      size_t left = ParserContext_saveTree(c);
      ParserContext_beginTree(c,0);
      {
         size_t left1 = ParserContext_saveTree(c);
         if (!p_046Expression(c)){
            return 0;
         }
         ParserContext_linkTree(c,_Lexpr);
         ParserContext_backTree(c,left1);
      }
      ParserContext_tagTree(c,_TReturn);
      ParserContext_endTree(c,0,_T,NULL, 0);
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
   ParserContext_tagTree(c,_TBlock);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e133(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e134(c)){
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
      if (p_046Block(c)){
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
static inline int p_046LambdaBody(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,63);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e133(c)){
         ParserContext_memoTreeSucc(c,63,pos);
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
         ParserContext_memoFail(c,63);
         return 0;
      }
   }
   return memo == 1;
}
static inline int p_04634456234(ParserContext * c){
   if (ParserContext_read(c) != 45){
      return 0;
   }
   if (ParserContext_read(c) != 62){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int e111(ParserContext * c){
   ParserContext_beginTree(c,0);
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046LambdaParameters(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lparam);
      ParserContext_backTree(c,left);
   }
   if (!p_04634456234(c)){
      return 0;
   }
   {
      size_t left1 = ParserContext_saveTree(c);
      if (!p_046LambdaBody(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lbody);
      ParserContext_backTree(c,left1);
   }
   ParserContext_tagTree(c,_TLambda);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int p_046LambdaExpression(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,53);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e111(c)){
         ParserContext_memoTreeSucc(c,53,pos);
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
         ParserContext_memoFail(c,53);
         return 0;
      }
   }
   return memo == 1;
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
static inline int e311(ParserContext * c){
   if (!p_04634436134(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TAssignAdd);
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
static inline int e312(ParserContext * c){
   if (!p_04634456134(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TAssignSub);
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
static inline int e310(ParserContext * c){
   if (!p_04634376134(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TAssignMod);
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
static inline int e308(ParserContext * c){
   if (!p_04634426134(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TAssignMul);
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
static inline int e309(ParserContext * c){
   if (!p_04634476134(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TAssignDiv);
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
static inline int e317(ParserContext * c){
   if (!p_04634946134(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TAssignBitwiseXOr);
   return 1;
}
static inline int e307(ParserContext * c){
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
static inline int e318(ParserContext * c){
   if (!p_046341246134(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TAssignBitwiseOr);
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
static inline int e315(ParserContext * c){
   if (!p_046346262626134(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TAssignLogicalRightShift);
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
static inline int e316(ParserContext * c){
   if (!p_04634386134(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TAssignBitwiseAnd);
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
static inline int e313(ParserContext * c){
   if (!p_0463460606134(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TAssignLeftShift);
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
static inline int e314(ParserContext * c){
   if (!p_0463462626134(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TAssignRightShift);
   return 1;
}
static inline int p_046addAssignmentOperator(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e307(c)){
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
      if (e308(c)){
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
      if (e309(c)){
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
      if (e310(c)){
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
      if (e311(c)){
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
      if (e312(c)){
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
      if (e313(c)){
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
      if (e314(c)){
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
      if (e315(c)){
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
      if (e316(c)){
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
      if (e317(c)){
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
      if (e318(c)){
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
static inline int e306(ParserContext * c){
   ParserContext_beginTree(c,0);
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046UnaryExpression(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lleft);
      ParserContext_backTree(c,left);
   }
   if (!p_046addAssignmentOperator(c)){
      return 0;
   }
   {
      size_t left1 = ParserContext_saveTree(c);
      if (!p_046Expression(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lright);
      ParserContext_backTree(c,left1);
   }
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e305(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e306(c)){
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
static inline int p_046AssignmentExpression(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,128);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e305(c)){
         ParserContext_memoTreeSucc(c,128,pos);
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
         ParserContext_memoFail(c,128);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e110(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (p_046LambdaExpression(c)){
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
      if (p_046AssignmentExpression(c)){
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
int p_046Expression(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,52);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e110(c)){
         ParserContext_memoTreeSucc(c,52,pos);
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
         ParserContext_memoFail(c,52);
         return 0;
      }
   }
   return memo == 1;
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
static inline int e365(ParserContext * c){
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
static inline int e20(ParserContext * c){
   if (!p_046344434(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046ElementValue(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
   return 1;
}
static inline int p_046addElementValueList(ParserContext * c){
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046ElementValue(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
while (1){
      const unsigned char * pos = c->pos;
      size_t left2 = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!e20(c)){
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
static inline int e19(ParserContext * c){
   if (!p_0463412334(c)){
      return 0;
   }
   ParserContext_beginTree(c,0);
   const unsigned char * pos = c->pos;
   size_t left = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!p_046addElementValueList(c)){
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
   const unsigned char * pos3 = c->pos;
   if (!p_046344434(c)){
      backtrack_count = backtrack_count + 1;
      long length = c->pos-pos3;
      total_backtrack_length = total_backtrack_length + length;
      if (backtrack_length < length){
         backtrack_length = length;
      }
      c->pos = pos3;
   }
   if (!p_0463412534(c)){
      return 0;
   }
   return 1;
}
static inline int p_046ElementValueArrayInitializer(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,9);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e19(c)){
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
static inline int e18(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (p_046ElementValueArrayInitializer(c)){
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
      const unsigned char * pos7 = c->pos;
      size_t left8 = ParserContext_saveTree(c);
      size_t log9 = ParserContext_saveLog(c);
      if (p_046Annotation(c)){
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
      return 0;
   }
   return 1;
}
int p_046ElementValue(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,8);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e18(c)){
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
static inline int p_046346434(ParserContext * c){
   if (ParserContext_read(c) != 64){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int e367(ParserContext * c){
   if (!p_046346434(c)){
      return 0;
   }
   ParserContext_beginTree(c,0);
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046QualifiedName(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
   if (!p_046344034(c)){
      return 0;
   }
   {
      size_t left1 = ParserContext_saveTree(c);
      if (!p_046ElementValue(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left1);
   }
   if (!p_046344134(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TAnnotation);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e17(ParserContext * c){
   ParserContext_beginTree(c,0);
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046Identifier(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
   if (!p_046346134(c)){
      return 0;
   }
   {
      size_t left1 = ParserContext_saveTree(c);
      if (!p_046ElementValue(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left1);
   }
   ParserContext_tagTree(c,_TKeyValue);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int p_046ElementValuePair(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,7);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e17(c)){
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
static inline int e366(ParserContext * c){
   if (!p_046344434(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046ElementValuePair(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
   return 1;
}
static inline int e16(ParserContext * c){
   ParserContext_beginTree(c,0);
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046ElementValuePair(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
while (1){
      const unsigned char * pos = c->pos;
      size_t left2 = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!e366(c)){
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
   ParserContext_tagTree(c,_TCommaList);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int p_046ElementValuePairList(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,6);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e16(c)){
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
static inline int e15(ParserContext * c){
   size_t left = ParserContext_saveTree(c);
   if (!p_046ElementValuePairList(c)){
      return 0;
   }
   ParserContext_linkTree(c,_L);
   ParserContext_backTree(c,left);
   return 1;
}
static inline int e11(ParserContext * c){
   if (!p_046346434(c)){
      return 0;
   }
   ParserContext_beginTree(c,0);
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046QualifiedName(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
   if (!p_046344034(c)){
      return 0;
   }
   const unsigned char * pos = c->pos;
   size_t left2 = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e15(c)){
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
   ParserContext_tagTree(c,_TAnnotation);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e368(ParserContext * c){
   if (!p_046346434(c)){
      return 0;
   }
   ParserContext_beginTree(c,0);
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046QualifiedName(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
   ParserContext_tagTree(c,_TAnnotation);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e10(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e11(c)){
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
      if (e367(c)){
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
      if (e368(c)){
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
      return 0;
   }
   return 1;
}
int p_046Annotation(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,3);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e10(c)){
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
int e9(ParserContext * c){
   size_t left = ParserContext_saveTree(c);
   if (!p_046Annotation(c)){
      return 0;
   }
   ParserContext_linkTree(c,_L);
   ParserContext_backTree(c,left);
   return 1;
}
static inline int e8(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!e9(c)){
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
   ParserContext_tagTree(c,_TModifiers);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
int p_046Annotations(ParserContext * c){
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
static inline int e62(ParserContext * c){
   if (!p_04634extends34(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TUpperBound);
   return 1;
}
static inline int e63(ParserContext * c){
   if (!p_04634super34(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TLowerBound);
   return 1;
}
static inline int e61(ParserContext * c){
   ParserContext_foldTree(c,0,_L);
   {
      int temp = 1;
      if (temp){
         const unsigned char * pos = c->pos;
         size_t left = ParserContext_saveTree(c);
         size_t log = ParserContext_saveLog(c);
         if (e62(c)){
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
         if (e63(c)){
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
      if (!p_046ReferenceType(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left7);
   }
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e60(ParserContext * c){
   ParserContext_beginTree(c,0);
   const unsigned char * pos = c->pos;
   size_t left = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e7(c)){
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
   if (!p_046346334(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TTWildCard);
   ParserContext_endTree(c,0,_T,NULL, 0);
   const unsigned char * pos3 = c->pos;
   size_t left4 = ParserContext_saveTree(c);
   size_t log5 = ParserContext_saveLog(c);
   if (!e61(c)){
      backtrack_count = backtrack_count + 1;
      long length = c->pos-pos3;
      total_backtrack_length = total_backtrack_length + length;
      if (backtrack_length < length){
         backtrack_length = length;
      }
      c->pos = pos3;
      ParserContext_backTree(c,left4);
      ParserContext_backLog(c,log5);
   }
   return 1;
}
static inline int e59(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (p_046ReferenceType(c)){
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
      if (e60(c)){
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
static inline int p_046TypeArgument(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,36);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e59(c)){
         ParserContext_memoTreeSucc(c,36,pos);
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
         ParserContext_memoFail(c,36);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e64(ParserContext * c){
   if (!p_046344434(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046TypeArgument(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
   return 1;
}
static inline int e58(ParserContext * c){
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046TypeArgument(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
while (1){
      const unsigned char * pos = c->pos;
      size_t left2 = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!e64(c)){
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
static inline int e57(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_046346034(c)){
      return 0;
   }
   const unsigned char * pos = c->pos;
   size_t left = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e58(c)){
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
   if (!p_046346234(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TTypeArguments);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int p_046TypeArguments(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,35);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e57(c)){
         ParserContext_memoTreeSucc(c,35,pos);
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
         ParserContext_memoFail(c,35);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e56(ParserContext * c){
   ParserContext_foldTree(c,0,_Lbase);
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046TypeArguments(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lparam);
      ParserContext_backTree(c,left);
   }
   ParserContext_tagTree(c,_TGenericType);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e55(ParserContext * c){
   if (!p_046Identifier(c)){
      return 0;
   }
   const unsigned char * pos = c->pos;
   size_t left = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e56(c)){
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
   return 1;
}
static inline int p_046UnannoSimpleClassType(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,34);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e55(c)){
         ParserContext_memoTreeSucc(c,34,pos);
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
         ParserContext_memoFail(c,34);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e54(ParserContext * c){
   ParserContext_beginTree(c,0);
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046Annotations(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lanno);
      ParserContext_backTree(c,left);
   }
   {
      size_t left1 = ParserContext_saveTree(c);
      if (!p_046UnannoSimpleClassType(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left1);
   }
   ParserContext_tagTree(c,_TAnnotated);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e53(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e54(c)){
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
      if (p_046UnannoSimpleClassType(c)){
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
static inline int p_046SimpleClassType(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,33);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e53(c)){
         ParserContext_memoTreeSucc(c,33,pos);
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
         ParserContext_memoFail(c,33);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e65(ParserContext * c){
   ParserContext_foldTree(c,0,_Lprefix);
   if (!p344634(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046SimpleClassType(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lname);
      ParserContext_backTree(c,left);
   }
   ParserContext_tagTree(c,_TJavaQualifiedNamedType);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e52(ParserContext * c){
   if (!p_046SimpleClassType(c)){
      return 0;
   }
while (1){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!e65(c)){
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
int p_046ClassType(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,32);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e52(c)){
         ParserContext_memoTreeSucc(c,32,pos);
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
         ParserContext_memoFail(c,32);
         return 0;
      }
   }
   return memo == 1;
}
int p_046InterfaceType(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,37);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (p_046ClassType(c)){
         ParserContext_memoTreeSucc(c,37,pos);
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
         ParserContext_memoFail(c,37);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e51(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (p_046ClassType(c)){
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
      if (p_046InterfaceType(c)){
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
int p_046ClassOrInterfaceType(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,31);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e51(c)){
         ParserContext_memoTreeSucc(c,31,pos);
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
         ParserContext_memoFail(c,31);
         return 0;
      }
   }
   return memo == 1;
}
int e220(ParserContext * c){
   if (!p_046344434(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046ClassOrInterfaceType(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
   return 1;
}
static inline int e219(ParserContext * c){
   ParserContext_beginTree(c,0);
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046ClassOrInterfaceType(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
while (1){
      const unsigned char * pos = c->pos;
      size_t left2 = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!e220(c)){
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
   ParserContext_tagTree(c,_TList);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
int p_046ClassOrInterfaceTypes(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,89);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e219(c)){
         ParserContext_memoTreeSucc(c,89,pos);
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
         ParserContext_memoFail(c,89);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e244(ParserContext * c){
   if (!p_04634throws34(c)){
      return 0;
   }
   ParserContext_beginTree(c,0);
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046ClassOrInterfaceTypes(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
   ParserContext_tagTree(c,_TThrows);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int p_046Throws(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,101);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e244(c)){
         ParserContext_memoTreeSucc(c,101,pos);
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
         ParserContext_memoFail(c,101);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e243(ParserContext * c){
   size_t left = ParserContext_saveTree(c);
   if (!p_046Throws(c)){
      return 0;
   }
   ParserContext_linkTree(c,_Lthrows);
   ParserContext_backTree(c,left);
   return 1;
}
static inline int e286(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (p_046Annotation(c)){
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
      if (e201(c)){
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
      if (e202(c)){
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
      if (e203(c)){
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
static inline int p_046ConstructorModifier(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,122);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e286(c)){
         ParserContext_memoTreeSucc(c,122,pos);
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
         ParserContext_memoFail(c,122);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e285(ParserContext * c){
   size_t left = ParserContext_saveTree(c);
   if (!p_046ConstructorModifier(c)){
      return 0;
   }
   ParserContext_linkTree(c,_L);
   ParserContext_backTree(c,left);
   return 1;
}
static inline int e284(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!e285(c)){
      return 0;
   }
while (1){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!e285(c)){
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
   ParserContext_tagTree(c,_TModifiers);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int p_046ConstructorModifiers(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,121);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e284(c)){
         ParserContext_memoTreeSucc(c,121,pos);
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
         ParserContext_memoFail(c,121);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e283(ParserContext * c){
   size_t left = ParserContext_saveTree(c);
   if (!p_046ConstructorModifiers(c)){
      return 0;
   }
   ParserContext_linkTree(c,_Lanno);
   ParserContext_backTree(c,left);
   return 1;
}
static inline int e245(ParserContext * c){
   size_t left = ParserContext_saveTree(c);
   if (!p_046Block(c)){
      return 0;
   }
   ParserContext_linkTree(c,_Lbody);
   ParserContext_backTree(c,left);
   return 1;
}
static inline int e238(ParserContext * c){
   if (!p_04634native34(c)){
      return 0;
   }
   ParserContext_beginTree(c,0);
   ParserContext_tagTree(c,_TNative);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e237(ParserContext * c){
   if (!p_04634synchronized34(c)){
      return 0;
   }
   ParserContext_beginTree(c,0);
   ParserContext_tagTree(c,_TSynchronized);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e236(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (p_046Annotation(c)){
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
      if (e201(c)){
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
      if (e202(c)){
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
      if (e203(c)){
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
      if (e204(c)){
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
      if (e122(c)){
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
      if (e205(c)){
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
      if (e237(c)){
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
      if (e238(c)){
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
      if (e206(c)){
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
      return 0;
   }
   return 1;
}
static inline int p_046MethodModifier(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,98);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e236(c)){
         ParserContext_memoTreeSucc(c,98,pos);
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
         ParserContext_memoFail(c,98);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e235(ParserContext * c){
   size_t left = ParserContext_saveTree(c);
   if (!p_046MethodModifier(c)){
      return 0;
   }
   ParserContext_linkTree(c,_L);
   ParserContext_backTree(c,left);
   return 1;
}
static inline int e234(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!e235(c)){
      return 0;
   }
while (1){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!e235(c)){
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
   ParserContext_tagTree(c,_TModifiers);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int p_046MethodModifiers(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,97);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e234(c)){
         ParserContext_memoTreeSucc(c,97,pos);
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
         ParserContext_memoFail(c,97);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e233(ParserContext * c){
   size_t left = ParserContext_saveTree(c);
   if (!p_046MethodModifiers(c)){
      return 0;
   }
   ParserContext_linkTree(c,_Lanno);
   ParserContext_backTree(c,left);
   return 1;
}
static inline int e239(ParserContext * c){
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046TypeParameters(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
   const unsigned char * pos = c->pos;
   size_t left2 = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e240(c)){
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
   return 1;
}
static inline int e242(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_04634void34(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TVoidType);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int p_046Void(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,100);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e242(c)){
         ParserContext_memoTreeSucc(c,100,pos);
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
         ParserContext_memoFail(c,100);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e241(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (p_046Type(c)){
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
      if (p_046Void(c)){
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
static inline int p_046TypeOrVoid(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,99);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e241(c)){
         ParserContext_memoTreeSucc(c,99,pos);
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
         ParserContext_memoFail(c,99);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e232(ParserContext * c){
   ParserContext_beginTree(c,0);
   const unsigned char * pos = c->pos;
   size_t left = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e233(c)){
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
   const unsigned char * pos3 = c->pos;
   size_t left4 = ParserContext_saveTree(c);
   size_t log5 = ParserContext_saveLog(c);
   if (!e239(c)){
      backtrack_count = backtrack_count + 1;
      long length = c->pos-pos3;
      total_backtrack_length = total_backtrack_length + length;
      if (backtrack_length < length){
         backtrack_length = length;
      }
      c->pos = pos3;
      ParserContext_backTree(c,left4);
      ParserContext_backLog(c,log5);
   }
   {
      size_t left6 = ParserContext_saveTree(c);
      if (!p_046TypeOrVoid(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Ltype);
      ParserContext_backTree(c,left6);
   }
   {
      size_t left7 = ParserContext_saveTree(c);
      if (!p_046Identifier(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lname);
      ParserContext_backTree(c,left7);
   }
   if (!p_046344034(c)){
      return 0;
   }
   {
      size_t left8 = ParserContext_saveTree(c);
      if (!p_046MethodParamList(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lparam);
      ParserContext_backTree(c,left8);
   }
   if (!p_046344134(c)){
      return 0;
   }
   const unsigned char * pos9 = c->pos;
   size_t left10 = ParserContext_saveTree(c);
   size_t log11 = ParserContext_saveLog(c);
   if (!e243(c)){
      backtrack_count = backtrack_count + 1;
      long length = c->pos-pos9;
      total_backtrack_length = total_backtrack_length + length;
      if (backtrack_length < length){
         backtrack_length = length;
      }
      c->pos = pos9;
      ParserContext_backTree(c,left10);
      ParserContext_backLog(c,log11);
   }
   {
      int temp = 1;
      if (temp){
         const unsigned char * pos13 = c->pos;
         size_t left14 = ParserContext_saveTree(c);
         size_t log15 = ParserContext_saveLog(c);
         if (e245(c)){
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
         if (p_046345934(c)){
            temp = 0;
         } else{
            backtrack_count = backtrack_count + 1;
            long length = c->pos-pos16;
            total_backtrack_length = total_backtrack_length + length;
            if (backtrack_length < length){
               backtrack_length = length;
            }
            c->pos = pos16;
         }
      }
      if (temp){
         return 0;
      }
   }
   ParserContext_tagTree(c,_TMethodDecl);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int p_046MethodDeclaration(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,96);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e232(c)){
         ParserContext_memoTreeSucc(c,96,pos);
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
         ParserContext_memoFail(c,96);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e281(ParserContext * c){
   if (!p_04634static34(c)){
      return 0;
   }
   ParserContext_beginTree(c,0);
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046Block(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
   ParserContext_tagTree(c,_TJavaStaticInitializer);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e108(ParserContext * c){
   if (!p_04634super34(c)){
      return 0;
   }
   ParserContext_beginTree(c,0);
   ParserContext_tagTree(c,_TSuper);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int p_046Super(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,51);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e108(c)){
         ParserContext_memoTreeSucc(c,51,pos);
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
         ParserContext_memoFail(c,51);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e291(ParserContext * c){
   size_t left = ParserContext_saveTree(c);
   if (!p_046TypeArguments(c)){
      return 0;
   }
   ParserContext_linkTree(c,_L);
   ParserContext_backTree(c,left);
   return 1;
}
static inline int e292(ParserContext * c){
   ParserContext_beginTree(c,0);
   const unsigned char * pos = c->pos;
   size_t left = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e291(c)){
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
   {
      size_t left3 = ParserContext_saveTree(c);
      ParserContext_beginTree(c,0);
      {
         size_t left4 = ParserContext_saveTree(c);
         if (!p_046Super(c)){
            return 0;
         }
         ParserContext_linkTree(c,_L);
         ParserContext_backTree(c,left4);
      }
      if (!p_046344034(c)){
         return 0;
      }
      const unsigned char * pos5 = c->pos;
      size_t left6 = ParserContext_saveTree(c);
      size_t log7 = ParserContext_saveLog(c);
      if (!p_046addArgumentExpressionList(c)){
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos5;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos5;
         ParserContext_backTree(c,left6);
         ParserContext_backLog(c,log7);
      }
      if (!p_046344134(c)){
         return 0;
      }
      ParserContext_tagTree(c,_TList);
      ParserContext_endTree(c,0,_T,NULL, 0);
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left3);
   }
   ParserContext_tagTree(c,_TExplicitConstructorInvocation);
   ParserContext_endTree(c,0,_T,NULL, 0);
   if (!p_046345934(c)){
      return 0;
   }
   return 1;
}
static inline int e320(ParserContext * c){
   ParserContext_beginTree(c,0);
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046QualifiedName(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
   if (!p344634(c)){
      return 0;
   }
   if (!p_04634this34(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TJavaThis);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e321(ParserContext * c){
   ParserContext_beginTree(c,0);
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046QualifiedName(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
   if (!p344634(c)){
      return 0;
   }
   if (!p_04634super34(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TJavaSuper);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e324(ParserContext * c){
   size_t left = ParserContext_saveTree(c);
   ParserContext_beginTree(c,0);
   {
      size_t left1 = ParserContext_saveTree(c);
      if (!p_046TypeArguments(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left1);
   }
   ParserContext_tagTree(c,_TJavaTypeArgs);
   ParserContext_endTree(c,0,_T,NULL, 0);
   ParserContext_linkTree(c,_L);
   ParserContext_backTree(c,left);
   return 1;
}
static inline int e325(ParserContext * c){
   size_t left = ParserContext_saveTree(c);
   if (!p_046ClassBody(c)){
      return 0;
   }
   ParserContext_linkTree(c,_Lbody);
   ParserContext_backTree(c,left);
   return 1;
}
static inline int e323(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_04634new34(c)){
      return 0;
   }
   const unsigned char * pos = c->pos;
   size_t left = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e324(c)){
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
   const unsigned char * pos3 = c->pos;
   size_t left4 = ParserContext_saveTree(c);
   size_t log5 = ParserContext_saveLog(c);
   if (!e7(c)){
      backtrack_count = backtrack_count + 1;
      long length = c->pos-pos3;
      total_backtrack_length = total_backtrack_length + length;
      if (backtrack_length < length){
         backtrack_length = length;
      }
      c->pos = pos3;
      ParserContext_backTree(c,left4);
      ParserContext_backLog(c,log5);
   }
   {
      size_t left6 = ParserContext_saveTree(c);
      if (!p_046ClassOrInterfaceType(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Ltype);
      ParserContext_backTree(c,left6);
   }
   {
      size_t left7 = ParserContext_saveTree(c);
      if (!p_046ArgumentExpressionList(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lparam);
      ParserContext_backTree(c,left7);
   }
   const unsigned char * pos8 = c->pos;
   size_t left9 = ParserContext_saveTree(c);
   size_t log10 = ParserContext_saveLog(c);
   if (!e325(c)){
      backtrack_count = backtrack_count + 1;
      long length = c->pos-pos8;
      total_backtrack_length = total_backtrack_length + length;
      if (backtrack_length < length){
         backtrack_length = length;
      }
      c->pos = pos8;
      ParserContext_backTree(c,left9);
      ParserContext_backLog(c,log10);
   }
   ParserContext_tagTree(c,_TNew);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int p_046InstanceCreationExpression(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,131);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e323(c)){
         ParserContext_memoTreeSucc(c,131,pos);
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
         ParserContext_memoFail(c,131);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e319(ParserContext * c){
   ParserContext_beginTree(c,0);
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046TypeOrVoid(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
   if (!p344634(c)){
      return 0;
   }
   if (!p_04634class34(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TJavaClassLiteral);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int p_046ClassLiteral(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,129);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e319(c)){
         ParserContext_memoTreeSucc(c,129,pos);
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
         ParserContext_memoFail(c,129);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e107(ParserContext * c){
   if (!p_04634this34(c)){
      return 0;
   }
   ParserContext_beginTree(c,0);
   ParserContext_tagTree(c,_TThis);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int p_046This(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,50);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e107(c)){
         ParserContext_memoTreeSucc(c,50,pos);
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
         ParserContext_memoFail(c,50);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e328(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (p_046ClassOrInterfaceType(c)){
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
      if (p_046PrimitiveType(c)){
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
static inline int p_046NonArrayType(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,133);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e328(c)){
         ParserContext_memoTreeSucc(c,133,pos);
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
         ParserContext_memoFail(c,133);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e330(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_04634new34(c)){
      return 0;
   }
   const unsigned char * pos = c->pos;
   size_t left = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e7(c)){
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
   {
      size_t left3 = ParserContext_saveTree(c);
      if (!p_046NonArrayType(c)){
         return 0;
      }
      if (!e68(c)){
         return 0;
      }
while (1){
         const unsigned char * pos4 = c->pos;
         size_t left5 = ParserContext_saveTree(c);
         size_t log6 = ParserContext_saveLog(c);
         if (!e68(c)){
            backtrack_count = backtrack_count + 1;
            long length = c->pos-pos4;
            total_backtrack_length = total_backtrack_length + length;
            if (backtrack_length < length){
               backtrack_length = length;
            }
            c->pos = pos4;
            ParserContext_backTree(c,left5);
            ParserContext_backLog(c,log6);
            break;
         }
      }
      ParserContext_linkTree(c,_Ltype);
      ParserContext_backTree(c,left3);
   }
   {
      size_t left7 = ParserContext_saveTree(c);
      if (!p_046ArrayInitializer(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left7);
   }
   ParserContext_tagTree(c,_TNewArray);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e329(ParserContext * c){
   ParserContext_foldTree(c,0,_Lbase);
   const unsigned char * pos = c->pos;
   size_t left = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e7(c)){
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
   if (!p_046349134(c)){
      return 0;
   }
   {
      size_t left3 = ParserContext_saveTree(c);
      if (!p_046Expression(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lsize);
      ParserContext_backTree(c,left3);
   }
   if (!p_046349334(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TArrayType);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e327(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_04634new34(c)){
      return 0;
   }
   const unsigned char * pos = c->pos;
   size_t left = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e7(c)){
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
   {
      size_t left3 = ParserContext_saveTree(c);
      if (!p_046NonArrayType(c)){
         return 0;
      }
      if (!e329(c)){
         return 0;
      }
while (1){
         const unsigned char * pos4 = c->pos;
         size_t left5 = ParserContext_saveTree(c);
         size_t log6 = ParserContext_saveLog(c);
         if (!e329(c)){
            backtrack_count = backtrack_count + 1;
            long length = c->pos-pos4;
            total_backtrack_length = total_backtrack_length + length;
            if (backtrack_length < length){
               backtrack_length = length;
            }
            c->pos = pos4;
            ParserContext_backTree(c,left5);
            ParserContext_backLog(c,log6);
            break;
         }
      }
while (1){
         const unsigned char * pos7 = c->pos;
         size_t left8 = ParserContext_saveTree(c);
         size_t log9 = ParserContext_saveLog(c);
         if (!e68(c)){
            backtrack_count = backtrack_count + 1;
            long length = c->pos-pos7;
            total_backtrack_length = total_backtrack_length + length;
            if (backtrack_length < length){
               backtrack_length = length;
            }
            c->pos = pos7;
            ParserContext_backTree(c,left8);
            ParserContext_backLog(c,log9);
            break;
         }
      }
      ParserContext_linkTree(c,_Ltype);
      ParserContext_backTree(c,left3);
   }
   ParserContext_tagTree(c,_TNewArray);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e326(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e327(c)){
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
      if (e330(c)){
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
static inline int p_046ArrayCreationExpression(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,132);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e326(c)){
         ParserContext_memoTreeSucc(c,132,pos);
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
         ParserContext_memoFail(c,132);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e322(ParserContext * c){
   ParserContext_beginTree(c,0);
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046Identifier(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lname);
      ParserContext_backTree(c,left);
   }
   {
      size_t left1 = ParserContext_saveTree(c);
      if (!p_046ArgumentExpressionList(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lparam);
      ParserContext_backTree(c,left1);
   }
   ParserContext_tagTree(c,_TApply);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int p_046MethodInvocationExpression(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,130);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e322(c)){
         ParserContext_memoTreeSucc(c,130,pos);
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
         ParserContext_memoFail(c,130);
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
static inline int e95(ParserContext * c){
   ParserContext_tagTree(c,_TInteger);
   return 1;
}
static inline int p_046INT_SUFFIX(ParserContext * c){
   if (!_set11[ParserContext_read(c)]){
      return 0;
   }
   return 1;
}
static inline int e94(ParserContext * c){
   if (!p_046INT_SUFFIX(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TLong);
   return 1;
}
static inline int p_046OCTALDIGIT(ParserContext * c){
   if (!(48 <= ParserContext_prefetch(c) && ParserContext_read(c) < 56)){
      return 0;
   }
   return 1;
}
static inline int e102(ParserContext * c){
   if (ParserContext_read(c) != 92){
      return 0;
   }
   if (!p_046OCTALDIGIT(c)){
      return 0;
   }
   return 1;
}
static inline int e100(ParserContext * c){
   if (ParserContext_read(c) != 92){
      return 0;
   }
   if (!(48 <= ParserContext_prefetch(c) && ParserContext_read(c) < 52)){
      return 0;
   }
   if (!p_046OCTALDIGIT(c)){
      return 0;
   }
   if (!p_046OCTALDIGIT(c)){
      return 0;
   }
   return 1;
}
static inline int e101(ParserContext * c){
   if (ParserContext_read(c) != 92){
      return 0;
   }
   if (!p_046OCTALDIGIT(c)){
      return 0;
   }
   if (!p_046OCTALDIGIT(c)){
      return 0;
   }
   return 1;
}
static inline int p_046HEXDIGIT(ParserContext * c){
   if (!_set5[ParserContext_read(c)]){
      return 0;
   }
   return 1;
}
static inline int p_046HEX_ESCAPE(ParserContext * c){
   if (ParserContext_read(c) != 92){
      return 0;
   }
   if (ParserContext_read(c) != 117){
      return 0;
   }
while (ParserContext_prefetch(c) == 117){
      ParserContext_move(c,1);
   }
   if (!p_046HEXDIGIT(c)){
      return 0;
   }
   if (!p_046HEXDIGIT(c)){
      return 0;
   }
   if (!p_046HEXDIGIT(c)){
      return 0;
   }
   if (!p_046HEXDIGIT(c)){
      return 0;
   }
   return 1;
}
static inline int p_046SIMPLE_ESCAPE(ParserContext * c){
   if (ParserContext_read(c) != 92){
      return 0;
   }
   if (!_set12[ParserContext_read(c)]){
      return 0;
   }
   return 1;
}
static inline int p_046OCTAL_ESCAPE(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      if (e100(c)){
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
      if (e101(c)){
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
      if (e102(c)){
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
static inline int p_046ESCAPE(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      if (p_046SIMPLE_ESCAPE(c)){
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
      if (p_046OCTAL_ESCAPE(c)){
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
      if (p_046HEX_ESCAPE(c)){
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
static inline int e105(ParserContext * c){
   if (_set14[ParserContext_prefetch(c)]){
      return 0;
   }
   if (ParserContext_read(c) == 0){
      return 0;
   }
   return 1;
}
static inline int p_046STRING_CONTENT(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      if (p_046ESCAPE(c)){
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
      if (e105(c)){
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
static inline int e104(ParserContext * c){
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
   int memo = ParserContext_memoLookupTree(c,48);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e104(c)){
         ParserContext_memoTreeSucc(c,48,pos);
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
         ParserContext_memoFail(c,48);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e97(ParserContext * c){
   if (!p_04634true34(c)){
      return 0;
   }
   ParserContext_beginTree(c,0);
   ParserContext_tagTree(c,_TTrue);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e77(ParserContext * c){
while (ParserContext_prefetch(c) == 95){
      ParserContext_move(c,1);
   }
   if (!p_046DIGIT(c)){
      return 0;
   }
   return 1;
}
static inline int p_046DIGITS(ParserContext * c){
   if (!p_046DIGIT(c)){
      return 0;
   }
while (1){
      const unsigned char * pos = c->pos;
      if (!e77(c)){
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
static inline int p_046EXPONENT(ParserContext * c){
   if (!_set2[ParserContext_read(c)]){
      return 0;
   }
   if (_set3[ParserContext_prefetch(c)]){
      ParserContext_move(c,1);
   }
   if (!p_046DIGITS(c)){
      return 0;
   }
   return 1;
}
static inline int e76(ParserContext * c){
   if (ParserContext_read(c) != 46){
      return 0;
   }
   if (!p_046DIGITS(c)){
      return 0;
   }
   return 1;
}
static inline int e78(ParserContext * c){
   if (!p_046DIGITS(c)){
      return 0;
   }
   if (ParserContext_read(c) != 46){
      return 0;
   }
   const unsigned char * pos = c->pos;
   if (!p_046DIGITS(c)){
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
static inline int p_046FRACTION(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      if (e76(c)){
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
      if (e78(c)){
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
static inline int e75(ParserContext * c){
   if (!p_046FRACTION(c)){
      return 0;
   }
   const unsigned char * pos = c->pos;
   if (!p_046EXPONENT(c)){
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
static inline int p_046DOUBLE_SUFFIX(ParserContext * c){
   if (!_set8[ParserContext_read(c)]){
      return 0;
   }
   return 1;
}
static inline int e86(ParserContext * c){
   const unsigned char * pos = c->pos;
   if (!p_046DOUBLE_SUFFIX(c)){
      backtrack_count = backtrack_count + 1;
      long length = c->pos-pos;
      total_backtrack_length = total_backtrack_length + length;
      if (backtrack_length < length){
         backtrack_length = length;
      }
      c->pos = pos;
   }
   ParserContext_tagTree(c,_TDouble);
   return 1;
}
static inline int p_046FLOAT_SUFFIX(ParserContext * c){
   if (!_set7[ParserContext_read(c)]){
      return 0;
   }
   return 1;
}
static inline int e85(ParserContext * c){
   if (!p_046FLOAT_SUFFIX(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TFloat);
   return 1;
}
static inline int e79(ParserContext * c){
   if (!p_046DIGITS(c)){
      return 0;
   }
   if (!p_046EXPONENT(c)){
      return 0;
   }
   return 1;
}
static inline int e82(ParserContext * c){
while (ParserContext_prefetch(c) == 95){
      ParserContext_move(c,1);
   }
   if (!p_046HEXDIGIT(c)){
      return 0;
   }
   return 1;
}
static inline int p_046HEXDIGITS(ParserContext * c){
   if (!p_046HEXDIGIT(c)){
      return 0;
   }
while (1){
      const unsigned char * pos = c->pos;
      if (!e82(c)){
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
static inline int e84(ParserContext * c){
   if (ParserContext_read(c) != 46){
      return 0;
   }
   const unsigned char * pos = c->pos;
   if (!p_046HEXDIGITS(c)){
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
static inline int p_046HEXADECIMAL(ParserContext * c){
   if (ParserContext_read(c) != 48){
      return 0;
   }
   if (!_set4[ParserContext_read(c)]){
      return 0;
   }
   if (!p_046HEXDIGITS(c)){
      return 0;
   }
   return 1;
}
static inline int e83(ParserContext * c){
   if (!p_046HEXADECIMAL(c)){
      return 0;
   }
   const unsigned char * pos = c->pos;
   if (!e84(c)){
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
static inline int p_046BINARYEXPONENT(ParserContext * c){
   if (!_set6[ParserContext_read(c)]){
      return 0;
   }
   if (_set3[ParserContext_prefetch(c)]){
      ParserContext_move(c,1);
   }
   if (!p_046DIGITS(c)){
      return 0;
   }
   return 1;
}
static inline int e81(ParserContext * c){
   if (ParserContext_read(c) != 48){
      return 0;
   }
   if (!_set4[ParserContext_read(c)]){
      return 0;
   }
   if (ParserContext_read(c) != 46){
      return 0;
   }
   if (!p_046HEXDIGITS(c)){
      return 0;
   }
   return 1;
}
static inline int p_046HEXFRACTION(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      if (e81(c)){
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
      if (e83(c)){
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
static inline int e80(ParserContext * c){
   if (!p_046HEXFRACTION(c)){
      return 0;
   }
   if (!p_046BINARYEXPONENT(c)){
      return 0;
   }
   return 1;
}
static inline int p_046FLOAT(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      if (e75(c)){
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
      if (e79(c)){
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
      if (e80(c)){
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
static inline int e74(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_046FLOAT(c)){
      return 0;
   }
   {
      int temp = 1;
      if (temp){
         const unsigned char * pos = c->pos;
         size_t left = ParserContext_saveTree(c);
         size_t log = ParserContext_saveLog(c);
         if (e85(c)){
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
         if (e86(c)){
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
   ParserContext_endTree(c,0,_T,NULL, 0);
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int e88(ParserContext * c){
   if (!p_046DOUBLE_SUFFIX(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TDouble);
   return 1;
}
static inline int e87(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_046DIGITS(c)){
      return 0;
   }
   {
      int temp = 1;
      if (temp){
         const unsigned char * pos = c->pos;
         size_t left = ParserContext_saveTree(c);
         size_t log = ParserContext_saveLog(c);
         if (e85(c)){
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
         if (e88(c)){
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
   ParserContext_endTree(c,0,_T,NULL, 0);
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int e98(ParserContext * c){
   if (!p_04634false34(c)){
      return 0;
   }
   ParserContext_beginTree(c,0);
   ParserContext_tagTree(c,_TFalse);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e73(ParserContext * c){
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
      if (e87(c)){
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
static inline int p_046FloatingPointLiteral(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,44);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e73(c)){
         ParserContext_memoTreeSucc(c,44,pos);
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
         ParserContext_memoFail(c,44);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e93(ParserContext * c){
while (ParserContext_prefetch(c) == 95){
      ParserContext_move(c,1);
   }
   if (!p_046OCTALDIGIT(c)){
      return 0;
   }
   return 1;
}
static inline int p_046OCTAL(ParserContext * c){
   if (ParserContext_read(c) != 48){
      return 0;
   }
while (1){
      const unsigned char * pos = c->pos;
      if (!e93(c)){
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
static inline int e103(ParserContext * c){
   if (_set13[ParserContext_prefetch(c)]){
      return 0;
   }
   if (ParserContext_read(c) == 0){
      return 0;
   }
   return 1;
}
static inline int p_046CHAR_CONTENT(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      if (p_046ESCAPE(c)){
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
      if (e103(c)){
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
static inline int e91(ParserContext * c){
   if (ParserContext_prefetch(c) == 48){
      return 0;
   }
   if (!p_046DIGITS(c)){
      return 0;
   }
   return 1;
}
static inline int e90(ParserContext * c){
   if (ParserContext_read(c) != 48){
      return 0;
   }
   if (_set9[ParserContext_prefetch(c)]){
      return 0;
   }
   return 1;
}
static inline int p_046DECIMAL(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      if (e90(c)){
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
      if (e91(c)){
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
static inline int p_046BINARYDIGIT(ParserContext * c){
   if (!(48 <= ParserContext_prefetch(c) && ParserContext_read(c) < 50)){
      return 0;
   }
   return 1;
}
static inline int e92(ParserContext * c){
while (ParserContext_prefetch(c) == 95){
      ParserContext_move(c,1);
   }
   if (!p_046BINARYDIGIT(c)){
      return 0;
   }
   return 1;
}
static inline int p_046BINARYDIGITS(ParserContext * c){
   if (!p_046BINARYDIGIT(c)){
      return 0;
   }
while (1){
      const unsigned char * pos = c->pos;
      if (!e92(c)){
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
static inline int p_046BINARY(ParserContext * c){
   if (ParserContext_read(c) != 48){
      return 0;
   }
   if (!_set10[ParserContext_read(c)]){
      return 0;
   }
   if (!p_046BINARYDIGITS(c)){
      return 0;
   }
   return 1;
}
static inline int e89(ParserContext * c){
   ParserContext_beginTree(c,0);
   {
      int temp = 1;
      if (temp){
         const unsigned char * pos = c->pos;
         if (p_046DECIMAL(c)){
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
         if (p_046HEXADECIMAL(c)){
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
         if (p_046BINARY(c)){
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
         if (p_046OCTAL(c)){
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
         return 0;
      }
   }
   {
      int temp5 = 1;
      if (temp5){
         const unsigned char * pos6 = c->pos;
         size_t left = ParserContext_saveTree(c);
         size_t log = ParserContext_saveLog(c);
         if (e94(c)){
            temp5 = 0;
         } else{
            backtrack_count = backtrack_count + 1;
            long length = c->pos-pos6;
            total_backtrack_length = total_backtrack_length + length;
            if (backtrack_length < length){
               backtrack_length = length;
            }
            c->pos = pos6;
            ParserContext_backTree(c,left);
            ParserContext_backLog(c,log);
         }
      }
      if (temp5){
         const unsigned char * pos9 = c->pos;
         size_t left10 = ParserContext_saveTree(c);
         size_t log11 = ParserContext_saveLog(c);
         if (e95(c)){
            temp5 = 0;
         } else{
            backtrack_count = backtrack_count + 1;
            long length = c->pos-pos9;
            total_backtrack_length = total_backtrack_length + length;
            if (backtrack_length < length){
               backtrack_length = length;
            }
            c->pos = pos9;
            ParserContext_backTree(c,left10);
            ParserContext_backLog(c,log11);
         }
      }
      if (temp5){
         return 0;
      }
   }
   ParserContext_endTree(c,0,_T,NULL, 0);
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_046IntegerLiteral(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,45);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e89(c)){
         ParserContext_memoTreeSucc(c,45,pos);
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
         ParserContext_memoFail(c,45);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e99(ParserContext * c){
   if (ParserContext_read(c) != 39){
      return 0;
   }
   ParserContext_beginTree(c,0);
   if (!p_046CHAR_CONTENT(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TCharacter);
   ParserContext_endTree(c,0,_T,NULL, 0);
   if (ParserContext_read(c) != 39){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_046CharacterLiteral(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,47);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e99(c)){
         ParserContext_memoTreeSucc(c,47,pos);
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
         ParserContext_memoFail(c,47);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e96(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e97(c)){
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
      if (e98(c)){
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
   int memo = ParserContext_memoLookupTree(c,46);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e96(c)){
         ParserContext_memoTreeSucc(c,46,pos);
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
         ParserContext_memoFail(c,46);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e106(ParserContext * c){
   if (!p_04634null34(c)){
      return 0;
   }
   ParserContext_beginTree(c,0);
   ParserContext_tagTree(c,_TNull);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int p_046NullLiteral(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,49);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e106(c)){
         ParserContext_memoTreeSucc(c,49,pos);
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
         ParserContext_memoFail(c,49);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e72(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (p_046FloatingPointLiteral(c)){
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
      if (p_046IntegerLiteral(c)){
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
      if (p_046BooleanLiteral(c)){
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
      if (p_046CharacterLiteral(c)){
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
      if (p_046StringLiteral(c)){
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
      if (p_046NullLiteral(c)){
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
      return 0;
   }
   return 1;
}
static inline int p_046Literal(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,43);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e72(c)){
         ParserContext_memoTreeSucc(c,43,pos);
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
         ParserContext_memoFail(c,43);
         return 0;
      }
   }
   return memo == 1;
}
static inline int p_046Constant(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,42);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (p_046Literal(c)){
         ParserContext_memoTreeSucc(c,42,pos);
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
         ParserContext_memoFail(c,42);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e109(ParserContext * c){
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
static inline int e332(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_04634new34(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TName);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e331(ParserContext * c){
   ParserContext_beginTree(c,0);
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046ReferenceType(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
   if (!p_04634585834(c)){
      return 0;
   }
   const unsigned char * pos = c->pos;
   size_t left2 = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e291(c)){
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
   {
      size_t left4 = ParserContext_saveTree(c);
      {
         int temp = 1;
         if (temp){
            const unsigned char * pos6 = c->pos;
            size_t left7 = ParserContext_saveTree(c);
            size_t log8 = ParserContext_saveLog(c);
            if (p_046Identifier(c)){
               temp = 0;
            } else{
               backtrack_count = backtrack_count + 1;
               long length = c->pos-pos6;
               total_backtrack_length = total_backtrack_length + length;
               if (backtrack_length < length){
                  backtrack_length = length;
               }
               c->pos = pos6;
               ParserContext_backTree(c,left7);
               ParserContext_backLog(c,log8);
            }
         }
         if (temp){
            const unsigned char * pos9 = c->pos;
            size_t left10 = ParserContext_saveTree(c);
            size_t log11 = ParserContext_saveLog(c);
            if (e332(c)){
               temp = 0;
            } else{
               backtrack_count = backtrack_count + 1;
               long length = c->pos-pos9;
               total_backtrack_length = total_backtrack_length + length;
               if (backtrack_length < length){
                  backtrack_length = length;
               }
               c->pos = pos9;
               ParserContext_backTree(c,left10);
               ParserContext_backLog(c,log11);
            }
         }
         if (temp){
            return 0;
         }
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left4);
   }
   ParserContext_tagTree(c,_TJavaMethodReference);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int p_046MethodReference(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,134);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e331(c)){
         ParserContext_memoTreeSucc(c,134,pos);
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
         ParserContext_memoFail(c,134);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e71(ParserContext * c){
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
      if (p_046This(c)){
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
      if (p_046Super(c)){
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
      if (e109(c)){
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
      if (p_046ClassLiteral(c)){
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
      if (e320(c)){
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
      if (e321(c)){
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
      if (p_046MethodInvocationExpression(c)){
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
      if (p_046InstanceCreationExpression(c)){
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
      if (p_046ArrayCreationExpression(c)){
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
      if (p_046MethodReference(c)){
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
      if (p_046Identifier(c)){
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
static inline int p_046PrimaryExpression(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,41);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e71(c)){
         ParserContext_memoTreeSucc(c,41,pos);
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
         ParserContext_memoFail(c,41);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e70(ParserContext * c){
   if (!p_046PrimaryExpression(c)){
      return 0;
   }
while (1){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!e333(c)){
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
static inline int p_046PostfixExpression(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,40);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e70(c)){
         ParserContext_memoTreeSucc(c,40,pos);
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
         ParserContext_memoFail(c,40);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e293(ParserContext * c){
   ParserContext_beginTree(c,0);
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046PostfixExpression(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
   if (!p344634(c)){
      return 0;
   }
   const unsigned char * pos = c->pos;
   size_t left2 = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e291(c)){
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
   {
      size_t left4 = ParserContext_saveTree(c);
      if (!p_046Super(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left4);
   }
   {
      size_t left5 = ParserContext_saveTree(c);
      ParserContext_beginTree(c,0);
      if (!p_046344034(c)){
         return 0;
      }
      const unsigned char * pos6 = c->pos;
      size_t left7 = ParserContext_saveTree(c);
      size_t log8 = ParserContext_saveLog(c);
      if (!p_046addArgumentExpressionList(c)){
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos6;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos6;
         ParserContext_backTree(c,left7);
         ParserContext_backLog(c,log8);
      }
      if (!p_046344134(c)){
         return 0;
      }
      ParserContext_tagTree(c,_TList);
      ParserContext_endTree(c,0,_T,NULL, 0);
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left5);
   }
   ParserContext_tagTree(c,_TExplicitConstructorInvocation);
   ParserContext_endTree(c,0,_T,NULL, 0);
   if (!p_046345934(c)){
      return 0;
   }
   return 1;
}
static inline int e290(ParserContext * c){
   ParserContext_beginTree(c,0);
   const unsigned char * pos = c->pos;
   size_t left = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e291(c)){
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
   {
      size_t left3 = ParserContext_saveTree(c);
      ParserContext_beginTree(c,0);
      {
         size_t left4 = ParserContext_saveTree(c);
         if (!p_046This(c)){
            return 0;
         }
         ParserContext_linkTree(c,_L);
         ParserContext_backTree(c,left4);
      }
      if (!p_046344034(c)){
         return 0;
      }
      const unsigned char * pos5 = c->pos;
      size_t left6 = ParserContext_saveTree(c);
      size_t log7 = ParserContext_saveLog(c);
      if (!p_046addArgumentExpressionList(c)){
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos5;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos5;
         ParserContext_backTree(c,left6);
         ParserContext_backLog(c,log7);
      }
      if (!p_046344134(c)){
         return 0;
      }
      ParserContext_tagTree(c,_TList);
      ParserContext_endTree(c,0,_T,NULL, 0);
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left3);
   }
   ParserContext_tagTree(c,_TExplicitConstructorInvocation);
   ParserContext_endTree(c,0,_T,NULL, 0);
   if (!p_046345934(c)){
      return 0;
   }
   return 1;
}
static inline int e289(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e290(c)){
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
      if (e292(c)){
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
      if (e293(c)){
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
      return 0;
   }
   return 1;
}
static inline int p_046ExplicitConstructorInvocation(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,124);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e289(c)){
         ParserContext_memoTreeSucc(c,124,pos);
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
         ParserContext_memoFail(c,124);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e288(ParserContext * c){
   size_t left = ParserContext_saveTree(c);
   if (!p_046ExplicitConstructorInvocation(c)){
      return 0;
   }
   ParserContext_linkTree(c,_L);
   ParserContext_backTree(c,left);
   return 1;
}
static inline int e287(ParserContext * c){
   if (!p_0463412334(c)){
      return 0;
   }
   ParserContext_beginTree(c,0);
   const unsigned char * pos = c->pos;
   size_t left = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e288(c)){
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
while (1){
      const unsigned char * pos3 = c->pos;
      size_t left4 = ParserContext_saveTree(c);
      size_t log5 = ParserContext_saveLog(c);
      if (!e136(c)){
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos3;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos3;
         ParserContext_backTree(c,left4);
         ParserContext_backLog(c,log5);
         break;
      }
   }
   ParserContext_tagTree(c,_TBlock);
   ParserContext_endTree(c,0,_T,NULL, 0);
   if (!p_0463412534(c)){
      return 0;
   }
   return 1;
}
static inline int p_046ConstructorBody(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,123);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e287(c)){
         ParserContext_memoTreeSucc(c,123,pos);
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
         ParserContext_memoFail(c,123);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e282(ParserContext * c){
   ParserContext_beginTree(c,0);
   const unsigned char * pos = c->pos;
   size_t left = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e283(c)){
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
   {
      size_t left3 = ParserContext_saveTree(c);
      if (!p_046ReferenceType(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Ltype);
      ParserContext_backTree(c,left3);
   }
   if (!p_046344034(c)){
      return 0;
   }
   {
      size_t left4 = ParserContext_saveTree(c);
      if (!p_046MethodParamList(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lparam);
      ParserContext_backTree(c,left4);
   }
   if (!p_046344134(c)){
      return 0;
   }
   const unsigned char * pos5 = c->pos;
   size_t left6 = ParserContext_saveTree(c);
   size_t log7 = ParserContext_saveLog(c);
   if (!e243(c)){
      backtrack_count = backtrack_count + 1;
      long length = c->pos-pos5;
      total_backtrack_length = total_backtrack_length + length;
      if (backtrack_length < length){
         backtrack_length = length;
      }
      c->pos = pos5;
      ParserContext_backTree(c,left6);
      ParserContext_backLog(c,log7);
   }
   {
      size_t left8 = ParserContext_saveTree(c);
      if (!p_046ConstructorBody(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lbody);
      ParserContext_backTree(c,left8);
   }
   ParserContext_tagTree(c,_TConstructor);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int p_046ConstructorDeclaration(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,120);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e282(c)){
         ParserContext_memoTreeSucc(c,120,pos);
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
         ParserContext_memoFail(c,120);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e280(ParserContext * c){
   ParserContext_beginTree(c,0);
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046Block(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
   ParserContext_tagTree(c,_TJavaInstanceInisializer);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e279(ParserContext * c){
   if (!p_04634default34(c)){
      return 0;
   }
   if (!p_046ElementValue(c)){
      return 0;
   }
   return 1;
}
static inline int p_046AnnotationTypeElementDefaultValue(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,119);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e279(c)){
         ParserContext_memoTreeSucc(c,119,pos);
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
         ParserContext_memoFail(c,119);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e278(ParserContext * c){
   size_t left = ParserContext_saveTree(c);
   if (!p_046AnnotationTypeElementDefaultValue(c)){
      return 0;
   }
   ParserContext_linkTree(c,_L);
   ParserContext_backTree(c,left);
   return 1;
}
static inline int e277(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (p_046Annotation(c)){
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
      if (e201(c)){
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
      if (e204(c)){
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
      return 0;
   }
   return 1;
}
static inline int p_046AnnotationTypeElementModifier(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,118);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e277(c)){
         ParserContext_memoTreeSucc(c,118,pos);
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
         ParserContext_memoFail(c,118);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e276(ParserContext * c){
   size_t left = ParserContext_saveTree(c);
   if (!p_046AnnotationTypeElementModifier(c)){
      return 0;
   }
   ParserContext_linkTree(c,_L);
   ParserContext_backTree(c,left);
   return 1;
}
static inline int e275(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!e276(c)){
      return 0;
   }
while (1){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!e276(c)){
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
   ParserContext_tagTree(c,_TModifiers);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int p_046AnnotationTypeElementModifiers(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,117);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e275(c)){
         ParserContext_memoTreeSucc(c,117,pos);
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
         ParserContext_memoFail(c,117);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e274(ParserContext * c){
   size_t left = ParserContext_saveTree(c);
   if (!p_046AnnotationTypeElementModifiers(c)){
      return 0;
   }
   ParserContext_linkTree(c,_L);
   ParserContext_backTree(c,left);
   return 1;
}
static inline int e273(ParserContext * c){
   ParserContext_beginTree(c,0);
   const unsigned char * pos = c->pos;
   size_t left = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e274(c)){
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
   {
      size_t left3 = ParserContext_saveTree(c);
      if (!p_046Type(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left3);
   }
   {
      size_t left4 = ParserContext_saveTree(c);
      if (!p_046Identifier(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left4);
   }
   if (!p_046344034(c)){
      return 0;
   }
   if (!p_046344134(c)){
      return 0;
   }
   const unsigned char * pos5 = c->pos;
   size_t left6 = ParserContext_saveTree(c);
   size_t log7 = ParserContext_saveLog(c);
   if (!e278(c)){
      backtrack_count = backtrack_count + 1;
      long length = c->pos-pos5;
      total_backtrack_length = total_backtrack_length + length;
      if (backtrack_length < length){
         backtrack_length = length;
      }
      c->pos = pos5;
      ParserContext_backTree(c,left6);
      ParserContext_backLog(c,log7);
   }
   if (!p_046345934(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TJavaAnnotationTypeElement);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int p_046AnnotationTypeElementDeclaration(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,116);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e273(c)){
         ParserContext_memoTreeSucc(c,116,pos);
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
         ParserContext_memoFail(c,116);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e267(ParserContext * c){
   if (!p_04634default34(c)){
      return 0;
   }
   ParserContext_beginTree(c,0);
   ParserContext_tagTree(c,_TDefault);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e266(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (p_046Annotation(c)){
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
      if (e201(c)){
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
      if (e204(c)){
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
      if (e267(c)){
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
      if (e205(c)){
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
      if (e206(c)){
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
      return 0;
   }
   return 1;
}
static inline int p_046InterfaceMethodModifier(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,113);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e266(c)){
         ParserContext_memoTreeSucc(c,113,pos);
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
         ParserContext_memoFail(c,113);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e265(ParserContext * c){
   size_t left = ParserContext_saveTree(c);
   if (!p_046InterfaceMethodModifier(c)){
      return 0;
   }
   ParserContext_linkTree(c,_L);
   ParserContext_backTree(c,left);
   return 1;
}
static inline int e264(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!e265(c)){
      return 0;
   }
while (1){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!e265(c)){
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
   ParserContext_tagTree(c,_TModifiers);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int p_046InterfaceMethodModifiers(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,112);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e264(c)){
         ParserContext_memoTreeSucc(c,112,pos);
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
         ParserContext_memoFail(c,112);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e263(ParserContext * c){
   size_t left = ParserContext_saveTree(c);
   if (!p_046InterfaceMethodModifiers(c)){
      return 0;
   }
   ParserContext_linkTree(c,_Lanno);
   ParserContext_backTree(c,left);
   return 1;
}
static inline int e261(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (p_046Annotation(c)){
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
      if (e201(c)){
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
      if (e122(c)){
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
      if (e205(c)){
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
static inline int p_046ConstantModifier(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,110);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e261(c)){
         ParserContext_memoTreeSucc(c,110,pos);
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
         ParserContext_memoFail(c,110);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e260(ParserContext * c){
   size_t left = ParserContext_saveTree(c);
   if (!p_046ConstantModifier(c)){
      return 0;
   }
   ParserContext_linkTree(c,_L);
   ParserContext_backTree(c,left);
   return 1;
}
static inline int e259(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!e260(c)){
      return 0;
   }
while (1){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!e260(c)){
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
   ParserContext_tagTree(c,_TModifiers);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int p_046ConstantModifiers(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,109);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e259(c)){
         ParserContext_memoTreeSucc(c,109,pos);
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
         ParserContext_memoFail(c,109);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e258(ParserContext * c){
   size_t left = ParserContext_saveTree(c);
   if (!p_046ConstantModifiers(c)){
      return 0;
   }
   ParserContext_linkTree(c,_Lanno);
   ParserContext_backTree(c,left);
   return 1;
}
static inline int e257(ParserContext * c){
   ParserContext_beginTree(c,0);
   const unsigned char * pos = c->pos;
   size_t left = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e258(c)){
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
   {
      size_t left3 = ParserContext_saveTree(c);
      if (!p_046Type(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Ltype);
      ParserContext_backTree(c,left3);
   }
   {
      size_t left4 = ParserContext_saveTree(c);
      if (!p_046InitDeclList(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Llist);
      ParserContext_backTree(c,left4);
   }
   ParserContext_tagTree(c,_TConstantDecl);
   ParserContext_endTree(c,0,_T,NULL, 0);
   if (!p_046345934(c)){
      return 0;
   }
   return 1;
}
static inline int p_046ConstantDeclaration(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,108);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e257(c)){
         ParserContext_memoTreeSucc(c,108,pos);
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
         ParserContext_memoFail(c,108);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e262(ParserContext * c){
   ParserContext_beginTree(c,0);
   const unsigned char * pos = c->pos;
   size_t left = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e263(c)){
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
   const unsigned char * pos3 = c->pos;
   size_t left4 = ParserContext_saveTree(c);
   size_t log5 = ParserContext_saveLog(c);
   if (!e239(c)){
      backtrack_count = backtrack_count + 1;
      long length = c->pos-pos3;
      total_backtrack_length = total_backtrack_length + length;
      if (backtrack_length < length){
         backtrack_length = length;
      }
      c->pos = pos3;
      ParserContext_backTree(c,left4);
      ParserContext_backLog(c,log5);
   }
   {
      size_t left6 = ParserContext_saveTree(c);
      if (!p_046TypeOrVoid(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Ltype);
      ParserContext_backTree(c,left6);
   }
   {
      size_t left7 = ParserContext_saveTree(c);
      if (!p_046Identifier(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lname);
      ParserContext_backTree(c,left7);
   }
   if (!p_046344034(c)){
      return 0;
   }
   {
      size_t left8 = ParserContext_saveTree(c);
      if (!p_046MethodParamList(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lparam);
      ParserContext_backTree(c,left8);
   }
   if (!p_046344134(c)){
      return 0;
   }
   const unsigned char * pos9 = c->pos;
   size_t left10 = ParserContext_saveTree(c);
   size_t log11 = ParserContext_saveLog(c);
   if (!e243(c)){
      backtrack_count = backtrack_count + 1;
      long length = c->pos-pos9;
      total_backtrack_length = total_backtrack_length + length;
      if (backtrack_length < length){
         backtrack_length = length;
      }
      c->pos = pos9;
      ParserContext_backTree(c,left10);
      ParserContext_backLog(c,log11);
   }
   {
      int temp = 1;
      if (temp){
         const unsigned char * pos13 = c->pos;
         size_t left14 = ParserContext_saveTree(c);
         size_t log15 = ParserContext_saveLog(c);
         if (e245(c)){
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
         if (p_046345934(c)){
            temp = 0;
         } else{
            backtrack_count = backtrack_count + 1;
            long length = c->pos-pos16;
            total_backtrack_length = total_backtrack_length + length;
            if (backtrack_length < length){
               backtrack_length = length;
            }
            c->pos = pos16;
         }
      }
      if (temp){
         return 0;
      }
   }
   ParserContext_tagTree(c,_TMethodDecl);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int p_046InterfaceMethodDeclaration(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,111);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e262(c)){
         ParserContext_memoTreeSucc(c,111,pos);
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
         ParserContext_memoFail(c,111);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e256(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (p_046ConstantDeclaration(c)){
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
      if (p_046InterfaceMethodDeclaration(c)){
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
      if (p_046ClassDeclaration(c)){
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
      if (p_046InterfaceDeclaration(c)){
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
      if (e193(c)){
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
      return 0;
   }
   return 1;
}
static inline int p_046InterfaceMemberDeclaration(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,107);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e256(c)){
         ParserContext_memoTreeSucc(c,107,pos);
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
         ParserContext_memoFail(c,107);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e255(ParserContext * c){
   size_t left = ParserContext_saveTree(c);
   if (!p_046InterfaceMemberDeclaration(c)){
      return 0;
   }
   ParserContext_linkTree(c,_L);
   ParserContext_backTree(c,left);
   return 1;
}
static inline int e254(ParserContext * c){
   if (!p_0463412334(c)){
      return 0;
   }
   ParserContext_beginTree(c,0);
while (1){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!e255(c)){
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
   ParserContext_tagTree(c,_TBlock);
   ParserContext_endTree(c,0,_T,NULL, 0);
   if (!p_0463412534(c)){
      return 0;
   }
   return 1;
}
static inline int p_046InterfaceBody(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,106);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e254(c)){
         ParserContext_memoTreeSucc(c,106,pos);
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
         ParserContext_memoFail(c,106);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e253(ParserContext * c){
   if (!p_04634extends34(c)){
      return 0;
   }
   if (!p_046ClassOrInterfaceTypes(c)){
      return 0;
   }
   return 1;
}
static inline int p_046ExtendsInterfaces(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,105);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e253(c)){
         ParserContext_memoTreeSucc(c,105,pos);
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
         ParserContext_memoFail(c,105);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e252(ParserContext * c){
   size_t left = ParserContext_saveTree(c);
   if (!p_046ExtendsInterfaces(c)){
      return 0;
   }
   ParserContext_linkTree(c,_Lsuper);
   ParserContext_backTree(c,left);
   return 1;
}
static inline int e251(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (p_046Annotation(c)){
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
      if (e201(c)){
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
      if (e202(c)){
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
      if (e203(c)){
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
      if (e204(c)){
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
      if (e205(c)){
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
      if (e206(c)){
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
      return 0;
   }
   return 1;
}
static inline int p_046InterfaceModifier(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,104);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e251(c)){
         ParserContext_memoTreeSucc(c,104,pos);
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
         ParserContext_memoFail(c,104);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e250(ParserContext * c){
   size_t left = ParserContext_saveTree(c);
   if (!p_046InterfaceModifier(c)){
      return 0;
   }
   ParserContext_linkTree(c,_L);
   ParserContext_backTree(c,left);
   return 1;
}
static inline int e249(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!e250(c)){
      return 0;
   }
while (1){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!e250(c)){
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
   ParserContext_tagTree(c,_TModifiers);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int p_046InterfaceModifiers(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,103);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e249(c)){
         ParserContext_memoTreeSucc(c,103,pos);
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
         ParserContext_memoFail(c,103);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e248(ParserContext * c){
   size_t left = ParserContext_saveTree(c);
   if (!p_046InterfaceModifiers(c)){
      return 0;
   }
   ParserContext_linkTree(c,_Lanno);
   ParserContext_backTree(c,left);
   return 1;
}
static inline int e247(ParserContext * c){
   ParserContext_beginTree(c,0);
   const unsigned char * pos = c->pos;
   size_t left = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e248(c)){
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
   if (!p_04634interface34(c)){
      return 0;
   }
   {
      size_t left3 = ParserContext_saveTree(c);
      if (!p_046Identifier(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lname);
      ParserContext_backTree(c,left3);
   }
   const unsigned char * pos4 = c->pos;
   size_t left5 = ParserContext_saveTree(c);
   size_t log6 = ParserContext_saveLog(c);
   if (!e207(c)){
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
   const unsigned char * pos7 = c->pos;
   size_t left8 = ParserContext_saveTree(c);
   size_t log9 = ParserContext_saveLog(c);
   if (!e252(c)){
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
   {
      size_t left10 = ParserContext_saveTree(c);
      if (!p_046InterfaceBody(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lbody);
      ParserContext_backTree(c,left10);
   }
   ParserContext_tagTree(c,_TInterfaceDecl);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e246(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e247(c)){
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
      if (e268(c)){
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
int p_046InterfaceDeclaration(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,102);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e246(c)){
         ParserContext_memoTreeSucc(c,102,pos);
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
         ParserContext_memoFail(c,102);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e272(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (p_046AnnotationTypeElementDeclaration(c)){
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
      if (p_046ConstantDeclaration(c)){
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
      if (p_046ClassDeclaration(c)){
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
      if (p_046InterfaceDeclaration(c)){
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
      if (e193(c)){
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
      return 0;
   }
   return 1;
}
static inline int p_046AnnotationTypeMemberDeclaration(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,115);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e272(c)){
         ParserContext_memoTreeSucc(c,115,pos);
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
         ParserContext_memoFail(c,115);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e271(ParserContext * c){
   size_t left = ParserContext_saveTree(c);
   if (!p_046AnnotationTypeMemberDeclaration(c)){
      return 0;
   }
   ParserContext_linkTree(c,_L);
   ParserContext_backTree(c,left);
   return 1;
}
static inline int e270(ParserContext * c){
   if (!p_0463412334(c)){
      return 0;
   }
   ParserContext_beginTree(c,0);
while (1){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!e271(c)){
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
   ParserContext_tagTree(c,_TBlock);
   ParserContext_endTree(c,0,_T,NULL, 0);
   if (!p_0463412534(c)){
      return 0;
   }
   return 1;
}
static inline int p_046AnnotationTypeBody(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,114);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e270(c)){
         ParserContext_memoTreeSucc(c,114,pos);
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
         ParserContext_memoFail(c,114);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e269(ParserContext * c){
   size_t left = ParserContext_saveTree(c);
   if (!p_046InterfaceModifiers(c)){
      return 0;
   }
   ParserContext_linkTree(c,_L);
   ParserContext_backTree(c,left);
   return 1;
}
int e268(ParserContext * c){
   ParserContext_beginTree(c,0);
   const unsigned char * pos = c->pos;
   size_t left = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e269(c)){
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
   if (!p_046346434(c)){
      return 0;
   }
   if (!p_04634interface34(c)){
      return 0;
   }
   {
      size_t left3 = ParserContext_saveTree(c);
      if (!p_046Identifier(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left3);
   }
   {
      size_t left4 = ParserContext_saveTree(c);
      if (!p_046AnnotationTypeBody(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left4);
   }
   ParserContext_tagTree(c,_TJavaAnnotationTypeDecl);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e230(ParserContext * c){
   if (!p_04634transient34(c)){
      return 0;
   }
   ParserContext_beginTree(c,0);
   ParserContext_tagTree(c,_TTransient);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e231(ParserContext * c){
   if (!p_04634volatile34(c)){
      return 0;
   }
   ParserContext_beginTree(c,0);
   ParserContext_tagTree(c,_TVolatile);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e229(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (p_046Annotation(c)){
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
      if (e201(c)){
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
      if (e202(c)){
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
      if (e203(c)){
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
      if (e122(c)){
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
      if (e205(c)){
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
      if (e230(c)){
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
      if (e231(c)){
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
      return 0;
   }
   return 1;
}
static inline int p_046FieldModifier(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,95);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e229(c)){
         ParserContext_memoTreeSucc(c,95,pos);
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
         ParserContext_memoFail(c,95);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e228(ParserContext * c){
   size_t left = ParserContext_saveTree(c);
   if (!p_046FieldModifier(c)){
      return 0;
   }
   ParserContext_linkTree(c,_L);
   ParserContext_backTree(c,left);
   return 1;
}
static inline int e227(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!e228(c)){
      return 0;
   }
while (1){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!e228(c)){
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
   ParserContext_tagTree(c,_TModifiers);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int p_046FieldModifiers(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,94);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e227(c)){
         ParserContext_memoTreeSucc(c,94,pos);
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
         ParserContext_memoFail(c,94);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e226(ParserContext * c){
   size_t left = ParserContext_saveTree(c);
   if (!p_046FieldModifiers(c)){
      return 0;
   }
   ParserContext_linkTree(c,_Lanno);
   ParserContext_backTree(c,left);
   return 1;
}
static inline int e225(ParserContext * c){
   ParserContext_beginTree(c,0);
   const unsigned char * pos = c->pos;
   size_t left = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e226(c)){
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
   {
      size_t left3 = ParserContext_saveTree(c);
      if (!p_046Type(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Ltype);
      ParserContext_backTree(c,left3);
   }
   {
      size_t left4 = ParserContext_saveTree(c);
      if (!p_046InitDeclList(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Llist);
      ParserContext_backTree(c,left4);
   }
   ParserContext_tagTree(c,_TFieldDecl);
   ParserContext_endTree(c,0,_T,NULL, 0);
   if (!p_046345934(c)){
      return 0;
   }
   return 1;
}
static inline int p_046FieldDeclaration(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,93);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e225(c)){
         ParserContext_memoTreeSucc(c,93,pos);
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
         ParserContext_memoFail(c,93);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e224(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (p_046FieldDeclaration(c)){
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
      if (p_046MethodDeclaration(c)){
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
      if (p_046ClassDeclaration(c)){
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
      if (p_046InterfaceDeclaration(c)){
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
      if (e193(c)){
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
      return 0;
   }
   return 1;
}
static inline int p_046ClassMemberDeclaration(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,92);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e224(c)){
         ParserContext_memoTreeSucc(c,92,pos);
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
         ParserContext_memoFail(c,92);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e223(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (p_046ClassMemberDeclaration(c)){
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
      if (e280(c)){
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
      if (e281(c)){
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
      if (p_046ConstructorDeclaration(c)){
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
static inline int p_046ClassBodyDeclaration(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,91);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e223(c)){
         ParserContext_memoTreeSucc(c,91,pos);
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
         ParserContext_memoFail(c,91);
         return 0;
      }
   }
   return memo == 1;
}
int e222(ParserContext * c){
   size_t left = ParserContext_saveTree(c);
   if (!p_046ClassBodyDeclaration(c)){
      return 0;
   }
   ParserContext_linkTree(c,_L);
   ParserContext_backTree(c,left);
   return 1;
}
static inline int e221(ParserContext * c){
   if (!p_0463412334(c)){
      return 0;
   }
   ParserContext_beginTree(c,0);
while (1){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!e222(c)){
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
   ParserContext_tagTree(c,_TBlock);
   ParserContext_endTree(c,0,_T,NULL, 0);
   if (!p_0463412534(c)){
      return 0;
   }
   return 1;
}
int p_046ClassBody(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,90);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e221(c)){
         ParserContext_memoTreeSucc(c,90,pos);
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
         ParserContext_memoFail(c,90);
         return 0;
      }
   }
   return memo == 1;
}
int e300(ParserContext * c){
   size_t left = ParserContext_saveTree(c);
   if (!p_046ClassBody(c)){
      return 0;
   }
   ParserContext_linkTree(c,_L);
   ParserContext_backTree(c,left);
   return 1;
}
static inline int p_04634454534(ParserContext * c){
   if (ParserContext_read(c) != 45){
      return 0;
   }
   if (ParserContext_read(c) != 45){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_046addDec(ParserContext * c){
   if (!p_04634454534(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TDec);
   return 1;
}
static inline int p_04634434334(ParserContext * c){
   if (ParserContext_read(c) != 43){
      return 0;
   }
   if (ParserContext_read(c) != 43){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int p_046addInc(ParserContext * c){
   if (!p_04634434334(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TInc);
   return 1;
}
static inline int p_046addInstanceCreation(ParserContext * c){
   if (!p344634(c)){
      return 0;
   }
   if (!p_04634new34(c)){
      return 0;
   }
   const unsigned char * pos = c->pos;
   size_t left = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e291(c)){
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
   const unsigned char * pos3 = c->pos;
   size_t left4 = ParserContext_saveTree(c);
   size_t log5 = ParserContext_saveLog(c);
   if (!e7(c)){
      backtrack_count = backtrack_count + 1;
      long length = c->pos-pos3;
      total_backtrack_length = total_backtrack_length + length;
      if (backtrack_length < length){
         backtrack_length = length;
      }
      c->pos = pos3;
      ParserContext_backTree(c,left4);
      ParserContext_backLog(c,log5);
   }
   {
      size_t left6 = ParserContext_saveTree(c);
      if (!p_046ClassOrInterfaceType(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left6);
   }
   {
      size_t left7 = ParserContext_saveTree(c);
      if (!p_046ArgumentExpressionList(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left7);
   }
   const unsigned char * pos8 = c->pos;
   size_t left9 = ParserContext_saveTree(c);
   size_t log10 = ParserContext_saveLog(c);
   if (!e300(c)){
      backtrack_count = backtrack_count + 1;
      long length = c->pos-pos8;
      total_backtrack_length = total_backtrack_length + length;
      if (backtrack_length < length){
         backtrack_length = length;
      }
      c->pos = pos8;
      ParserContext_backTree(c,left9);
      ParserContext_backLog(c,log10);
   }
   ParserContext_tagTree(c,_TJavaInstanceCreation);
   return 1;
}
static inline int p_046addIndex(ParserContext * c){
   if (!p_046349134(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      ParserContext_beginTree(c,0);
      {
         size_t left1 = ParserContext_saveTree(c);
         if (!p_046Expression(c)){
            return 0;
         }
         ParserContext_linkTree(c,_L);
         ParserContext_backTree(c,left1);
      }
      ParserContext_tagTree(c,_TList);
      ParserContext_endTree(c,0,_T,NULL, 0);
      ParserContext_linkTree(c,_Lparam);
      ParserContext_backTree(c,left);
   }
   if (!p_046349334(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TIndexer);
   return 1;
}
static inline int p_046addField(ParserContext * c){
   if (!p344634(c)){
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
   if (ParserContext_prefetch(c) == 40){
      return 0;
   }
   ParserContext_tagTree(c,_TField);
   return 1;
}
static inline int p_046addMethodReference(ParserContext * c){
   if (!p_04634585834(c)){
      return 0;
   }
   const unsigned char * pos = c->pos;
   size_t left = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e291(c)){
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
   {
      size_t left3 = ParserContext_saveTree(c);
      if (!p_046Identifier(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left3);
   }
   ParserContext_tagTree(c,_TJavaMethodReference);
   return 1;
}
static inline int p_046addMethodCall(ParserContext * c){
   if (!p344634(c)){
      return 0;
   }
   const unsigned char * pos = c->pos;
   size_t left = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e291(c)){
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
   {
      size_t left3 = ParserContext_saveTree(c);
      if (!p_046Identifier(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lname);
      ParserContext_backTree(c,left3);
   }
   {
      size_t left4 = ParserContext_saveTree(c);
      if (!p_046ArgumentExpressionList(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lparam);
      ParserContext_backTree(c,left4);
   }
   ParserContext_tagTree(c,_TMethodApply);
   return 1;
}
int e333(ParserContext * c){
   ParserContext_foldTree(c,0,_Lrecv);
   {
      int temp = 1;
      if (temp){
         const unsigned char * pos = c->pos;
         size_t left = ParserContext_saveTree(c);
         size_t log = ParserContext_saveLog(c);
         if (p_046addMethodCall(c)){
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
         if (p_046addInstanceCreation(c)){
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
         if (p_046addIndex(c)){
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
         if (p_046addField(c)){
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
         if (p_046addMethodReference(c)){
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
         if (p_046addInc(c)){
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
         if (p_046addDec(c)){
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
         return 0;
      }
   }
   ParserContext_endTree(c,0,_T,NULL, 0);
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
static inline int e339(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_046343334(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046CastNewExpression(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lexpr);
      ParserContext_backTree(c,left);
   }
   ParserContext_tagTree(c,_TNot);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int p_046344534(ParserContext * c){
   if (ParserContext_read(c) != 45){
      return 0;
   }
   if (_set20[ParserContext_prefetch(c)]){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int e337(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_046344534(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046CastNewExpression(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lexpr);
      ParserContext_backTree(c,left);
   }
   ParserContext_tagTree(c,_TMinus);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int p_0463412634(ParserContext * c){
   if (ParserContext_read(c) != 126){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int e338(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_0463412634(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046CastNewExpression(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lexpr);
      ParserContext_backTree(c,left);
   }
   ParserContext_tagTree(c,_TCompl);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e335(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_04634454534(c)){
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
   ParserContext_tagTree(c,_TPreDec);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int p_046344334(ParserContext * c){
   if (ParserContext_read(c) != 43){
      return 0;
   }
   if (_set19[ParserContext_prefetch(c)]){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int e336(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_046344334(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046CastNewExpression(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lexpr);
      ParserContext_backTree(c,left);
   }
   ParserContext_tagTree(c,_TPlus);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e69(ParserContext * c){
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
      if (e334(c)){
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
      if (e335(c)){
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
      if (e336(c)){
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
      if (e337(c)){
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
      if (e338(c)){
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
      if (e339(c)){
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
      return 0;
   }
   return 1;
}
int p_046UnaryExpression(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,39);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e69(c)){
         ParserContext_memoTreeSucc(c,39,pos);
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
         ParserContext_memoFail(c,39);
         return 0;
      }
   }
   return memo == 1;
}
int e334(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_04634434334(c)){
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
   ParserContext_tagTree(c,_TPreInc);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e33(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_046344034(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046Type(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Ltype);
      ParserContext_backTree(c,left);
   }
   if (!p_046344134(c)){
      return 0;
   }
   {
      size_t left1 = ParserContext_saveTree(c);
      if (!p_046CastNewExpression(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lexpr);
      ParserContext_backTree(c,left1);
   }
   ParserContext_tagTree(c,_TCast);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e32(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e33(c)){
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
      if (p_046UnaryExpression(c)){
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
int p_046CastNewExpression(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,21);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e32(c)){
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
static inline int p_046344734(ParserContext * c){
   if (ParserContext_read(c) != 47){
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
static inline int e342(ParserContext * c){
   if (!p_046344734(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TDiv);
   return 1;
}
static inline int p_046343734(ParserContext * c){
   if (ParserContext_read(c) != 37){
      return 0;
   }
   if ((61 <= ParserContext_prefetch(c) && ParserContext_prefetch(c) < 63)){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int e343(ParserContext * c){
   if (!p_046343734(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TMod);
   return 1;
}
static inline int p_046344234(ParserContext * c){
   if (ParserContext_read(c) != 42){
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
static inline int e341(ParserContext * c){
   if (!p_046344234(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TMul);
   return 1;
}
static inline int e340(ParserContext * c){
   ParserContext_foldTree(c,0,_Lleft);
   {
      int temp = 1;
      if (temp){
         const unsigned char * pos = c->pos;
         size_t left = ParserContext_saveTree(c);
         size_t log = ParserContext_saveLog(c);
         if (e341(c)){
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
         if (e342(c)){
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
         if (e343(c)){
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
         return 0;
      }
   }
   {
      size_t left10 = ParserContext_saveTree(c);
      if (!p_046CastNewExpression(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lright);
      ParserContext_backTree(c,left10);
   }
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e31(ParserContext * c){
   if (!p_046CastNewExpression(c)){
      return 0;
   }
while (1){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!e340(c)){
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
static inline int p_046MultiplicativeExpression(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,20);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e31(c)){
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
static inline int e345(ParserContext * c){
   if (!p_046344334(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TAdd);
   return 1;
}
static inline int e346(ParserContext * c){
   if (!p_046344534(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TSub);
   return 1;
}
static inline int e344(ParserContext * c){
   ParserContext_foldTree(c,0,_Lleft);
   {
      int temp = 1;
      if (temp){
         const unsigned char * pos = c->pos;
         size_t left = ParserContext_saveTree(c);
         size_t log = ParserContext_saveLog(c);
         if (e345(c)){
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
         if (e346(c)){
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
      if (!p_046MultiplicativeExpression(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lright);
      ParserContext_backTree(c,left7);
   }
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e30(ParserContext * c){
   if (!p_046MultiplicativeExpression(c)){
      return 0;
   }
while (1){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!e344(c)){
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
static inline int p_046AdditiveExpression(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,19);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e30(c)){
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
static inline int p_0463462626234(ParserContext * c){
   if (ParserContext_read(c) != 62){
      return 0;
   }
   if (ParserContext_read(c) != 62){
      return 0;
   }
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
static inline int e350(ParserContext * c){
   if (!p_0463462626234(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TLogicalRightShift);
   return 1;
}
static inline int p_04634606034(ParserContext * c){
   if (ParserContext_read(c) != 60){
      return 0;
   }
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
static inline int e348(ParserContext * c){
   if (!p_04634606034(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TLeftShift);
   return 1;
}
static inline int p_04634626234(ParserContext * c){
   if (ParserContext_read(c) != 62){
      return 0;
   }
   if (ParserContext_read(c) != 62){
      return 0;
   }
   if ((61 <= ParserContext_prefetch(c) && ParserContext_prefetch(c) < 63)){
      return 0;
   }
   if (!p_046_(c)){
      return 0;
   }
   return 1;
}
static inline int e349(ParserContext * c){
   if (!p_04634626234(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TRightShift);
   return 1;
}
static inline int e347(ParserContext * c){
   ParserContext_foldTree(c,0,_Lleft);
   {
      int temp = 1;
      if (temp){
         const unsigned char * pos = c->pos;
         size_t left = ParserContext_saveTree(c);
         size_t log = ParserContext_saveLog(c);
         if (e348(c)){
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
         if (e349(c)){
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
         if (e350(c)){
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
         return 0;
      }
   }
   {
      size_t left10 = ParserContext_saveTree(c);
      if (!p_046AdditiveExpression(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lright);
      ParserContext_backTree(c,left10);
   }
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e29(ParserContext * c){
   if (!p_046AdditiveExpression(c)){
      return 0;
   }
while (1){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!e347(c)){
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
static inline int p_046ShiftExpression(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,18);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e29(c)){
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
static inline int e355(ParserContext * c){
   if (!p_046346234(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046ShiftExpression(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lright);
      ParserContext_backTree(c,left);
   }
   ParserContext_tagTree(c,_TGreaterThan);
   return 1;
}
static inline int e356(ParserContext * c){
   if (!p_04634instanceof34(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046ReferenceType(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lright);
      ParserContext_backTree(c,left);
   }
   ParserContext_tagTree(c,_TInstanceOf);
   return 1;
}
static inline int p_04634626134(ParserContext * c){
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
static inline int e353(ParserContext * c){
   if (!p_04634626134(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046ShiftExpression(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lright);
      ParserContext_backTree(c,left);
   }
   ParserContext_tagTree(c,_TGreaterThanEquals);
   return 1;
}
static inline int e354(ParserContext * c){
   if (!p_046346034(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046ShiftExpression(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lright);
      ParserContext_backTree(c,left);
   }
   ParserContext_tagTree(c,_TLessThan);
   return 1;
}
static inline int p_04634606134(ParserContext * c){
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
static inline int e352(ParserContext * c){
   if (!p_04634606134(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046ShiftExpression(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lright);
      ParserContext_backTree(c,left);
   }
   ParserContext_tagTree(c,_TLessThanEquals);
   return 1;
}
static inline int p_046addRelationalOperator(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e352(c)){
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
      if (e353(c)){
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
      if (e354(c)){
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
      if (e355(c)){
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
      if (e356(c)){
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
      return 0;
   }
   return 1;
}
static inline int e351(ParserContext * c){
   ParserContext_foldTree(c,0,_Lleft);
   if (!p_046addRelationalOperator(c)){
      return 0;
   }
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e28(ParserContext * c){
   if (!p_046ShiftExpression(c)){
      return 0;
   }
while (1){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!e351(c)){
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
static inline int p_046RelationalExpression(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,17);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e28(c)){
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
static inline int e359(ParserContext * c){
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
static inline int e358(ParserContext * c){
   if (!p_04634616134(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TEquals);
   return 1;
}
static inline int e357(ParserContext * c){
   ParserContext_foldTree(c,0,_Lleft);
   {
      int temp = 1;
      if (temp){
         const unsigned char * pos = c->pos;
         size_t left = ParserContext_saveTree(c);
         size_t log = ParserContext_saveLog(c);
         if (e358(c)){
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
         if (e359(c)){
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
static inline int e27(ParserContext * c){
   if (!p_046RelationalExpression(c)){
      return 0;
   }
while (1){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!e357(c)){
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
   int memo = ParserContext_memoLookupTree(c,16);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e27(c)){
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
static inline int e360(ParserContext * c){
   ParserContext_foldTree(c,0,_Lleft);
   if (!p_046343834(c)){
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
   ParserContext_tagTree(c,_TBitwiseAnd);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e26(ParserContext * c){
   if (!p_046EqualityExpression(c)){
      return 0;
   }
while (1){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!e360(c)){
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
static inline int p_046AndExpression(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,15);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e26(c)){
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
static inline int p_046349434(ParserContext * c){
   if (ParserContext_read(c) != 94){
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
static inline int e361(ParserContext * c){
   ParserContext_foldTree(c,0,_Lleft);
   if (!p_046349434(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046AndExpression(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lright);
      ParserContext_backTree(c,left);
   }
   ParserContext_tagTree(c,_TBitwiseXor);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e25(ParserContext * c){
   if (!p_046AndExpression(c)){
      return 0;
   }
while (1){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!e361(c)){
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
static inline int p_046ExclusiveOrExpression(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,14);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e25(c)){
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
static inline int e362(ParserContext * c){
   ParserContext_foldTree(c,0,_Lleft);
   if (!p_0463412434(c)){
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046ExclusiveOrExpression(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lright);
      ParserContext_backTree(c,left);
   }
   ParserContext_tagTree(c,_TBitwiseOr);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e24(ParserContext * c){
   if (!p_046ExclusiveOrExpression(c)){
      return 0;
   }
while (1){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!e362(c)){
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
int p_046InclusiveOrExpression(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,13);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e24(c)){
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
static inline int e23(ParserContext * c){
   if (!p_046InclusiveOrExpression(c)){
      return 0;
   }
while (1){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!e363(c)){
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
int p_046LogicalAndExpression(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,12);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e23(c)){
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
int e364(ParserContext * c){
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
static inline int e21(ParserContext * c){
   if (!p_046LogicalOrExpression(c)){
      return 0;
   }
while (1){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!e365(c)){
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
int p_046ConditionalExpression(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,10);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e21(c)){
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
int p_046ConstantExpression(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,60);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (p_046ConditionalExpression(c)){
         ParserContext_memoTreeSucc(c,60,pos);
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
         ParserContext_memoFail(c,60);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e125(ParserContext * c){
   size_t left = ParserContext_saveTree(c);
   if (!p_046ConstantExpression(c)){
      return 0;
   }
   ParserContext_linkTree(c,_Lparam);
   ParserContext_backTree(c,left);
   return 1;
}
int e124(ParserContext * c){
   ParserContext_foldTree(c,0,_Lname);
   if (!p_046349134(c)){
      return 0;
   }
   const unsigned char * pos = c->pos;
   size_t left = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e125(c)){
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
   if (!p_046349334(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TArrayName);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e123(ParserContext * c){
   if (!p_046Identifier(c)){
      return 0;
   }
while (1){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (!e124(c)){
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
int p_046VarName(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,59);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e123(c)){
         ParserContext_memoTreeSucc(c,59,pos);
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
         ParserContext_memoFail(c,59);
         return 0;
      }
   }
   return memo == 1;
}
int e168(ParserContext * c){
   ParserContext_beginTree(c,0);
   {
      size_t left = ParserContext_saveTree(c);
      if (!p_046VarName(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lname);
      ParserContext_backTree(c,left);
   }
   const unsigned char * pos = c->pos;
   size_t left2 = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e161(c)){
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
   return 1;
}
static inline int e6(ParserContext * c){
   ParserContext_beginTree(c,0);
   const unsigned char * pos = c->pos;
   size_t left = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e7(c)){
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
   if (!p_04634package34(c)){
      return 0;
   }
   {
      size_t left3 = ParserContext_saveTree(c);
      if (!p_046QualifiedName(c)){
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left3);
   }
   ParserContext_tagTree(c,_TJavaPackageDeclaration);
   ParserContext_endTree(c,0,_T,NULL, 0);
   if (!p_046345934(c)){
      return 0;
   }
   return 1;
}
static inline int p_046PackageDeclaration(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,1);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e6(c)){
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
static inline int e5(ParserContext * c){
   size_t left = ParserContext_saveTree(c);
   if (!p_046PackageDeclaration(c)){
      return 0;
   }
   ParserContext_linkTree(c,_L);
   ParserContext_backTree(c,left);
   return 1;
}
static inline int e376(ParserContext * c){
   int temp = 1;
   if (temp){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (p_046ClassDeclaration(c)){
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
      if (p_046InterfaceDeclaration(c)){
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
      if (e193(c)){
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
      return 0;
   }
   return 1;
}
static inline int p_046TypeDeclaration(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,137);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e376(c)){
         ParserContext_memoTreeSucc(c,137,pos);
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
         ParserContext_memoFail(c,137);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e375(ParserContext * c){
   size_t left = ParserContext_saveTree(c);
   if (!p_046TypeDeclaration(c)){
      return 0;
   }
   ParserContext_linkTree(c,_L);
   ParserContext_backTree(c,left);
   return 1;
}
static inline int p_046EOT(ParserContext * c){
   if (!ParserContext_eof(c)){
      return 0;
   }
   return 1;
}
static inline int e374(ParserContext * c){
   ParserContext_foldTree(c,0,_L);
   if (!p344634(c)){
      return 0;
   }
   if (!p_046344234(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TWildCardName);
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
static inline int e373(ParserContext * c){
   if (!p_046QualifiedName(c)){
      return 0;
   }
   const unsigned char * pos = c->pos;
   size_t left = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   if (!e374(c)){
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
   return 1;
}
static inline int p_046PackageName(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,136);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e373(c)){
         ParserContext_memoTreeSucc(c,136,pos);
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
         ParserContext_memoFail(c,136);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e371(ParserContext * c){
   if (!p_04634static34(c)){
      return 0;
   }
   ParserContext_tagTree(c,_TJavaStaticImport);
   return 1;
}
static inline int e372(ParserContext * c){
   ParserContext_tagTree(c,_TImport);
   return 1;
}
static inline int e370(ParserContext * c){
   ParserContext_beginTree(c,0);
   if (!p_04634import34(c)){
      return 0;
   }
   {
      int temp = 1;
      if (temp){
         const unsigned char * pos = c->pos;
         size_t left = ParserContext_saveTree(c);
         size_t log = ParserContext_saveLog(c);
         if (e371(c)){
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
         if (e372(c)){
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
      if (!p_046PackageName(c)){
         return 0;
      }
      ParserContext_linkTree(c,_Lname);
      ParserContext_backTree(c,left7);
   }
   ParserContext_endTree(c,0,_T,NULL, 0);
   if (!p_046345934(c)){
      return 0;
   }
   return 1;
}
static inline int p_046ImportDeclaration(ParserContext * c){
   int memo = ParserContext_memoLookupTree(c,135);
   if (memo == 0){
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e370(c)){
         ParserContext_memoTreeSucc(c,135,pos);
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
         ParserContext_memoFail(c,135);
         return 0;
      }
   }
   return memo == 1;
}
static inline int e369(ParserContext * c){
   size_t left = ParserContext_saveTree(c);
   if (!p_046ImportDeclaration(c)){
      return 0;
   }
   ParserContext_linkTree(c,_L);
   ParserContext_backTree(c,left);
   return 1;
}
static inline int e0(ParserContext * c){
   if (!p_046_(c)){
      return 0;
   }
   ParserContext_beginTree(c,0);
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
   }
while (1){
      const unsigned char * pos3 = c->pos;
      size_t left4 = ParserContext_saveTree(c);
      size_t log5 = ParserContext_saveLog(c);
      if (!e369(c)){
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos3;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos3;
         ParserContext_backTree(c,left4);
         ParserContext_backLog(c,log5);
         break;
      }
   }
while (1){
      const unsigned char * pos6 = c->pos;
      size_t left7 = ParserContext_saveTree(c);
      size_t log8 = ParserContext_saveLog(c);
      if (!e375(c)){
         backtrack_count = backtrack_count + 1;
         long length = c->pos-pos6;
         total_backtrack_length = total_backtrack_length + length;
         if (backtrack_length < length){
            backtrack_length = length;
         }
         c->pos = pos6;
         ParserContext_backTree(c,left7);
         ParserContext_backLog(c,log8);
         break;
      }
   }
   ParserContext_tagTree(c,_TSource);
   ParserContext_endTree(c,0,_T,NULL, 0);
   if (!p_046_(c)){
      return 0;
   }
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
			//cnez_dump(data, stdout);
			fprintf(stdout, "\n");
			if(getenv("MEM") != NULL) {
				cnez_dump_memory("Memory Usage", data);
			}
			cnez_free(data);
		}
	}
	printf("total backtrack count: %ld\n", backtrack_count);
	printf("longest backtrack length: %ld\n", backtrack_length);
	printf("total backtrack length: %ld\n", total_backtrack_length);
	return 0;
}
#endif/*UNUSE_MAIN*/
void* java8_parse(const char *text, size_t len, void *thunk, void* (*fnew)(symbol_t, const unsigned char *, size_t, size_t, void *), void  (*fset)(void *, size_t, symbol_t, void *, void *), void  (*fgc)(void *, int, void *)){
   void* result = NULL;
   ParserContext * c = ParserContext_new((const unsigned char*)text, len);
   ParserContext_initTreeFunc(c,thunk,fnew,fset,fgc);
   ParserContext_initMemo(c,64/*FIXME*/,138);
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
   return java8_parse(text, len, NULL, NULL, NULL, NULL);
}
long java8_match(const char *text, size_t len){
   long result = -1;
   ParserContext * c = ParserContext_new((const unsigned char*)text, len);
   ParserContext_initNoTreeFunc(c);
   ParserContext_initMemo(c,64/*FIXME*/,138);
   if (p_046File(c)){
      result = c->pos-c->inputs;
   }
   ParserContext_free(c);
   return result;
}
const char* java8_tag(symbol_t n){
   return _tags[n];
}
const char* java8_label(symbol_t n){
   return _labels[n];
}
#ifndef UNUSE_MAIN
int main(int ac, const char **argv){
   return cnez_main(ac, argv, cnez_parse);
}
#endif/*MAIN*/
// End of File