
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


static int _T = 0;
static int _L = 0;
static int _S = 0;
static const unsigned char _index0[256] = {0,0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static const unsigned char _set1[256] = {0,0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static const unsigned char _index2[256] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static const unsigned char _set3[256] = {0,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};
static int _Ltype = 1;
static const unsigned char _index4[256] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,2,0,0,3,0,0,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static const unsigned char _set5[256] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static int _TTBoolean = 1;
static int _TTInt = 2;
static int _TTString = 3;
static int _Lname = 2;
static const unsigned char _index6[256] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,2,3,0,0,4,0,0,5,0,0,0,0,0,6,7,8,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static const unsigned char _index7[256] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static int _TName = 4;
static int _Llist = 3;
static int _TParam = 5;
static int _TList = 6;
static int _Lbody = 4;
static const unsigned char _index8[256] = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,1,2,1,1,1,2,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,1,1,1,2,1,2,3,2,2,2,2,2,2,3,2,2,3,2,2,2,2,2,2,3,2,2,2,2,2,2,2,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0};
static const unsigned char _index9[256] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,1,0,0,0,1,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,0,1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1,3,1,1,1,1,1,1,1,1,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static int _Lexpr = 5;
static const unsigned char _index10[256] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,2,0,2,0,0,0,2,0,0,0,0,0,0,0,2,2,2,2,2,2,2,2,2,2,0,0,0,0,0,0,0,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,0,0,0,0,2,0,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static int _TNot = 7;
static int _TInteger = 8;
static int _TTrue = 9;
static int _TFalse = 10;
static const unsigned char _set11[256] = {0,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};
static int _TString = 11;
static int _TNull = 12;
static int _Lrecv = 6;
static int _TApply = 13;
static int _Lleft = 7;
static const unsigned char _index12[256] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static int _TLessThan = 14;
static int _TGreaterThan = 15;
static int _Lright = 8;
static const unsigned char _index13[256] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static int _TNotEquals = 16;
static int _TEquals = 17;
static const unsigned char _index14[256] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static int _TAnd = 18;
static int _TOr = 19;
static int _TExpression = 20;
static int _TExpressionStatement = 21;
static int _Lcond = 9;
static int _Lthen = 10;
static int _Lelse = 11;
static int _TIf = 22;
static int _TReturn = 23;
static int _TBlock = 24;
static const unsigned char _index15[256] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static int _TVarDecl = 25;
static int _TVarList = 26;
static int _TDeclaration = 27;
static int _TFunction = 28;
static int _TEmpty = 29;
static int _TSource = 30;
static const char * _tags[31] = {"","TBoolean","TInt","TString","Name","Param","List","Not","Integer","True","False","String","Null","Apply","LessThan","GreaterThan","NotEquals","Equals","And","Or","Expression","ExpressionStatement","If","Return","Block","VarDecl","VarList","Declaration","Function","Empty","Source"};
static const char * _labels[12] = {"","type","name","list","body","expr","recv","left","right","cond","then","else"};
static const char * _tables[1] = {""};
// Prototypes
int e40(ParserContext *c);
int e33(ParserContext *c);
int pConditionalExpression(ParserContext *c);
int e54(ParserContext *c);
int pRelationalExpression(ParserContext *c);
int e34(ParserContext *c);
int pUnaryExpression(ParserContext *c);
// [\t-\r ]
static inline int e2(ParserContext * c) {
   if (!_set1[ParserContext_read(c)]) {
      return 0;
   }
   return 1;
}
// '/' [\x01-\t\x0b-\xff]*
static inline int e6(ParserContext * c) {
   if (ParserContext_read(c) != 47) {
      return 0;
   }
   while (_set3[ParserContext_prefetch(c)]) {
      ParserContext_move(c,1);
   }
   return 1;
}
// !"*/" .
static inline int e5(ParserContext * c) {
   if (ParserContext_match2(c,42,47)) {
      return 0;
   }
   if (ParserContext_read(c) == 0) {
      return 0;
   }
   return 1;
}
// '*' (!"*/" .)* "*/"
static inline int e4(ParserContext * c) {
   if (ParserContext_read(c) != 42) {
      return 0;
   }
   while (1) {
      const unsigned char * pos = c->pos;
      // !"*/" .
      if (!e5(c)) {
         c->pos = pos;
         break;
      }
   }
   if (!ParserContext_match2(c,42,47)) {
      return 0;
   }
   return 1;
}
// '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*)
static inline int e3(ParserContext * c) {
   if (ParserContext_read(c) != 47) {
      return 0;
   }
   int temp = 1;
   switch(_index2[ParserContext_prefetch(c)]) {
      case 0: 
      return 0;
      case 1: 
      // '*' (!"*/" .)* "*/"
      temp = e4(c);
      break;
      case 2: 
      // '/' [\x01-\t\x0b-\xff]*
      temp = e6(c);
      break;
   }
   if (!temp) {
      return 0;
   }
   return 1;
}
// [\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*)
static inline int e1(ParserContext * c) {
   int temp = 1;
   switch(_index0[ParserContext_prefetch(c)]) {
      case 0: 
      return 0;
      case 1: 
      // [\t-\r ]
      temp = e2(c);
      break;
      case 2: 
      // '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*)
      temp = e3(c);
      break;
   }
   if (!temp) {
      return 0;
   }
   return 1;
}
// ([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))*
static inline int e0(ParserContext * c) {
   while (1) {
      const unsigned char * pos = c->pos;
      // [\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*)
      if (!e1(c)) {
         c->pos = pos;
         break;
      }
   }
   return 1;
}
// ([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))*
static inline int p__(ParserContext * c) {
   int memo = ParserContext_memoLookup(c,0);
   if (memo == 0) {
      const unsigned char * pos = c->pos;
      if (e0(c)) {
         ParserContext_memoSucc(c,0,pos);
         return 1;
      } else {
         c->pos = pos;
         ParserContext_memoFail(c,0);
         return 0;
      }
   }
   return memo == 1;
}
// ~_ ';' ~_
static inline int e73(ParserContext * c) {
   if (!p__(c)) {
      return 0;
   }
   if (ParserContext_read(c) != 59) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   return 1;
}
// "nt"
static inline int e22(ParserContext * c) {
   if (!ParserContext_match2(c,110,116)) {
      return 0;
   }
   return 1;
}
// 'f'
static inline int e21(ParserContext * c) {
   if (ParserContext_read(c) != 102) {
      return 0;
   }
   return 1;
}
// 'i' ('f' / "nt")
static inline int e20(ParserContext * c) {
   if (ParserContext_read(c) != 105) {
      return 0;
   }
   int temp = 1;
   switch(_index7[ParserContext_prefetch(c)]) {
      case 0: 
      return 0;
      case 1: 
      // 'f'
      temp = e21(c);
      break;
      case 2: 
      // "nt"
      temp = e22(c);
      break;
   }
   if (!temp) {
      return 0;
   }
   return 1;
}
// "return"
static inline int e24(ParserContext * c) {
   if (!ParserContext_match6(c,114,101,116,117,114,110)) {
      return 0;
   }
   return 1;
}
// "long"
static inline int e23(ParserContext * c) {
   if (!ParserContext_match4(c,108,111,110,103)) {
      return 0;
   }
   return 1;
}
// "true"
static inline int e26(ParserContext * c) {
   if (!ParserContext_match4(c,116,114,117,101)) {
      return 0;
   }
   return 1;
}
// "string"
static inline int e25(ParserContext * c) {
   if (!ParserContext_match6(c,115,116,114,105,110,103)) {
      return 0;
   }
   return 1;
}
// "boolean"
static inline int e17(ParserContext * c) {
   if (!ParserContext_match7(c,98,111,111,108,101,97,110)) {
      return 0;
   }
   return 1;
}
// "false"
static inline int e19(ParserContext * c) {
   if (!ParserContext_match5(c,102,97,108,115,101)) {
      return 0;
   }
   return 1;
}
// "else"
static inline int e18(ParserContext * c) {
   if (!ParserContext_match4(c,101,108,115,101)) {
      return 0;
   }
   return 1;
}
// ("boolean" / "else" / "false" / 'i' ('f' / "nt") / "long" / "return" / "string" / "true") ![$0-9A-Z_a-z]
static inline int e16(ParserContext * c) {
   int temp = 1;
   switch(_index6[ParserContext_prefetch(c)]) {
      case 0: 
      return 0;
      case 1: 
      // "boolean"
      temp = e17(c);
      break;
      case 2: 
      // "else"
      temp = e18(c);
      break;
      case 3: 
      // "false"
      temp = e19(c);
      break;
      case 4: 
      // 'i' ('f' / "nt")
      temp = e20(c);
      break;
      case 5: 
      // "long"
      temp = e23(c);
      break;
      case 6: 
      // "return"
      temp = e24(c);
      break;
      case 7: 
      // "string"
      temp = e25(c);
      break;
      case 8: 
      // "true"
      temp = e26(c);
      break;
   }
   if (!temp) {
      return 0;
   }
   if (_set5[ParserContext_prefetch(c)]) {
      return 0;
   }
   return 1;
}
// { ![0-9] !(("boolean" / "else" / "false" / 'i' ('f' / "nt") / "long" / "return" / "string" / "true") ![$0-9A-Z_a-z]) [$0-9A-Z_a-z]+ #Name } ~_
static inline int e15(ParserContext * c) {
   ParserContext_beginTree(c,0);
   if ((48 <= ParserContext_prefetch(c) && ParserContext_prefetch(c) < 58)) {
      return 0;
   }
   {
      const unsigned char * pos = c->pos;
      // ("boolean" / "else" / "false" / 'i' ('f' / "nt") / "long" / "return" / "string" / "true") ![$0-9A-Z_a-z]
      if (e16(c)) {
         return 0;
      }
      c->pos = pos;
   }
   if (!_set5[ParserContext_read(c)]) {
      return 0;
   }
   while (_set5[ParserContext_prefetch(c)]) {
      ParserContext_move(c,1);
   }
   ParserContext_endTree(c,0,_TName,NULL, 0);
   if (!p__(c)) {
      return 0;
   }
   return 1;
}
// { ![0-9] !(("boolean" / "else" / "false" / 'i' ('f' / "nt") / "long" / "return" / "string" / "true") ![$0-9A-Z_a-z]) [$0-9A-Z_a-z]+ #Name } ~_
static inline int pName(ParserContext * c) {
   int memo = ParserContext_memoLookupTree(c,3);
   if (memo == 0) {
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e15(c)) {
         ParserContext_memoTreeSucc(c,3,pos);
         return 1;
      } else {
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         ParserContext_memoFail(c,3);
         return 0;
      }
   }
   return memo == 1;
}
// "!=" ~_ #NotEquals
static inline int e55(ParserContext * c) {
   if (!ParserContext_match2(c,33,61)) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   ParserContext_tagTree(c,_TNotEquals);
   return 1;
}
// [1-9] [0-9]*
static inline int e42(ParserContext * c) {
   if (!(49 <= ParserContext_prefetch(c) && ParserContext_read(c) < 58)) {
      return 0;
   }
   while ((48 <= ParserContext_prefetch(c) && ParserContext_prefetch(c) < 58)) {
      ParserContext_move(c,1);
   }
   return 1;
}
// '0'
static inline int e43(ParserContext * c) {
   if (ParserContext_read(c) != 48) {
      return 0;
   }
   return 1;
}
// { ([1-9] [0-9]* / '0') #Integer } ~_
static inline int e41(ParserContext * c) {
   ParserContext_beginTree(c,0);
   {
      int temp = 1;
      if (temp) {
         const unsigned char * pos = c->pos;
         // [1-9] [0-9]*
         if (e42(c)) {
            temp = 0;
         } else {
            c->pos = pos;
         }
      }
      if (temp) {
         const unsigned char * pos2 = c->pos;
         // '0'
         if (e43(c)) {
            temp = 0;
         } else {
            c->pos = pos2;
         }
      }
      if (temp) {
         return 0;
      }
   }
   ParserContext_endTree(c,0,_TInteger,NULL, 0);
   if (!p__(c)) {
      return 0;
   }
   return 1;
}
// "true" ![$0-9A-Z_a-z] { #True } ~_
static inline int e44(ParserContext * c) {
   if (!ParserContext_match4(c,116,114,117,101)) {
      return 0;
   }
   if (_set5[ParserContext_prefetch(c)]) {
      return 0;
   }
   ParserContext_beginTree(c,0);
   ParserContext_endTree(c,0,_TTrue,NULL, 0);
   if (!p__(c)) {
      return 0;
   }
   return 1;
}
// '"' { [\x01-\t\x0b-!#-[\]-\xff]* #String } '"' ~_
static inline int e46(ParserContext * c) {
   if (ParserContext_read(c) != 34) {
      return 0;
   }
   ParserContext_beginTree(c,0);
   while (_set11[ParserContext_prefetch(c)]) {
      ParserContext_move(c,1);
   }
   ParserContext_endTree(c,0,_TString,NULL, 0);
   if (ParserContext_read(c) != 34) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   return 1;
}
// "false" ![$0-9A-Z_a-z] { #False } ~_
static inline int e45(ParserContext * c) {
   if (!ParserContext_match5(c,102,97,108,115,101)) {
      return 0;
   }
   if (_set5[ParserContext_prefetch(c)]) {
      return 0;
   }
   ParserContext_beginTree(c,0);
   ParserContext_endTree(c,0,_TFalse,NULL, 0);
   if (!p__(c)) {
      return 0;
   }
   return 1;
}
// "||" ~_ #Or
static inline int e59(ParserContext * c) {
   if (!ParserContext_match2(c,124,124)) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   ParserContext_tagTree(c,_TOr);
   return 1;
}
// "&&" ~_ #And
static inline int e58(ParserContext * c) {
   if (!ParserContext_match2(c,38,38)) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   ParserContext_tagTree(c,_TAnd);
   return 1;
}
// '>' !'=' ~_ #GreaterThan
static inline int e53(ParserContext * c) {
   if (ParserContext_read(c) != 62) {
      return 0;
   }
   if (ParserContext_prefetch(c) == 61) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   ParserContext_tagTree(c,_TGreaterThan);
   return 1;
}
// '<' !'=' ~_ #LessThan
static inline int e52(ParserContext * c) {
   if (ParserContext_read(c) != 60) {
      return 0;
   }
   if (ParserContext_prefetch(c) == 61) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   ParserContext_tagTree(c,_TLessThan);
   return 1;
}
// '!' { !'=' ~_ $expr(UnaryExpression) #Not }
static inline int e39(ParserContext * c) {
   if (ParserContext_read(c) != 33) {
      return 0;
   }
   ParserContext_beginTree(c,-1);
   if (ParserContext_prefetch(c) == 61) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!pUnaryExpression(c)) {
         return 0;
      }
      ParserContext_linkTree(c,_Lexpr);
      ParserContext_backTree(c,left);
   }
   ParserContext_endTree(c,0,_TNot,NULL, 0);
   return 1;
}
// '!' { !'=' ~_ $expr(UnaryExpression) #Not } / PrimaryExpression ({$recv _FunctionCall })*
int pUnaryExpression(ParserContext * c) {
   int temp = 1;
   switch(_index10[ParserContext_prefetch(c)]) {
      case 0: 
      return 0;
      case 1: 
      // '!' { !'=' ~_ $expr(UnaryExpression) #Not }
      temp = e39(c);
      break;
      case 2: 
      // PrimaryExpression ({$recv _FunctionCall })*
      temp = e40(c);
      break;
   }
   if (!temp) {
      return 0;
   }
   return 1;
}
// {$left ('<' !'=' ~_ #LessThan / '>' !'=' ~_ #GreaterThan) $right(UnaryExpression) }
static inline int e51(ParserContext * c) {
   ParserContext_foldTree(c,0,_Lleft);
   int temp = 1;
   switch(_index12[ParserContext_prefetch(c)]) {
      case 0: 
      return 0;
      case 1: 
      // '<' !'=' ~_ #LessThan
      temp = e52(c);
      break;
      case 2: 
      // '>' !'=' ~_ #GreaterThan
      temp = e53(c);
      break;
   }
   if (!temp) {
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!pUnaryExpression(c)) {
         return 0;
      }
      ParserContext_linkTree(c,_Lright);
      ParserContext_backTree(c,left);
   }
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
// RelationalExpression ({$left ("!=" ~_ #NotEquals / "==" ~_ #Equals) $right(RelationalExpression) })*
static inline int pEqualityExpression(ParserContext * c) {
   if (!pRelationalExpression(c)) {
      return 0;
   }
   while (1) {
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      // {$left ("!=" ~_ #NotEquals / "==" ~_ #Equals) $right(RelationalExpression) }
      if (!e54(c)) {
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         break;
      }
   }
   return 1;
}
// {$left ("&&" ~_ #And / "||" ~_ #Or) $right(EqualityExpression) }
static inline int e57(ParserContext * c) {
   ParserContext_foldTree(c,0,_Lleft);
   int temp = 1;
   switch(_index14[ParserContext_prefetch(c)]) {
      case 0: 
      return 0;
      case 1: 
      // "&&" ~_ #And
      temp = e58(c);
      break;
      case 2: 
      // "||" ~_ #Or
      temp = e59(c);
      break;
   }
   if (!temp) {
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!pEqualityExpression(c)) {
         return 0;
      }
      ParserContext_linkTree(c,_Lright);
      ParserContext_backTree(c,left);
   }
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
// ',' {$ ~_ $(ConditionalExpression) #Expression }
static inline int e60(ParserContext * c) {
   if (ParserContext_read(c) != 44) {
      return 0;
   }
   ParserContext_foldTree(c,-1,_L);
   if (!p__(c)) {
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!pConditionalExpression(c)) {
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
   ParserContext_endTree(c,0,_TExpression,NULL, 0);
   return 1;
}
// ConditionalExpression (',' {$ ~_ $(ConditionalExpression) #Expression })*
static inline int e38(ParserContext * c) {
   if (!pConditionalExpression(c)) {
      return 0;
   }
   while (1) {
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      // ',' {$ ~_ $(ConditionalExpression) #Expression }
      if (!e60(c)) {
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         break;
      }
   }
   return 1;
}
// ConditionalExpression (',' {$ ~_ $(ConditionalExpression) #Expression })*
static inline int pExpression(ParserContext * c) {
   int memo = ParserContext_memoLookupTree(c,1);
   if (memo == 0) {
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e38(c)) {
         ParserContext_memoTreeSucc(c,1,pos);
         return 1;
      } else {
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         ParserContext_memoFail(c,1);
         return 0;
      }
   }
   return memo == 1;
}
// '(' ~_ Expression ')' ~_
static inline int e48(ParserContext * c) {
   if (ParserContext_read(c) != 40) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   if (!pExpression(c)) {
      return 0;
   }
   if (ParserContext_read(c) != 41) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   return 1;
}
// "null" ![$0-9A-Z_a-z] { #Null } ~_
static inline int e47(ParserContext * c) {
   if (!ParserContext_match4(c,110,117,108,108)) {
      return 0;
   }
   if (_set5[ParserContext_prefetch(c)]) {
      return 0;
   }
   ParserContext_beginTree(c,0);
   ParserContext_endTree(c,0,_TNull,NULL, 0);
   if (!p__(c)) {
      return 0;
   }
   return 1;
}
// { ([1-9] [0-9]* / '0') #Integer } ~_ / "true" ![$0-9A-Z_a-z] { #True } ~_ / "false" ![$0-9A-Z_a-z] { #False } ~_ / '"' { [\x01-\t\x0b-!#-[\]-\xff]* #String } '"' ~_ / "null" ![$0-9A-Z_a-z] { #Null } ~_ / '(' ~_ Expression ')' ~_ / { ![0-9] !(("boolean" / "else" / "false" / 'i' ('f' / "nt") / "long" / "return" / "string" / "true") ![$0-9A-Z_a-z]) [$0-9A-Z_a-z]+ #Name } ~_
static inline int pPrimaryExpression(ParserContext * c) {
   int temp = 1;
   if (temp) {
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      // { ([1-9] [0-9]* / '0') #Integer } ~_
      if (e41(c)) {
         temp = 0;
      } else {
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
      }
   }
   if (temp) {
      const unsigned char * pos4 = c->pos;
      size_t left5 = ParserContext_saveTree(c);
      size_t log6 = ParserContext_saveLog(c);
      // "true" ![$0-9A-Z_a-z] { #True } ~_
      if (e44(c)) {
         temp = 0;
      } else {
         c->pos = pos4;
         ParserContext_backTree(c,left5);
         ParserContext_backLog(c,log6);
      }
   }
   if (temp) {
      const unsigned char * pos7 = c->pos;
      size_t left8 = ParserContext_saveTree(c);
      size_t log9 = ParserContext_saveLog(c);
      // "false" ![$0-9A-Z_a-z] { #False } ~_
      if (e45(c)) {
         temp = 0;
      } else {
         c->pos = pos7;
         ParserContext_backTree(c,left8);
         ParserContext_backLog(c,log9);
      }
   }
   if (temp) {
      const unsigned char * pos10 = c->pos;
      size_t left11 = ParserContext_saveTree(c);
      size_t log12 = ParserContext_saveLog(c);
      // '"' { [\x01-\t\x0b-!#-[\]-\xff]* #String } '"' ~_
      if (e46(c)) {
         temp = 0;
      } else {
         c->pos = pos10;
         ParserContext_backTree(c,left11);
         ParserContext_backLog(c,log12);
      }
   }
   if (temp) {
      const unsigned char * pos13 = c->pos;
      size_t left14 = ParserContext_saveTree(c);
      size_t log15 = ParserContext_saveLog(c);
      // "null" ![$0-9A-Z_a-z] { #Null } ~_
      if (e47(c)) {
         temp = 0;
      } else {
         c->pos = pos13;
         ParserContext_backTree(c,left14);
         ParserContext_backLog(c,log15);
      }
   }
   if (temp) {
      const unsigned char * pos16 = c->pos;
      size_t left17 = ParserContext_saveTree(c);
      size_t log18 = ParserContext_saveLog(c);
      // '(' ~_ Expression ')' ~_
      if (e48(c)) {
         temp = 0;
      } else {
         c->pos = pos16;
         ParserContext_backTree(c,left17);
         ParserContext_backLog(c,log18);
      }
   }
   if (temp) {
      const unsigned char * pos19 = c->pos;
      size_t left20 = ParserContext_saveTree(c);
      size_t log21 = ParserContext_saveLog(c);
      // { ![0-9] !(("boolean" / "else" / "false" / 'i' ('f' / "nt") / "long" / "return" / "string" / "true") ![$0-9A-Z_a-z]) [$0-9A-Z_a-z]+ #Name } ~_
      if (e15(c)) {
         temp = 0;
      } else {
         c->pos = pos19;
         ParserContext_backTree(c,left20);
         ParserContext_backLog(c,log21);
      }
   }
   if (temp) {
      return 0;
   }
   return 1;
}
// ',' ~_ $(Expression)
static inline int e50(ParserContext * c) {
   if (ParserContext_read(c) != 44) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!pExpression(c)) {
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
   return 1;
}
// $(Expression) (',' ~_ $(Expression))*
static inline int p_ArgumentExpressionList(ParserContext * c) {
   {
      size_t left = ParserContext_saveTree(c);
      if (!pExpression(c)) {
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
   while (1) {
      const unsigned char * pos = c->pos;
      size_t left2 = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      // ',' ~_ $(Expression)
      if (!e50(c)) {
         c->pos = pos;
         ParserContext_backTree(c,left2);
         ParserContext_backLog(c,log);
         break;
      }
   }
   return 1;
}
// $(('(' { ~_ _ArgumentExpressionList? ')' ~_ #List })) #Apply
static inline int p_FunctionCall(ParserContext * c) {
   {
      size_t left = ParserContext_saveTree(c);
      if (ParserContext_read(c) != 40) {
         return 0;
      }
      ParserContext_beginTree(c,-1);
      if (!p__(c)) {
         return 0;
      }
      const unsigned char * pos = c->pos;
      size_t left2 = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      // _ArgumentExpressionList
      if (!p_ArgumentExpressionList(c)) {
         c->pos = pos;
         ParserContext_backTree(c,left2);
         ParserContext_backLog(c,log);
      }
      if (ParserContext_read(c) != 41) {
         return 0;
      }
      if (!p__(c)) {
         return 0;
      }
      ParserContext_endTree(c,0,_TList,NULL, 0);
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
   ParserContext_tagTree(c,_TApply);
   return 1;
}
// {$recv _FunctionCall }
static inline int e49(ParserContext * c) {
   ParserContext_foldTree(c,0,_Lrecv);
   if (!p_FunctionCall(c)) {
      return 0;
   }
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
// PrimaryExpression ({$recv _FunctionCall })*
int e40(ParserContext * c) {
   if (!pPrimaryExpression(c)) {
      return 0;
   }
   while (1) {
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      // {$recv _FunctionCall }
      if (!e49(c)) {
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         break;
      }
   }
   return 1;
}
// UnaryExpression ({$left ('<' !'=' ~_ #LessThan / '>' !'=' ~_ #GreaterThan) $right(UnaryExpression) })*
int pRelationalExpression(ParserContext * c) {
   if (!pUnaryExpression(c)) {
      return 0;
   }
   while (1) {
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      // {$left ('<' !'=' ~_ #LessThan / '>' !'=' ~_ #GreaterThan) $right(UnaryExpression) }
      if (!e51(c)) {
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         break;
      }
   }
   return 1;
}
// "==" ~_ #Equals
static inline int e56(ParserContext * c) {
   if (!ParserContext_match2(c,61,61)) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   ParserContext_tagTree(c,_TEquals);
   return 1;
}
// {$left ("!=" ~_ #NotEquals / "==" ~_ #Equals) $right(RelationalExpression) }
int e54(ParserContext * c) {
   ParserContext_foldTree(c,0,_Lleft);
   int temp = 1;
   switch(_index13[ParserContext_prefetch(c)]) {
      case 0: 
      return 0;
      case 1: 
      // "!=" ~_ #NotEquals
      temp = e55(c);
      break;
      case 2: 
      // "==" ~_ #Equals
      temp = e56(c);
      break;
   }
   if (!temp) {
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!pRelationalExpression(c)) {
         return 0;
      }
      ParserContext_linkTree(c,_Lright);
      ParserContext_backTree(c,left);
   }
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
// EqualityExpression ({$left ("&&" ~_ #And / "||" ~_ #Or) $right(EqualityExpression) })*
int pConditionalExpression(ParserContext * c) {
   if (!pEqualityExpression(c)) {
      return 0;
   }
   while (1) {
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      // {$left ("&&" ~_ #And / "||" ~_ #Or) $right(EqualityExpression) }
      if (!e57(c)) {
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         break;
      }
   }
   return 1;
}
// '=' !'=' ~_ ~_ $expr(ConditionalExpression)
static inline int e71(ParserContext * c) {
   if (ParserContext_read(c) != 61) {
      return 0;
   }
   if (ParserContext_prefetch(c) == 61) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!pConditionalExpression(c)) {
         return 0;
      }
      ParserContext_linkTree(c,_Lexpr);
      ParserContext_backTree(c,left);
   }
   return 1;
}
// { $name(Name) ('=' !'=' ~_ ~_ $expr(ConditionalExpression))? #VarDecl } ~_
static inline int pInitDecl(ParserContext * c) {
   ParserContext_beginTree(c,0);
   {
      size_t left = ParserContext_saveTree(c);
      if (!pName(c)) {
         return 0;
      }
      ParserContext_linkTree(c,_Lname);
      ParserContext_backTree(c,left);
   }
   const unsigned char * pos = c->pos;
   size_t left2 = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   // '=' !'=' ~_ ~_ $expr(ConditionalExpression)
   if (!e71(c)) {
      c->pos = pos;
      ParserContext_backTree(c,left2);
      ParserContext_backLog(c,log);
   }
   ParserContext_endTree(c,0,_TVarDecl,NULL, 0);
   if (!p__(c)) {
      return 0;
   }
   return 1;
}
// ',' ~_ ~_ $(InitDecl)
static inline int e72(ParserContext * c) {
   if (ParserContext_read(c) != 44) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!pInitDecl(c)) {
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
   return 1;
}
// ',' ~_ "..." ~_
static inline int e31(ParserContext * c) {
   if (ParserContext_read(c) != 44) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   if (!ParserContext_match3(c,46,46,46)) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   return 1;
}
// "boolean" { ![$0-9A-Z_a-z] #TBoolean }
static inline int e11(ParserContext * c) {
   if (!ParserContext_match7(c,98,111,111,108,101,97,110)) {
      return 0;
   }
   ParserContext_beginTree(c,-7);
   if (_set5[ParserContext_prefetch(c)]) {
      return 0;
   }
   ParserContext_endTree(c,0,_TTBoolean,NULL, 0);
   return 1;
}
// "long" { ![$0-9A-Z_a-z] #TString }
static inline int e13(ParserContext * c) {
   if (!ParserContext_match4(c,108,111,110,103)) {
      return 0;
   }
   ParserContext_beginTree(c,-4);
   if (_set5[ParserContext_prefetch(c)]) {
      return 0;
   }
   ParserContext_endTree(c,0,_TTString,NULL, 0);
   return 1;
}
// "int" { ![$0-9A-Z_a-z] #TInt }
static inline int e12(ParserContext * c) {
   if (!ParserContext_match3(c,105,110,116)) {
      return 0;
   }
   ParserContext_beginTree(c,-3);
   if (_set5[ParserContext_prefetch(c)]) {
      return 0;
   }
   ParserContext_endTree(c,0,_TTInt,NULL, 0);
   return 1;
}
// "string" { ![$0-9A-Z_a-z] #TString }
static inline int e14(ParserContext * c) {
   if (!ParserContext_match6(c,115,116,114,105,110,103)) {
      return 0;
   }
   ParserContext_beginTree(c,-6);
   if (_set5[ParserContext_prefetch(c)]) {
      return 0;
   }
   ParserContext_endTree(c,0,_TTString,NULL, 0);
   return 1;
}
// "boolean" { ![$0-9A-Z_a-z] #TBoolean } / "int" { ![$0-9A-Z_a-z] #TInt } / "long" { ![$0-9A-Z_a-z] #TString } / "string" { ![$0-9A-Z_a-z] #TString }
static inline int e10(ParserContext * c) {
   int temp = 1;
   switch(_index4[ParserContext_prefetch(c)]) {
      case 0: 
      return 0;
      case 1: 
      // "boolean" { ![$0-9A-Z_a-z] #TBoolean }
      temp = e11(c);
      break;
      case 2: 
      // "int" { ![$0-9A-Z_a-z] #TInt }
      temp = e12(c);
      break;
      case 3: 
      // "long" { ![$0-9A-Z_a-z] #TString }
      temp = e13(c);
      break;
      case 4: 
      // "string" { ![$0-9A-Z_a-z] #TString }
      temp = e14(c);
      break;
   }
   if (!temp) {
      return 0;
   }
   return 1;
}
// "boolean" { ![$0-9A-Z_a-z] #TBoolean } / "int" { ![$0-9A-Z_a-z] #TInt } / "long" { ![$0-9A-Z_a-z] #TString } / "string" { ![$0-9A-Z_a-z] #TString }
static inline int pType(ParserContext * c) {
   int memo = ParserContext_memoLookupTree(c,2);
   if (memo == 0) {
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e10(c)) {
         ParserContext_memoTreeSucc(c,2,pos);
         return 1;
      } else {
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         ParserContext_memoFail(c,2);
         return 0;
      }
   }
   return memo == 1;
}
// $name(Name)
static inline int e29(ParserContext * c) {
   size_t left = ParserContext_saveTree(c);
   if (!pName(c)) {
      return 0;
   }
   ParserContext_linkTree(c,_Lname);
   ParserContext_backTree(c,left);
   return 1;
}
// { $type(Type) ~_ ($name(Name))? #Param } ~_
static inline int e28(ParserContext * c) {
   ParserContext_beginTree(c,0);
   {
      size_t left = ParserContext_saveTree(c);
      if (!pType(c)) {
         return 0;
      }
      ParserContext_linkTree(c,_Ltype);
      ParserContext_backTree(c,left);
   }
   if (!p__(c)) {
      return 0;
   }
   const unsigned char * pos = c->pos;
   size_t left2 = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   // $name(Name)
   if (!e29(c)) {
      c->pos = pos;
      ParserContext_backTree(c,left2);
      ParserContext_backLog(c,log);
   }
   ParserContext_endTree(c,0,_TParam,NULL, 0);
   if (!p__(c)) {
      return 0;
   }
   return 1;
}
// { $type(Type) ~_ ($name(Name))? #Param } ~_
static inline int pFunctionParam(ParserContext * c) {
   int memo = ParserContext_memoLookupTree(c,4);
   if (memo == 0) {
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e28(c)) {
         ParserContext_memoTreeSucc(c,4,pos);
         return 1;
      } else {
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         ParserContext_memoFail(c,4);
         return 0;
      }
   }
   return memo == 1;
}
// ',' ~_ $(FunctionParam)
static inline int e30(ParserContext * c) {
   if (ParserContext_read(c) != 44) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!pFunctionParam(c)) {
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
   return 1;
}
// { #Empty } ';' ~_
static inline int e74(ParserContext * c) {
   ParserContext_beginTree(c,0);
   ParserContext_endTree(c,0,_TEmpty,NULL, 0);
   if (ParserContext_read(c) != 59) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   return 1;
}
// '{' { ~_ (([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))* / ($(Statement) / ([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))*) / ($(Statement) / $(Declaration) / ([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))*))* '}' ~_ #Block }
static inline int pBlock(ParserContext * c) {
   int memo = ParserContext_memoLookupTree(c,5);
   if (memo == 0) {
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e33(c)) {
         ParserContext_memoTreeSucc(c,5,pos);
         return 1;
      } else {
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         ParserContext_memoFail(c,5);
         return 0;
      }
   }
   return memo == 1;
}
// "else" ![$0-9A-Z_a-z] ~_ $else(Block)
static inline int e63(ParserContext * c) {
   if (!ParserContext_match4(c,101,108,115,101)) {
      return 0;
   }
   if (_set5[ParserContext_prefetch(c)]) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!pBlock(c)) {
         return 0;
      }
      ParserContext_linkTree(c,_Lelse);
      ParserContext_backTree(c,left);
   }
   return 1;
}
// "if" { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If }
static inline int e62(ParserContext * c) {
   if (!ParserContext_match2(c,105,102)) {
      return 0;
   }
   ParserContext_beginTree(c,-2);
   if (_set5[ParserContext_prefetch(c)]) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   if (ParserContext_read(c) != 40) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!pExpression(c)) {
         return 0;
      }
      ParserContext_linkTree(c,_Lcond);
      ParserContext_backTree(c,left);
   }
   if (ParserContext_read(c) != 41) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   {
      size_t left1 = ParserContext_saveTree(c);
      if (!pBlock(c)) {
         return 0;
      }
      ParserContext_linkTree(c,_Lthen);
      ParserContext_backTree(c,left1);
   }
   const unsigned char * pos = c->pos;
   size_t left3 = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   // "else" ![$0-9A-Z_a-z] ~_ $else(Block)
   if (!e63(c)) {
      c->pos = pos;
      ParserContext_backTree(c,left3);
      ParserContext_backLog(c,log);
   }
   ParserContext_endTree(c,0,_TIf,NULL, 0);
   return 1;
}
// { $expr(Expression) ';' ~_ #ExpressionStatement }
static inline int e37(ParserContext * c) {
   ParserContext_beginTree(c,0);
   {
      size_t left = ParserContext_saveTree(c);
      if (!pExpression(c)) {
         return 0;
      }
      ParserContext_linkTree(c,_Lexpr);
      ParserContext_backTree(c,left);
   }
   if (ParserContext_read(c) != 59) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   ParserContext_endTree(c,0,_TExpressionStatement,NULL, 0);
   return 1;
}
// "if" { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If } / { $expr(Expression) ';' ~_ #ExpressionStatement }
static inline int e61(ParserContext * c) {
   int temp = 1;
   if (temp) {
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      // "if" { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If }
      if (e62(c)) {
         temp = 0;
      } else {
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
      }
   }
   if (temp) {
      const unsigned char * pos4 = c->pos;
      size_t left5 = ParserContext_saveTree(c);
      size_t log6 = ParserContext_saveLog(c);
      // { $expr(Expression) ';' ~_ #ExpressionStatement }
      if (e37(c)) {
         temp = 0;
      } else {
         c->pos = pos4;
         ParserContext_backTree(c,left5);
         ParserContext_backLog(c,log6);
      }
   }
   if (temp) {
      return 0;
   }
   return 1;
}
// $expr(Expression)
static inline int e66(ParserContext * c) {
   size_t left = ParserContext_saveTree(c);
   if (!pExpression(c)) {
      return 0;
   }
   ParserContext_linkTree(c,_Lexpr);
   ParserContext_backTree(c,left);
   return 1;
}
// "return" { ![$0-9A-Z_a-z] ~_ ($expr(Expression))? ';' ~_ #Return }
static inline int e65(ParserContext * c) {
   if (!ParserContext_match6(c,114,101,116,117,114,110)) {
      return 0;
   }
   ParserContext_beginTree(c,-6);
   if (_set5[ParserContext_prefetch(c)]) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   const unsigned char * pos = c->pos;
   size_t left = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   // $expr(Expression)
   if (!e66(c)) {
      c->pos = pos;
      ParserContext_backTree(c,left);
      ParserContext_backLog(c,log);
   }
   if (ParserContext_read(c) != 59) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   ParserContext_endTree(c,0,_TReturn,NULL, 0);
   return 1;
}
// "return" { ![$0-9A-Z_a-z] ~_ ($expr(Expression))? ';' ~_ #Return } / { $expr(Expression) ';' ~_ #ExpressionStatement }
static inline int e64(ParserContext * c) {
   int temp = 1;
   if (temp) {
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      // "return" { ![$0-9A-Z_a-z] ~_ ($expr(Expression))? ';' ~_ #Return }
      if (e65(c)) {
         temp = 0;
      } else {
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
      }
   }
   if (temp) {
      const unsigned char * pos4 = c->pos;
      size_t left5 = ParserContext_saveTree(c);
      size_t log6 = ParserContext_saveLog(c);
      // { $expr(Expression) ';' ~_ #ExpressionStatement }
      if (e37(c)) {
         temp = 0;
      } else {
         c->pos = pos4;
         ParserContext_backTree(c,left5);
         ParserContext_backLog(c,log6);
      }
   }
   if (temp) {
      return 0;
   }
   return 1;
}
// { '{' ~_ (([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))* / ($(Statement) / ([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))*) / ($(Statement) / $(Declaration) / ([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))*))* '}' ~_ #Block #Block }
static inline int e67(ParserContext * c) {
   ParserContext_beginTree(c,-1);
   if (ParserContext_read(c) != 123) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   while (1) {
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      // ([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))* / ($(Statement) / ([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))*) / ($(Statement) / $(Declaration) / ([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))*)
      if (!e34(c)) {
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         break;
      }
      if (pos == c->pos) {
         break;
      }
   }
   if (ParserContext_read(c) != 125) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   ParserContext_tagTree(c,_TBlock);
   ParserContext_endTree(c,0,_TBlock,NULL, 0);
   return 1;
}
// { $expr(Expression) ';' ~_ #ExpressionStatement } / ("if" { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If } / { $expr(Expression) ';' ~_ #ExpressionStatement }) / ("return" { ![$0-9A-Z_a-z] ~_ ($expr(Expression))? ';' ~_ #Return } / { $expr(Expression) ';' ~_ #ExpressionStatement }) / { '{' ~_ (([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))* / ($(Statement) / ([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))*) / ($(Statement) / $(Declaration) / ([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))*))* '}' ~_ #Block #Block }
static inline int pStatement(ParserContext * c) {
   int temp = 1;
   switch(_index9[ParserContext_prefetch(c)]) {
      case 0: 
      return 0;
      case 1: 
      // { $expr(Expression) ';' ~_ #ExpressionStatement }
      temp = e37(c);
      break;
      case 2: 
      // "if" { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If } / { $expr(Expression) ';' ~_ #ExpressionStatement }
      temp = e61(c);
      break;
      case 3: 
      // "return" { ![$0-9A-Z_a-z] ~_ ($expr(Expression))? ';' ~_ #Return } / { $expr(Expression) ';' ~_ #ExpressionStatement }
      temp = e64(c);
      break;
      case 4: 
      // { '{' ~_ (([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))* / ($(Statement) / ([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))*) / ($(Statement) / $(Declaration) / ([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))*))* '}' ~_ #Block #Block }
      temp = e67(c);
      break;
   }
   if (!temp) {
      return 0;
   }
   return 1;
}
// $(Statement)
static inline int e36(ParserContext * c) {
   size_t left = ParserContext_saveTree(c);
   if (!pStatement(c)) {
      return 0;
   }
   ParserContext_linkTree(c,_L);
   ParserContext_backTree(c,left);
   return 1;
}
// $(Statement) / ([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))*
static inline int e35(ParserContext * c) {
   int temp = 1;
   if (temp) {
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      // $(Statement)
      if (e36(c)) {
         temp = 0;
      } else {
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
      }
   }
   if (temp) {
      const unsigned char * pos4 = c->pos;
      // ([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))*
      if (e0(c)) {
         temp = 0;
      } else {
         c->pos = pos4;
      }
   }
   if (temp) {
      return 0;
   }
   return 1;
}
// ~_ $body(Block)
static inline int e32(ParserContext * c) {
   if (!p__(c)) {
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!pBlock(c)) {
         return 0;
      }
      ParserContext_linkTree(c,_Lbody);
      ParserContext_backTree(c,left);
   }
   return 1;
}
// $(FunctionParam) (',' ~_ $(FunctionParam))*
static inline int e27(ParserContext * c) {
   {
      size_t left = ParserContext_saveTree(c);
      if (!pFunctionParam(c)) {
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
   while (1) {
      const unsigned char * pos = c->pos;
      size_t left2 = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      // ',' ~_ $(FunctionParam)
      if (!e30(c)) {
         c->pos = pos;
         ParserContext_backTree(c,left2);
         ParserContext_backLog(c,log);
         break;
      }
   }
   return 1;
}
// { $type(Type) ~_ $name(Name) ~_ '(' ~_ $list(({ ($(FunctionParam) (',' ~_ $(FunctionParam))*)? (',' ~_ "..." ~_)? #List })) ')' ~_ (~_ $body(Block) / ~_ ';' ~_) #Function }
static inline int e9(ParserContext * c) {
   ParserContext_beginTree(c,0);
   {
      size_t left = ParserContext_saveTree(c);
      if (!pType(c)) {
         return 0;
      }
      ParserContext_linkTree(c,_Ltype);
      ParserContext_backTree(c,left);
   }
   if (!p__(c)) {
      return 0;
   }
   {
      size_t left1 = ParserContext_saveTree(c);
      if (!pName(c)) {
         return 0;
      }
      ParserContext_linkTree(c,_Lname);
      ParserContext_backTree(c,left1);
   }
   if (!p__(c)) {
      return 0;
   }
   if (ParserContext_read(c) != 40) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   {
      size_t left2 = ParserContext_saveTree(c);
      ParserContext_beginTree(c,0);
      const unsigned char * pos = c->pos;
      size_t left4 = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      // $(FunctionParam) (',' ~_ $(FunctionParam))*
      if (!e27(c)) {
         c->pos = pos;
         ParserContext_backTree(c,left4);
         ParserContext_backLog(c,log);
      }
      const unsigned char * pos6 = c->pos;
      // ',' ~_ "..." ~_
      if (!e31(c)) {
         c->pos = pos6;
      }
      ParserContext_endTree(c,0,_TList,NULL, 0);
      ParserContext_linkTree(c,_Llist);
      ParserContext_backTree(c,left2);
   }
   if (ParserContext_read(c) != 41) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   {
      int temp = 1;
      if (temp) {
         const unsigned char * pos8 = c->pos;
         size_t left9 = ParserContext_saveTree(c);
         size_t log10 = ParserContext_saveLog(c);
         // ~_ $body(Block)
         if (e32(c)) {
            temp = 0;
         } else {
            c->pos = pos8;
            ParserContext_backTree(c,left9);
            ParserContext_backLog(c,log10);
         }
      }
      if (temp) {
         const unsigned char * pos11 = c->pos;
         // ~_ ';' ~_
         if (e73(c)) {
            temp = 0;
         } else {
            c->pos = pos11;
         }
      }
      if (temp) {
         return 0;
      }
   }
   ParserContext_endTree(c,0,_TFunction,NULL, 0);
   return 1;
}
// { $type(Type) ~_ $list(({ $(InitDecl) (',' ~_ ~_ $(InitDecl))* #VarList })) ';' ~_ #Declaration }
static inline int e70(ParserContext * c) {
   ParserContext_beginTree(c,0);
   {
      size_t left = ParserContext_saveTree(c);
      if (!pType(c)) {
         return 0;
      }
      ParserContext_linkTree(c,_Ltype);
      ParserContext_backTree(c,left);
   }
   if (!p__(c)) {
      return 0;
   }
   {
      size_t left1 = ParserContext_saveTree(c);
      ParserContext_beginTree(c,0);
      {
         size_t left2 = ParserContext_saveTree(c);
         if (!pInitDecl(c)) {
            return 0;
         }
         ParserContext_linkTree(c,_L);
         ParserContext_backTree(c,left2);
      }
      while (1) {
         const unsigned char * pos = c->pos;
         size_t left4 = ParserContext_saveTree(c);
         size_t log = ParserContext_saveLog(c);
         // ',' ~_ ~_ $(InitDecl)
         if (!e72(c)) {
            c->pos = pos;
            ParserContext_backTree(c,left4);
            ParserContext_backLog(c,log);
            break;
         }
      }
      ParserContext_endTree(c,0,_TVarList,NULL, 0);
      ParserContext_linkTree(c,_Llist);
      ParserContext_backTree(c,left1);
   }
   if (ParserContext_read(c) != 59) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   ParserContext_endTree(c,0,_TDeclaration,NULL, 0);
   return 1;
}
// ({ $type(Type) ~_ $name(Name) ~_ '(' ~_ $list(({ ($(FunctionParam) (',' ~_ $(FunctionParam))*)? (',' ~_ "..." ~_)? #List })) ')' ~_ (~_ $body(Block) / ~_ ';' ~_) #Function } / { $type(Type) ~_ $list(({ $(InitDecl) (',' ~_ ~_ $(InitDecl))* #VarList })) ';' ~_ #Declaration })
static inline int pDeclaration(ParserContext * c) {
   int temp = 1;
   if (temp) {
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      // { $type(Type) ~_ $name(Name) ~_ '(' ~_ $list(({ ($(FunctionParam) (',' ~_ $(FunctionParam))*)? (',' ~_ "..." ~_)? #List })) ')' ~_ (~_ $body(Block) / ~_ ';' ~_) #Function }
      if (e9(c)) {
         temp = 0;
      } else {
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
      }
   }
   if (temp) {
      const unsigned char * pos4 = c->pos;
      size_t left5 = ParserContext_saveTree(c);
      size_t log6 = ParserContext_saveLog(c);
      // { $type(Type) ~_ $list(({ $(InitDecl) (',' ~_ ~_ $(InitDecl))* #VarList })) ';' ~_ #Declaration }
      if (e70(c)) {
         temp = 0;
      } else {
         c->pos = pos4;
         ParserContext_backTree(c,left5);
         ParserContext_backLog(c,log6);
      }
   }
   if (temp) {
      return 0;
   }
   return 1;
}
// $(Declaration)
static inline int e69(ParserContext * c) {
   size_t left = ParserContext_saveTree(c);
   if (!pDeclaration(c)) {
      return 0;
   }
   ParserContext_linkTree(c,_L);
   ParserContext_backTree(c,left);
   return 1;
}
// $(Statement) / $(Declaration) / ([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))*
static inline int e68(ParserContext * c) {
   int temp = 1;
   if (temp) {
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      // $(Statement)
      if (e36(c)) {
         temp = 0;
      } else {
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
      }
   }
   if (temp) {
      const unsigned char * pos4 = c->pos;
      size_t left5 = ParserContext_saveTree(c);
      size_t log6 = ParserContext_saveLog(c);
      // $(Declaration)
      if (e69(c)) {
         temp = 0;
      } else {
         c->pos = pos4;
         ParserContext_backTree(c,left5);
         ParserContext_backLog(c,log6);
      }
   }
   if (temp) {
      const unsigned char * pos7 = c->pos;
      // ([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))*
      if (e0(c)) {
         temp = 0;
      } else {
         c->pos = pos7;
      }
   }
   if (temp) {
      return 0;
   }
   return 1;
}
// ([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))* / ($(Statement) / ([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))*) / ($(Statement) / $(Declaration) / ([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))*)
int e34(ParserContext * c) {
   int temp = 1;
   switch(_index8[ParserContext_prefetch(c)]) {
      case 0: 
      return 0;
      case 1: 
      // ([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))*
      temp = e0(c);
      break;
      case 2: 
      // $(Statement) / ([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))*
      temp = e35(c);
      break;
      case 3: 
      // $(Statement) / $(Declaration) / ([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))*
      temp = e68(c);
      break;
   }
   if (!temp) {
      return 0;
   }
   return 1;
}
// '{' { ~_ (([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))* / ($(Statement) / ([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))*) / ($(Statement) / $(Declaration) / ([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))*))* '}' ~_ #Block }
int e33(ParserContext * c) {
   if (ParserContext_read(c) != 123) {
      return 0;
   }
   ParserContext_beginTree(c,-1);
   if (!p__(c)) {
      return 0;
   }
   while (1) {
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      // ([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))* / ($(Statement) / ([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))*) / ($(Statement) / $(Declaration) / ([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))*)
      if (!e34(c)) {
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         break;
      }
      if (pos == c->pos) {
         break;
      }
   }
   if (ParserContext_read(c) != 125) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   ParserContext_endTree(c,0,_TBlock,NULL, 0);
   return 1;
}
// $(({ $type(Type) ~_ $name(Name) ~_ '(' ~_ $list(({ ($(FunctionParam) (',' ~_ $(FunctionParam))*)? (',' ~_ "..." ~_)? #List })) ')' ~_ (~_ $body(Block) / ~_ ';' ~_) #Function } / { $type(Type) ~_ $list(({ $(InitDecl) (',' ~_ ~_ $(InitDecl))* #VarList })) ';' ~_ #Declaration } / { #Empty } ';' ~_))
static inline int e8(ParserContext * c) {
   size_t left = ParserContext_saveTree(c);
   {
      int temp = 1;
      if (temp) {
         const unsigned char * pos = c->pos;
         size_t left3 = ParserContext_saveTree(c);
         size_t log = ParserContext_saveLog(c);
         // { $type(Type) ~_ $name(Name) ~_ '(' ~_ $list(({ ($(FunctionParam) (',' ~_ $(FunctionParam))*)? (',' ~_ "..." ~_)? #List })) ')' ~_ (~_ $body(Block) / ~_ ';' ~_) #Function }
         if (e9(c)) {
            temp = 0;
         } else {
            c->pos = pos;
            ParserContext_backTree(c,left3);
            ParserContext_backLog(c,log);
         }
      }
      if (temp) {
         const unsigned char * pos5 = c->pos;
         size_t left6 = ParserContext_saveTree(c);
         size_t log7 = ParserContext_saveLog(c);
         // { $type(Type) ~_ $list(({ $(InitDecl) (',' ~_ ~_ $(InitDecl))* #VarList })) ';' ~_ #Declaration }
         if (e70(c)) {
            temp = 0;
         } else {
            c->pos = pos5;
            ParserContext_backTree(c,left6);
            ParserContext_backLog(c,log7);
         }
      }
      if (temp) {
         const unsigned char * pos8 = c->pos;
         size_t left9 = ParserContext_saveTree(c);
         size_t log10 = ParserContext_saveLog(c);
         // { #Empty } ';' ~_
         if (e74(c)) {
            temp = 0;
         } else {
            c->pos = pos8;
            ParserContext_backTree(c,left9);
            ParserContext_backLog(c,log10);
         }
      }
      if (temp) {
         return 0;
      }
   }
   ParserContext_linkTree(c,_L);
   ParserContext_backTree(c,left);
   return 1;
}
// $(({ $type(Type) ~_ $name(Name) ~_ '(' ~_ $list(({ ($(FunctionParam) (',' ~_ $(FunctionParam))*)? (',' ~_ "..." ~_)? #List })) ')' ~_ (~_ $body(Block) / ~_ ';' ~_) #Function } / { $type(Type) ~_ $list(({ $(InitDecl) (',' ~_ ~_ $(InitDecl))* #VarList })) ';' ~_ #Declaration } / { #Empty } ';' ~_)) / ([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))*
static inline int e7(ParserContext * c) {
   int temp = 1;
   if (temp) {
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      // $(({ $type(Type) ~_ $name(Name) ~_ '(' ~_ $list(({ ($(FunctionParam) (',' ~_ $(FunctionParam))*)? (',' ~_ "..." ~_)? #List })) ')' ~_ (~_ $body(Block) / ~_ ';' ~_) #Function } / { $type(Type) ~_ $list(({ $(InitDecl) (',' ~_ ~_ $(InitDecl))* #VarList })) ';' ~_ #Declaration } / { #Empty } ';' ~_))
      if (e8(c)) {
         temp = 0;
      } else {
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
      }
   }
   if (temp) {
      const unsigned char * pos4 = c->pos;
      // ([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))*
      if (e0(c)) {
         temp = 0;
      } else {
         c->pos = pos4;
      }
   }
   if (temp) {
      return 0;
   }
   return 1;
}
// ~_ { ($(({ $type(Type) ~_ $name(Name) ~_ '(' ~_ $list(({ ($(FunctionParam) (',' ~_ $(FunctionParam))*)? (',' ~_ "..." ~_)? #List })) ')' ~_ (~_ $body(Block) / ~_ ';' ~_) #Function } / { $type(Type) ~_ $list(({ $(InitDecl) (',' ~_ ~_ $(InitDecl))* #VarList })) ';' ~_ #Declaration } / { #Empty } ';' ~_)) / ([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))*)* #Source } !.
static inline int pFile(ParserContext * c) {
   if (!p__(c)) {
      return 0;
   }
   ParserContext_beginTree(c,0);
   while (1) {
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      // $(({ $type(Type) ~_ $name(Name) ~_ '(' ~_ $list(({ ($(FunctionParam) (',' ~_ $(FunctionParam))*)? (',' ~_ "..." ~_)? #List })) ')' ~_ (~_ $body(Block) / ~_ ';' ~_) #Function } / { $type(Type) ~_ $list(({ $(InitDecl) (',' ~_ ~_ $(InitDecl))* #VarList })) ';' ~_ #Declaration } / { #Empty } ';' ~_)) / ([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))*
      if (!e7(c)) {
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         break;
      }
      if (pos == c->pos) {
         break;
      }
   }
   ParserContext_endTree(c,0,_TSource,NULL, 0);
   if (!ParserContext_eof(c)) {
      return 0;
   }
   return 1;
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
void* cello_parse(const char *text, size_t len, void *thunk, void* (*fnew)(symbol_t, const unsigned char *, size_t, size_t, void *), void  (*fset)(void *, size_t, symbol_t, void *, void *), void  (*fgc)(void *, int, void *)) {
   void* result = NULL;
   ParserContext * c = ParserContext_new((const unsigned char*)text, len);
   ParserContext_initTreeFunc(c,thunk,fnew,fset,fgc);
   ParserContext_initMemo(c,64,6);
   if (pFile(c)) {
      result = c->left;
      if (result == NULL) {
         result = c->fnew(0, (const unsigned char*)text, (c->pos - (const unsigned char*)text), 0, c->thunk);
      }
   }
   ParserContext_free(c);
   return result;
}
static void* cnez_parse(const char *text, size_t len) {
   return cello_parse(text, len, NULL, NULL, NULL, NULL);
}
long cello_match(const char *text, size_t len) {
   long result = -1;
   ParserContext * c = ParserContext_new((const unsigned char*)text, len);
   ParserContext_initNoTreeFunc(c);
   ParserContext_initMemo(c,64,6);
   if (pFile(c)) {
      result = c->pos-c->inputs;
   }
   ParserContext_free(c);
   return result;
}
const char* cello_tag(symbol_t n) {
   return _tags[n];
}
const char* cello_label(symbol_t n) {
   return _labels[n];
}
#ifndef UNUSE_MAIN
int main(int ac, const char **argv) {
   return cnez_main(ac, argv, cnez_parse);
}
#endif/*MAIN*/
// End of File
