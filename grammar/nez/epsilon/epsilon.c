
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
static const unsigned char _set4[256] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static int _Lname = 1;
static const unsigned char _index5[256] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,2,0,0,3,0,0,0,0,0,0,0,0,4,0,0,0,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static int _TName = 1;
static int _Llist = 2;
static int _TParam = 2;
static int _TList = 3;
static int _Lbody = 3;
static const unsigned char _index6[256] = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,1,2,1,1,1,2,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,1,2,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,1,1,1,2,1,2,2,2,2,2,3,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,3,2,2,2,2,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0};
static const unsigned char _index7[256] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,1,0,0,0,1,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,0,2,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,0,1,1,1,1,1,1,1,1,3,1,1,1,1,1,1,1,1,4,1,1,1,1,1,1,1,1,5,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static int _Lexpr = 4;
static const unsigned char _index8[256] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,1,0,0,0,1,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static int _Lleft = 5;
static const unsigned char _index9[256] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,2,0,3,0,0,0,4,0,0,0,0,0,0,0,5,5,5,5,5,5,5,5,5,5,0,0,0,0,0,0,0,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,0,0,0,0,3,0,3,3,3,3,3,6,3,3,3,3,3,3,3,7,3,3,3,3,3,8,3,3,3,3,3,3,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static int _TNot = 4;
static int _TInteger = 5;
static int _TTrue = 6;
static int _TFalse = 7;
static const unsigned char _set10[256] = {0,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};
static int _TString = 8;
static int _TNull = 9;
static int _TFunctionExpr = 10;
static int _Lrecv = 6;
static int _TApply = 11;
static int _TAssign = 12;
static int _TAssignMul = 13;
static int _TAssignDiv = 14;
static int _TAssignMod = 15;
static int _TAssignAdd = 16;
static int _TAssignSub = 17;
static int _TAssignLeftShift = 18;
static int _TAssignRightShift = 19;
static int _TAssignLogicalRightShift = 20;
static int _TAssignBitwiseAnd = 21;
static int _TAssignBitwiseXOr = 22;
static int _TAssignBitwiseOr = 23;
static int _Lright = 7;
static const unsigned char _index11[256] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static const unsigned char _index12[256] = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0};
static int _TLessThan = 24;
static int _TLessThanEquals = 25;
static int _TGreaterThan = 26;
static int _TGreaterThanEquals = 27;
static const unsigned char _index13[256] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static int _TNotEquals = 28;
static int _TEquals = 29;
static int _TAnd = 30;
static int _TOr = 31;
static int _Lcond = 8;
static int _Lthen = 9;
static int _Lelse = 10;
static int _TConditional = 32;
static int _TExpression = 33;
static int _TExpressionStatement = 34;
static int _TEmpty = 35;
static int _TIf = 36;
static int _TIf1 = 37;
static int _TIf2 = 38;
static int _TIf3 = 39;
static int _TIf4 = 40;
static int _TIf5 = 41;
static int _TIf6 = 42;
static int _TIf7 = 43;
static int _TIf8 = 44;
static int _TIf9 = 45;
static int _TIfA = 46;
static int _TIfB = 47;
static int _TReturn = 48;
static int _TBlock = 49;
static const unsigned char _index14[256] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static int _TVarDecl = 50;
static int _TVarList = 51;
static int _TDeclaration = 52;
static int _TFunctionDecl = 53;
static int _TSource = 54;
static const char * _tags[55] = {"","Name","Param","List","Not","Integer","True","False","String","Null","FunctionExpr","Apply","Assign","AssignMul","AssignDiv","AssignMod","AssignAdd","AssignSub","AssignLeftShift","AssignRightShift","AssignLogicalRightShift","AssignBitwiseAnd","AssignBitwiseXOr","AssignBitwiseOr","LessThan","LessThanEquals","GreaterThan","GreaterThanEquals","NotEquals","Equals","And","Or","Conditional","Expression","ExpressionStatement","Empty","If","If1","If2","If3","If4","If5","If6","If7","If8","If9","IfA","IfB","Return","Block","VarDecl","VarList","Declaration","FunctionDecl","Source"};
static const char * _labels[11] = {"","name","list","body","expr","left","recv","right","cond","then","else"};
static const char * _tables[1] = {""};
// Prototypes
int e75(ParserContext *c);
int e31(ParserContext *c);
int pExpression(ParserContext *c);
int e22(ParserContext *c);
int e32(ParserContext *c);
int pRelationalExpression(ParserContext *c);
int e68(ParserContext *c);
int e67(ParserContext *c);
int pUnaryExpression(ParserContext *c);
int pAssignmentExpression(ParserContext *c);
int pBlock(ParserContext *c);
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
// !'=' ~_ #GreaterThan
static inline int e73(ParserContext * c) {
   if (ParserContext_prefetch(c) == 61) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   ParserContext_tagTree(c,_TGreaterThan);
   return 1;
}
// '=' !'=' ~_ #GreaterThanEquals
static inline int e74(ParserContext * c) {
   if (ParserContext_read(c) != 61) {
      return 0;
   }
   if (ParserContext_prefetch(c) == 61) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   ParserContext_tagTree(c,_TGreaterThanEquals);
   return 1;
}
// '>' (!'=' ~_ #GreaterThan / '=' !'=' ~_ #GreaterThanEquals)
static inline int e72(ParserContext * c) {
   if (ParserContext_read(c) != 62) {
      return 0;
   }
   int temp = 1;
   switch(_index12[ParserContext_prefetch(c)]) {
      case 0: 
      return 0;
      case 1: 
      // !'=' ~_ #GreaterThan
      temp = e73(c);
      break;
      case 2: 
      // '=' !'=' ~_ #GreaterThanEquals
      temp = e74(c);
      break;
   }
   if (!temp) {
      return 0;
   }
   return 1;
}
// "==" ~_ #Equals
static inline int e77(ParserContext * c) {
   if (!ParserContext_match2(c,61,61)) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   ParserContext_tagTree(c,_TEquals);
   return 1;
}
// "!=" ~_ #NotEquals
static inline int e76(ParserContext * c) {
   if (!ParserContext_match2(c,33,61)) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   ParserContext_tagTree(c,_TNotEquals);
   return 1;
}
// "null" ![$0-9A-Z_a-z] { #Null } ~_
static inline int e40(ParserContext * c) {
   if (!ParserContext_match4(c,110,117,108,108)) {
      return 0;
   }
   if (_set4[ParserContext_prefetch(c)]) {
      return 0;
   }
   ParserContext_beginTree(c,0);
   ParserContext_endTree(c,0,_TNull,NULL, 0);
   if (!p__(c)) {
      return 0;
   }
   return 1;
}
// "function" ![$0-9A-Z_a-z]
static inline int e13(ParserContext * c) {
   if (!ParserContext_match8(c,102,117,110,99,116,105,111,110)) {
      return 0;
   }
   if (_set4[ParserContext_prefetch(c)]) {
      return 0;
   }
   return 1;
}
// "else" ![$0-9A-Z_a-z]
static inline int e12(ParserContext * c) {
   if (!ParserContext_match4(c,101,108,115,101)) {
      return 0;
   }
   if (_set4[ParserContext_prefetch(c)]) {
      return 0;
   }
   return 1;
}
// "return" ![$0-9A-Z_a-z]
static inline int e15(ParserContext * c) {
   if (!ParserContext_match6(c,114,101,116,117,114,110)) {
      return 0;
   }
   if (_set4[ParserContext_prefetch(c)]) {
      return 0;
   }
   return 1;
}
// "if" ![$0-9A-Z_a-z]
static inline int e14(ParserContext * c) {
   if (!ParserContext_match2(c,105,102)) {
      return 0;
   }
   if (_set4[ParserContext_prefetch(c)]) {
      return 0;
   }
   return 1;
}
// "var" ![$0-9A-Z_a-z]
static inline int e16(ParserContext * c) {
   if (!ParserContext_match3(c,118,97,114)) {
      return 0;
   }
   if (_set4[ParserContext_prefetch(c)]) {
      return 0;
   }
   return 1;
}
// ("else" ![$0-9A-Z_a-z] / "function" ![$0-9A-Z_a-z] / "if" ![$0-9A-Z_a-z] / "return" ![$0-9A-Z_a-z] / "var" ![$0-9A-Z_a-z]) ![$0-9A-Z_a-z]
static inline int e11(ParserContext * c) {
   int temp = 1;
   switch(_index5[ParserContext_prefetch(c)]) {
      case 0: 
      return 0;
      case 1: 
      // "else" ![$0-9A-Z_a-z]
      temp = e12(c);
      break;
      case 2: 
      // "function" ![$0-9A-Z_a-z]
      temp = e13(c);
      break;
      case 3: 
      // "if" ![$0-9A-Z_a-z]
      temp = e14(c);
      break;
      case 4: 
      // "return" ![$0-9A-Z_a-z]
      temp = e15(c);
      break;
      case 5: 
      // "var" ![$0-9A-Z_a-z]
      temp = e16(c);
      break;
   }
   if (!temp) {
      return 0;
   }
   if (_set4[ParserContext_prefetch(c)]) {
      return 0;
   }
   return 1;
}
// { ![0-9] !(("else" ![$0-9A-Z_a-z] / "function" ![$0-9A-Z_a-z] / "if" ![$0-9A-Z_a-z] / "return" ![$0-9A-Z_a-z] / "var" ![$0-9A-Z_a-z]) ![$0-9A-Z_a-z]) [$0-9A-Z_a-z]+ #Name } ~_
static inline int e10(ParserContext * c) {
   ParserContext_beginTree(c,0);
   if ((48 <= ParserContext_prefetch(c) && ParserContext_prefetch(c) < 58)) {
      return 0;
   }
   {
      const unsigned char * pos = c->pos;
      // ("else" ![$0-9A-Z_a-z] / "function" ![$0-9A-Z_a-z] / "if" ![$0-9A-Z_a-z] / "return" ![$0-9A-Z_a-z] / "var" ![$0-9A-Z_a-z]) ![$0-9A-Z_a-z]
      if (e11(c)) {
         return 0;
      }
      c->pos = pos;
   }
   if (!_set4[ParserContext_read(c)]) {
      return 0;
   }
   while (_set4[ParserContext_prefetch(c)]) {
      ParserContext_move(c,1);
   }
   ParserContext_endTree(c,0,_TName,NULL, 0);
   if (!p__(c)) {
      return 0;
   }
   return 1;
}
// { ![0-9] !(("else" ![$0-9A-Z_a-z] / "function" ![$0-9A-Z_a-z] / "if" ![$0-9A-Z_a-z] / "return" ![$0-9A-Z_a-z] / "var" ![$0-9A-Z_a-z]) ![$0-9A-Z_a-z]) [$0-9A-Z_a-z]+ #Name } ~_
static inline int pIdentifier(ParserContext * c) {
   int memo = ParserContext_memoLookupTree(c,4);
   if (memo == 0) {
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e10(c)) {
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
// $name(Identifier) ~_
static inline int e43(ParserContext * c) {
   {
      size_t left = ParserContext_saveTree(c);
      if (!pIdentifier(c)) {
         return 0;
      }
      ParserContext_linkTree(c,_Lname);
      ParserContext_backTree(c,left);
   }
   if (!p__(c)) {
      return 0;
   }
   return 1;
}
// ',' ~_ "..." ~_
static inline int e20(ParserContext * c) {
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
// { $name(Identifier) #Param } ~_
static inline int pFunctionParam(ParserContext * c) {
   ParserContext_beginTree(c,0);
   {
      size_t left = ParserContext_saveTree(c);
      if (!pIdentifier(c)) {
         return 0;
      }
      ParserContext_linkTree(c,_Lname);
      ParserContext_backTree(c,left);
   }
   ParserContext_endTree(c,0,_TParam,NULL, 0);
   if (!p__(c)) {
      return 0;
   }
   return 1;
}
// ',' ~_ $(FunctionParam)
static inline int e19(ParserContext * c) {
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
// $(FunctionParam) (',' ~_ $(FunctionParam))*
static inline int e18(ParserContext * c) {
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
      if (!e19(c)) {
         c->pos = pos;
         ParserContext_backTree(c,left2);
         ParserContext_backLog(c,log);
         break;
      }
   }
   return 1;
}
// { ($(FunctionParam) (',' ~_ $(FunctionParam))*)? (',' ~_ "..." ~_)? #List }
static inline int e17(ParserContext * c) {
   ParserContext_beginTree(c,0);
   const unsigned char * pos = c->pos;
   size_t left = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   // $(FunctionParam) (',' ~_ $(FunctionParam))*
   if (!e18(c)) {
      c->pos = pos;
      ParserContext_backTree(c,left);
      ParserContext_backLog(c,log);
   }
   const unsigned char * pos3 = c->pos;
   // ',' ~_ "..." ~_
   if (!e20(c)) {
      c->pos = pos3;
   }
   ParserContext_endTree(c,0,_TList,NULL, 0);
   return 1;
}
// { ($(FunctionParam) (',' ~_ $(FunctionParam))*)? (',' ~_ "..." ~_)? #List }
static inline int pFunctionParamList(ParserContext * c) {
   int memo = ParserContext_memoLookupTree(c,6);
   if (memo == 0) {
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e17(c)) {
         ParserContext_memoTreeSucc(c,6,pos);
         return 1;
      } else {
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         ParserContext_memoFail(c,6);
         return 0;
      }
   }
   return memo == 1;
}
// "function" { ![$0-9A-Z_a-z] ~_ $name(Identifier) ~_ '(' ~_ $list(FunctionParamList) ')' ~_ $body(Block) #FunctionDecl }
static inline int e9(ParserContext * c) {
   if (!ParserContext_match8(c,102,117,110,99,116,105,111,110)) {
      return 0;
   }
   ParserContext_beginTree(c,-8);
   if (_set4[ParserContext_prefetch(c)]) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!pIdentifier(c)) {
         return 0;
      }
      ParserContext_linkTree(c,_Lname);
      ParserContext_backTree(c,left);
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
      size_t left1 = ParserContext_saveTree(c);
      if (!pFunctionParamList(c)) {
         return 0;
      }
      ParserContext_linkTree(c,_Llist);
      ParserContext_backTree(c,left1);
   }
   if (ParserContext_read(c) != 41) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   {
      size_t left2 = ParserContext_saveTree(c);
      if (!pBlock(c)) {
         return 0;
      }
      ParserContext_linkTree(c,_Lbody);
      ParserContext_backTree(c,left2);
   }
   ParserContext_endTree(c,0,_TFunctionDecl,NULL, 0);
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
      if (!e75(c)) {
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         break;
      }
   }
   return 1;
}
// "&&" {$left ~_ $right(EqualityExpression) #And }
static inline int e78(ParserContext * c) {
   if (!ParserContext_match2(c,38,38)) {
      return 0;
   }
   ParserContext_foldTree(c,-2,_Lleft);
   if (!p__(c)) {
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
   ParserContext_endTree(c,0,_TAnd,NULL, 0);
   return 1;
}
// EqualityExpression ("&&" {$left ~_ $right(EqualityExpression) #And })*
static inline int pLogicalAndExpression(ParserContext * c) {
   if (!pEqualityExpression(c)) {
      return 0;
   }
   while (1) {
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      // "&&" {$left ~_ $right(EqualityExpression) #And }
      if (!e78(c)) {
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         break;
      }
   }
   return 1;
}
// "||" {$left ~_ $right(LogicalAndExpression) #Or }
static inline int e79(ParserContext * c) {
   if (!ParserContext_match2(c,124,124)) {
      return 0;
   }
   ParserContext_foldTree(c,-2,_Lleft);
   if (!p__(c)) {
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!pLogicalAndExpression(c)) {
         return 0;
      }
      ParserContext_linkTree(c,_Lright);
      ParserContext_backTree(c,left);
   }
   ParserContext_endTree(c,0,_TOr,NULL, 0);
   return 1;
}
// LogicalAndExpression ("||" {$left ~_ $right(LogicalAndExpression) #Or })*
static inline int pLogicalOrExpression(ParserContext * c) {
   if (!pLogicalAndExpression(c)) {
      return 0;
   }
   while (1) {
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      // "||" {$left ~_ $right(LogicalAndExpression) #Or }
      if (!e79(c)) {
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         break;
      }
   }
   return 1;
}
// ">>=" ~_ #AssignRightShift
static inline int e62(ParserContext * c) {
   if (!ParserContext_match3(c,62,62,61)) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   ParserContext_tagTree(c,_TAssignRightShift);
   return 1;
}
// "<<=" ~_ #AssignLeftShift
static inline int e61(ParserContext * c) {
   if (!ParserContext_match3(c,60,60,61)) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   ParserContext_tagTree(c,_TAssignLeftShift);
   return 1;
}
// PrimaryExpression ({$recv _FunctionCall })+ / "null" ![$0-9A-Z_a-z] { #Null } ~_ / { ![0-9] !(("else" ![$0-9A-Z_a-z] / "function" ![$0-9A-Z_a-z] / "if" ![$0-9A-Z_a-z] / "return" ![$0-9A-Z_a-z] / "var" ![$0-9A-Z_a-z]) ![$0-9A-Z_a-z]) [$0-9A-Z_a-z]+ #Name } ~_
static inline int e53(ParserContext * c) {
   int temp = 1;
   if (temp) {
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      // PrimaryExpression ({$recv _FunctionCall })+
      if (e32(c)) {
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
      // "null" ![$0-9A-Z_a-z] { #Null } ~_
      if (e40(c)) {
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
      // { ![0-9] !(("else" ![$0-9A-Z_a-z] / "function" ![$0-9A-Z_a-z] / "if" ![$0-9A-Z_a-z] / "return" ![$0-9A-Z_a-z] / "var" ![$0-9A-Z_a-z]) ![$0-9A-Z_a-z]) [$0-9A-Z_a-z]+ #Name } ~_
      if (e10(c)) {
         temp = 0;
      } else {
         c->pos = pos7;
         ParserContext_backTree(c,left8);
         ParserContext_backLog(c,log9);
      }
   }
   if (temp) {
      return 0;
   }
   return 1;
}
// "&=" ~_ #AssignBitwiseAnd
static inline int e64(ParserContext * c) {
   if (!ParserContext_match2(c,38,61)) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   ParserContext_tagTree(c,_TAssignBitwiseAnd);
   return 1;
}
// '!' { !'=' ~_ $expr(UnaryExpression) #Not }
static inline int e30(ParserContext * c) {
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
// ">>>=" ~_ #AssignLogicalRightShift
static inline int e63(ParserContext * c) {
   if (!ParserContext_match4(c,62,62,62,61)) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   ParserContext_tagTree(c,_TAssignLogicalRightShift);
   return 1;
}
// '=' !'=' ~_ #Assign
static inline int e55(ParserContext * c) {
   if (ParserContext_read(c) != 61) {
      return 0;
   }
   if (ParserContext_prefetch(c) == 61) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   ParserContext_tagTree(c,_TAssign);
   return 1;
}
// "|=" ~_ #AssignBitwiseOr
static inline int e66(ParserContext * c) {
   if (!ParserContext_match2(c,124,61)) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   ParserContext_tagTree(c,_TAssignBitwiseOr);
   return 1;
}
// "true" ![$0-9A-Z_a-z] { #True } ~_
static inline int e37(ParserContext * c) {
   if (!ParserContext_match4(c,116,114,117,101)) {
      return 0;
   }
   if (_set4[ParserContext_prefetch(c)]) {
      return 0;
   }
   ParserContext_beginTree(c,0);
   ParserContext_endTree(c,0,_TTrue,NULL, 0);
   if (!p__(c)) {
      return 0;
   }
   return 1;
}
// PrimaryExpression ({$recv _FunctionCall })+ / "true" ![$0-9A-Z_a-z] { #True } ~_ / { ![0-9] !(("else" ![$0-9A-Z_a-z] / "function" ![$0-9A-Z_a-z] / "if" ![$0-9A-Z_a-z] / "return" ![$0-9A-Z_a-z] / "var" ![$0-9A-Z_a-z]) ![$0-9A-Z_a-z]) [$0-9A-Z_a-z]+ #Name } ~_
static inline int e54(ParserContext * c) {
   int temp = 1;
   if (temp) {
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      // PrimaryExpression ({$recv _FunctionCall })+
      if (e32(c)) {
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
      if (e37(c)) {
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
      // { ![0-9] !(("else" ![$0-9A-Z_a-z] / "function" ![$0-9A-Z_a-z] / "if" ![$0-9A-Z_a-z] / "return" ![$0-9A-Z_a-z] / "var" ![$0-9A-Z_a-z]) ![$0-9A-Z_a-z]) [$0-9A-Z_a-z]+ #Name } ~_
      if (e10(c)) {
         temp = 0;
      } else {
         c->pos = pos7;
         ParserContext_backTree(c,left8);
         ParserContext_backLog(c,log9);
      }
   }
   if (temp) {
      return 0;
   }
   return 1;
}
// "^=" ~_ #AssignBitwiseXOr
static inline int e65(ParserContext * c) {
   if (!ParserContext_match2(c,94,61)) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   ParserContext_tagTree(c,_TAssignBitwiseXOr);
   return 1;
}
// PrimaryExpression ({$recv _FunctionCall })+ / { ![0-9] !(("else" ![$0-9A-Z_a-z] / "function" ![$0-9A-Z_a-z] / "if" ![$0-9A-Z_a-z] / "return" ![$0-9A-Z_a-z] / "var" ![$0-9A-Z_a-z]) ![$0-9A-Z_a-z]) [$0-9A-Z_a-z]+ #Name } ~_
static inline int e46(ParserContext * c) {
   int temp = 1;
   if (temp) {
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      // PrimaryExpression ({$recv _FunctionCall })+
      if (e32(c)) {
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
      // { ![0-9] !(("else" ![$0-9A-Z_a-z] / "function" ![$0-9A-Z_a-z] / "if" ![$0-9A-Z_a-z] / "return" ![$0-9A-Z_a-z] / "var" ![$0-9A-Z_a-z]) ![$0-9A-Z_a-z]) [$0-9A-Z_a-z]+ #Name } ~_
      if (e10(c)) {
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
// "/=" ~_ #AssignDiv
static inline int e57(ParserContext * c) {
   if (!ParserContext_match2(c,47,61)) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   ParserContext_tagTree(c,_TAssignDiv);
   return 1;
}
// "*=" ~_ #AssignMul
static inline int e56(ParserContext * c) {
   if (!ParserContext_match2(c,42,61)) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   ParserContext_tagTree(c,_TAssignMul);
   return 1;
}
// [1-9] [0-9]*
static inline int e35(ParserContext * c) {
   if (!(49 <= ParserContext_prefetch(c) && ParserContext_read(c) < 58)) {
      return 0;
   }
   while ((48 <= ParserContext_prefetch(c) && ParserContext_prefetch(c) < 58)) {
      ParserContext_move(c,1);
   }
   return 1;
}
// '0'
static inline int e36(ParserContext * c) {
   if (ParserContext_read(c) != 48) {
      return 0;
   }
   return 1;
}
// { ([1-9] [0-9]* / '0') #Integer } ~_
static inline int e34(ParserContext * c) {
   ParserContext_beginTree(c,0);
   {
      int temp = 1;
      if (temp) {
         const unsigned char * pos = c->pos;
         // [1-9] [0-9]*
         if (e35(c)) {
            temp = 0;
         } else {
            c->pos = pos;
         }
      }
      if (temp) {
         const unsigned char * pos2 = c->pos;
         // '0'
         if (e36(c)) {
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
// PrimaryExpression ({$recv _FunctionCall })+ / { ([1-9] [0-9]* / '0') #Integer } ~_
static inline int e48(ParserContext * c) {
   int temp = 1;
   if (temp) {
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      // PrimaryExpression ({$recv _FunctionCall })+
      if (e32(c)) {
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
      // { ([1-9] [0-9]* / '0') #Integer } ~_
      if (e34(c)) {
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
// "+=" ~_ #AssignAdd
static inline int e59(ParserContext * c) {
   if (!ParserContext_match2(c,43,61)) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   ParserContext_tagTree(c,_TAssignAdd);
   return 1;
}
// '(' ~_ Expression ')' ~_
static inline int e41(ParserContext * c) {
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
// PrimaryExpression ({$recv _FunctionCall })+ / '(' ~_ Expression ')' ~_
static inline int e47(ParserContext * c) {
   int temp = 1;
   if (temp) {
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      // PrimaryExpression ({$recv _FunctionCall })+
      if (e32(c)) {
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
      // '(' ~_ Expression ')' ~_
      if (e41(c)) {
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
// "%=" ~_ #AssignMod
static inline int e58(ParserContext * c) {
   if (!ParserContext_match2(c,37,61)) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   ParserContext_tagTree(c,_TAssignMod);
   return 1;
}
// "alse" ![$0-9A-Z_a-z] { #False } ~_
static inline int e51(ParserContext * c) {
   if (!ParserContext_match4(c,97,108,115,101)) {
      return 0;
   }
   if (_set4[ParserContext_prefetch(c)]) {
      return 0;
   }
   ParserContext_beginTree(c,0);
   ParserContext_endTree(c,0,_TFalse,NULL, 0);
   if (!p__(c)) {
      return 0;
   }
   return 1;
}
// "unction" { ![$0-9A-Z_a-z] ~_ ($name(Identifier) ~_)? '(' ~_ $list(FunctionParamList) ')' ~_ $body(Block) "::" ~_ #FunctionExpr }
static inline int e52(ParserContext * c) {
   if (!ParserContext_match7(c,117,110,99,116,105,111,110)) {
      return 0;
   }
   ParserContext_beginTree(c,-8);
   if (_set4[ParserContext_prefetch(c)]) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   const unsigned char * pos = c->pos;
   size_t left = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   // $name(Identifier) ~_
   if (!e43(c)) {
      c->pos = pos;
      ParserContext_backTree(c,left);
      ParserContext_backLog(c,log);
   }
   if (ParserContext_read(c) != 40) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   {
      size_t left3 = ParserContext_saveTree(c);
      if (!pFunctionParamList(c)) {
         return 0;
      }
      ParserContext_linkTree(c,_Llist);
      ParserContext_backTree(c,left3);
   }
   if (ParserContext_read(c) != 41) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   {
      size_t left4 = ParserContext_saveTree(c);
      if (!pBlock(c)) {
         return 0;
      }
      ParserContext_linkTree(c,_Lbody);
      ParserContext_backTree(c,left4);
   }
   if (!ParserContext_match2(c,58,58)) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   ParserContext_endTree(c,0,_TFunctionExpr,NULL, 0);
   return 1;
}
// 'f' ("alse" ![$0-9A-Z_a-z] { #False } ~_ / "unction" { ![$0-9A-Z_a-z] ~_ ($name(Identifier) ~_)? '(' ~_ $list(FunctionParamList) ')' ~_ $body(Block) "::" ~_ #FunctionExpr })
static inline int e50(ParserContext * c) {
   if (ParserContext_read(c) != 102) {
      return 0;
   }
   {
      int temp = 1;
      if (temp) {
         const unsigned char * pos = c->pos;
         size_t left = ParserContext_saveTree(c);
         size_t log = ParserContext_saveLog(c);
         // "alse" ![$0-9A-Z_a-z] { #False } ~_
         if (e51(c)) {
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
         // "unction" { ![$0-9A-Z_a-z] ~_ ($name(Identifier) ~_)? '(' ~_ $list(FunctionParamList) ')' ~_ $body(Block) "::" ~_ #FunctionExpr }
         if (e52(c)) {
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
   }
   return 1;
}
// PrimaryExpression ({$recv _FunctionCall })+ / 'f' ("alse" ![$0-9A-Z_a-z] { #False } ~_ / "unction" { ![$0-9A-Z_a-z] ~_ ($name(Identifier) ~_)? '(' ~_ $list(FunctionParamList) ')' ~_ $body(Block) "::" ~_ #FunctionExpr }) / { ![0-9] !(("else" ![$0-9A-Z_a-z] / "function" ![$0-9A-Z_a-z] / "if" ![$0-9A-Z_a-z] / "return" ![$0-9A-Z_a-z] / "var" ![$0-9A-Z_a-z]) ![$0-9A-Z_a-z]) [$0-9A-Z_a-z]+ #Name } ~_
static inline int e49(ParserContext * c) {
   int temp = 1;
   if (temp) {
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      // PrimaryExpression ({$recv _FunctionCall })+
      if (e32(c)) {
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
      // 'f' ("alse" ![$0-9A-Z_a-z] { #False } ~_ / "unction" { ![$0-9A-Z_a-z] ~_ ($name(Identifier) ~_)? '(' ~_ $list(FunctionParamList) ')' ~_ $body(Block) "::" ~_ #FunctionExpr })
      if (e50(c)) {
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
      // { ![0-9] !(("else" ![$0-9A-Z_a-z] / "function" ![$0-9A-Z_a-z] / "if" ![$0-9A-Z_a-z] / "return" ![$0-9A-Z_a-z] / "var" ![$0-9A-Z_a-z]) ![$0-9A-Z_a-z]) [$0-9A-Z_a-z]+ #Name } ~_
      if (e10(c)) {
         temp = 0;
      } else {
         c->pos = pos7;
         ParserContext_backTree(c,left8);
         ParserContext_backLog(c,log9);
      }
   }
   if (temp) {
      return 0;
   }
   return 1;
}
// "-=" ~_ #AssignSub
static inline int e60(ParserContext * c) {
   if (!ParserContext_match2(c,45,61)) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   ParserContext_tagTree(c,_TAssignSub);
   return 1;
}
// { $left(UnaryExpression) ('=' !'=' ~_ #Assign / "*=" ~_ #AssignMul / "/=" ~_ #AssignDiv / "%=" ~_ #AssignMod / "+=" ~_ #AssignAdd / "-=" ~_ #AssignSub / "<<=" ~_ #AssignLeftShift / ">>=" ~_ #AssignRightShift / ">>>=" ~_ #AssignLogicalRightShift / "&=" ~_ #AssignBitwiseAnd / "^=" ~_ #AssignBitwiseXOr / "|=" ~_ #AssignBitwiseOr) $right(AssignmentExpression) }
static inline int e29(ParserContext * c) {
   ParserContext_beginTree(c,0);
   {
      size_t left = ParserContext_saveTree(c);
      if (!pUnaryExpression(c)) {
         return 0;
      }
      ParserContext_linkTree(c,_Lleft);
      ParserContext_backTree(c,left);
   }
   {
      int temp = 1;
      if (temp) {
         const unsigned char * pos = c->pos;
         size_t left3 = ParserContext_saveTree(c);
         size_t log = ParserContext_saveLog(c);
         // '=' !'=' ~_ #Assign
         if (e55(c)) {
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
         // "*=" ~_ #AssignMul
         if (e56(c)) {
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
         // "/=" ~_ #AssignDiv
         if (e57(c)) {
            temp = 0;
         } else {
            c->pos = pos8;
            ParserContext_backTree(c,left9);
            ParserContext_backLog(c,log10);
         }
      }
      if (temp) {
         const unsigned char * pos11 = c->pos;
         size_t left12 = ParserContext_saveTree(c);
         size_t log13 = ParserContext_saveLog(c);
         // "%=" ~_ #AssignMod
         if (e58(c)) {
            temp = 0;
         } else {
            c->pos = pos11;
            ParserContext_backTree(c,left12);
            ParserContext_backLog(c,log13);
         }
      }
      if (temp) {
         const unsigned char * pos14 = c->pos;
         size_t left15 = ParserContext_saveTree(c);
         size_t log16 = ParserContext_saveLog(c);
         // "+=" ~_ #AssignAdd
         if (e59(c)) {
            temp = 0;
         } else {
            c->pos = pos14;
            ParserContext_backTree(c,left15);
            ParserContext_backLog(c,log16);
         }
      }
      if (temp) {
         const unsigned char * pos17 = c->pos;
         size_t left18 = ParserContext_saveTree(c);
         size_t log19 = ParserContext_saveLog(c);
         // "-=" ~_ #AssignSub
         if (e60(c)) {
            temp = 0;
         } else {
            c->pos = pos17;
            ParserContext_backTree(c,left18);
            ParserContext_backLog(c,log19);
         }
      }
      if (temp) {
         const unsigned char * pos20 = c->pos;
         size_t left21 = ParserContext_saveTree(c);
         size_t log22 = ParserContext_saveLog(c);
         // "<<=" ~_ #AssignLeftShift
         if (e61(c)) {
            temp = 0;
         } else {
            c->pos = pos20;
            ParserContext_backTree(c,left21);
            ParserContext_backLog(c,log22);
         }
      }
      if (temp) {
         const unsigned char * pos23 = c->pos;
         size_t left24 = ParserContext_saveTree(c);
         size_t log25 = ParserContext_saveLog(c);
         // ">>=" ~_ #AssignRightShift
         if (e62(c)) {
            temp = 0;
         } else {
            c->pos = pos23;
            ParserContext_backTree(c,left24);
            ParserContext_backLog(c,log25);
         }
      }
      if (temp) {
         const unsigned char * pos26 = c->pos;
         size_t left27 = ParserContext_saveTree(c);
         size_t log28 = ParserContext_saveLog(c);
         // ">>>=" ~_ #AssignLogicalRightShift
         if (e63(c)) {
            temp = 0;
         } else {
            c->pos = pos26;
            ParserContext_backTree(c,left27);
            ParserContext_backLog(c,log28);
         }
      }
      if (temp) {
         const unsigned char * pos29 = c->pos;
         size_t left30 = ParserContext_saveTree(c);
         size_t log31 = ParserContext_saveLog(c);
         // "&=" ~_ #AssignBitwiseAnd
         if (e64(c)) {
            temp = 0;
         } else {
            c->pos = pos29;
            ParserContext_backTree(c,left30);
            ParserContext_backLog(c,log31);
         }
      }
      if (temp) {
         const unsigned char * pos32 = c->pos;
         size_t left33 = ParserContext_saveTree(c);
         size_t log34 = ParserContext_saveLog(c);
         // "^=" ~_ #AssignBitwiseXOr
         if (e65(c)) {
            temp = 0;
         } else {
            c->pos = pos32;
            ParserContext_backTree(c,left33);
            ParserContext_backLog(c,log34);
         }
      }
      if (temp) {
         const unsigned char * pos35 = c->pos;
         size_t left36 = ParserContext_saveTree(c);
         size_t log37 = ParserContext_saveLog(c);
         // "|=" ~_ #AssignBitwiseOr
         if (e66(c)) {
            temp = 0;
         } else {
            c->pos = pos35;
            ParserContext_backTree(c,left36);
            ParserContext_backLog(c,log37);
         }
      }
      if (temp) {
         return 0;
      }
   }
   {
      size_t left38 = ParserContext_saveTree(c);
      if (!pAssignmentExpression(c)) {
         return 0;
      }
      ParserContext_linkTree(c,_Lright);
      ParserContext_backTree(c,left38);
   }
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
// ',' {$ ~_ $(AssignmentExpression) #Expression }
static inline int e81(ParserContext * c) {
   if (ParserContext_read(c) != 44) {
      return 0;
   }
   ParserContext_foldTree(c,-1,_L);
   if (!p__(c)) {
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!pAssignmentExpression(c)) {
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
   ParserContext_endTree(c,0,_TExpression,NULL, 0);
   return 1;
}
// AssignmentExpression (',' {$ ~_ $(AssignmentExpression) #Expression })*
static inline int e27(ParserContext * c) {
   if (!pAssignmentExpression(c)) {
      return 0;
   }
   while (1) {
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      // ',' {$ ~_ $(AssignmentExpression) #Expression }
      if (!e81(c)) {
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         break;
      }
   }
   return 1;
}
// AssignmentExpression (',' {$ ~_ $(AssignmentExpression) #Expression })*
int pExpression(ParserContext * c) {
   int memo = ParserContext_memoLookupTree(c,2);
   if (memo == 0) {
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e27(c)) {
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
// '?' {$cond ~_ $then(Expression) ':' !'>' ~_ $else(LogicalOrExpression) #Conditional }
static inline int e80(ParserContext * c) {
   if (ParserContext_read(c) != 63) {
      return 0;
   }
   ParserContext_foldTree(c,-1,_Lcond);
   if (!p__(c)) {
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!pExpression(c)) {
         return 0;
      }
      ParserContext_linkTree(c,_Lthen);
      ParserContext_backTree(c,left);
   }
   if (ParserContext_read(c) != 58) {
      return 0;
   }
   if (ParserContext_prefetch(c) == 62) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   {
      size_t left1 = ParserContext_saveTree(c);
      if (!pLogicalOrExpression(c)) {
         return 0;
      }
      ParserContext_linkTree(c,_Lelse);
      ParserContext_backTree(c,left1);
   }
   ParserContext_endTree(c,0,_TConditional,NULL, 0);
   return 1;
}
// LogicalOrExpression ('?' {$cond ~_ $then(Expression) ':' !'>' ~_ $else(LogicalOrExpression) #Conditional })*
int e67(ParserContext * c) {
   if (!pLogicalOrExpression(c)) {
      return 0;
   }
   while (1) {
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      // '?' {$cond ~_ $then(Expression) ':' !'>' ~_ $else(LogicalOrExpression) #Conditional }
      if (!e80(c)) {
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         break;
      }
   }
   return 1;
}
// ({ $left(UnaryExpression) ('=' !'=' ~_ #Assign / "*=" ~_ #AssignMul / "/=" ~_ #AssignDiv / "%=" ~_ #AssignMod / "+=" ~_ #AssignAdd / "-=" ~_ #AssignSub / "<<=" ~_ #AssignLeftShift / ">>=" ~_ #AssignRightShift / ">>>=" ~_ #AssignLogicalRightShift / "&=" ~_ #AssignBitwiseAnd / "^=" ~_ #AssignBitwiseXOr / "|=" ~_ #AssignBitwiseOr) $right(AssignmentExpression) } / LogicalOrExpression ('?' {$cond ~_ $then(Expression) ':' !'>' ~_ $else(LogicalOrExpression) #Conditional })*)
static inline int e28(ParserContext * c) {
   int temp = 1;
   if (temp) {
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      // { $left(UnaryExpression) ('=' !'=' ~_ #Assign / "*=" ~_ #AssignMul / "/=" ~_ #AssignDiv / "%=" ~_ #AssignMod / "+=" ~_ #AssignAdd / "-=" ~_ #AssignSub / "<<=" ~_ #AssignLeftShift / ">>=" ~_ #AssignRightShift / ">>>=" ~_ #AssignLogicalRightShift / "&=" ~_ #AssignBitwiseAnd / "^=" ~_ #AssignBitwiseXOr / "|=" ~_ #AssignBitwiseOr) $right(AssignmentExpression) }
      if (e29(c)) {
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
      // LogicalOrExpression ('?' {$cond ~_ $then(Expression) ':' !'>' ~_ $else(LogicalOrExpression) #Conditional })*
      if (e67(c)) {
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
// ({ $left(UnaryExpression) ('=' !'=' ~_ #Assign / "*=" ~_ #AssignMul / "/=" ~_ #AssignDiv / "%=" ~_ #AssignMod / "+=" ~_ #AssignAdd / "-=" ~_ #AssignSub / "<<=" ~_ #AssignLeftShift / ">>=" ~_ #AssignRightShift / ">>>=" ~_ #AssignLogicalRightShift / "&=" ~_ #AssignBitwiseAnd / "^=" ~_ #AssignBitwiseXOr / "|=" ~_ #AssignBitwiseOr) $right(AssignmentExpression) } / LogicalOrExpression ('?' {$cond ~_ $then(Expression) ':' !'>' ~_ $else(LogicalOrExpression) #Conditional })*)
int pAssignmentExpression(ParserContext * c) {
   int memo = ParserContext_memoLookupTree(c,5);
   if (memo == 0) {
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e28(c)) {
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
// '=' !'=' ~_ $expr(AssignmentExpression)
static inline int e105(ParserContext * c) {
   if (ParserContext_read(c) != 61) {
      return 0;
   }
   if (ParserContext_prefetch(c) == 61) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!pAssignmentExpression(c)) {
         return 0;
      }
      ParserContext_linkTree(c,_Lexpr);
      ParserContext_backTree(c,left);
   }
   return 1;
}
// { $name(Identifier) ('=' !'=' ~_ $expr(AssignmentExpression))? #VarDecl } ~_
static inline int pInitDecl(ParserContext * c) {
   ParserContext_beginTree(c,0);
   {
      size_t left = ParserContext_saveTree(c);
      if (!pIdentifier(c)) {
         return 0;
      }
      ParserContext_linkTree(c,_Lname);
      ParserContext_backTree(c,left);
   }
   const unsigned char * pos = c->pos;
   size_t left2 = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   // '=' !'=' ~_ $expr(AssignmentExpression)
   if (!e105(c)) {
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
// ',' ~_ $(InitDecl)
static inline int e106(ParserContext * c) {
   if (ParserContext_read(c) != 44) {
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
// { $(InitDecl) (',' ~_ $(InitDecl))* #VarList }
static inline int pVariableList(ParserContext * c) {
   ParserContext_beginTree(c,0);
   {
      size_t left = ParserContext_saveTree(c);
      if (!pInitDecl(c)) {
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
   while (1) {
      const unsigned char * pos = c->pos;
      size_t left2 = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      // ',' ~_ $(InitDecl)
      if (!e106(c)) {
         c->pos = pos;
         ParserContext_backTree(c,left2);
         ParserContext_backLog(c,log);
         break;
      }
   }
   ParserContext_endTree(c,0,_TVarList,NULL, 0);
   return 1;
}
// "var" { ![$0-9A-Z_a-z] ~_ $list(VariableList) ';' ~_ #Declaration }
static inline int e104(ParserContext * c) {
   if (!ParserContext_match3(c,118,97,114)) {
      return 0;
   }
   ParserContext_beginTree(c,-3);
   if (_set4[ParserContext_prefetch(c)]) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!pVariableList(c)) {
         return 0;
      }
      ParserContext_linkTree(c,_Llist);
      ParserContext_backTree(c,left);
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
// "function" { ![$0-9A-Z_a-z] ~_ $name(Identifier) ~_ '(' ~_ $list(FunctionParamList) ')' ~_ $body(Block) #FunctionDecl } / "var" { ![$0-9A-Z_a-z] ~_ $list(VariableList) ';' ~_ #Declaration }
static inline int pDeclaration(ParserContext * c) {
   int temp = 1;
   switch(_index14[ParserContext_prefetch(c)]) {
      case 0: 
      return 0;
      case 1: 
      // "function" { ![$0-9A-Z_a-z] ~_ $name(Identifier) ~_ '(' ~_ $list(FunctionParamList) ')' ~_ $body(Block) #FunctionDecl }
      temp = e9(c);
      break;
      case 2: 
      // "var" { ![$0-9A-Z_a-z] ~_ $list(VariableList) ';' ~_ #Declaration }
      temp = e104(c);
      break;
   }
   if (!temp) {
      return 0;
   }
   return 1;
}
// $(Declaration)
static inline int e103(ParserContext * c) {
   size_t left = ParserContext_saveTree(c);
   if (!pDeclaration(c)) {
      return 0;
   }
   ParserContext_linkTree(c,_L);
   ParserContext_backTree(c,left);
   return 1;
}
// "else" ![$0-9A-Z_a-z] ~_ $else(Block)
static inline int e86(ParserContext * c) {
   if (!ParserContext_match4(c,101,108,115,101)) {
      return 0;
   }
   if (_set4[ParserContext_prefetch(c)]) {
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
// '9' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If9 }
static inline int e95(ParserContext * c) {
   if (ParserContext_read(c) != 57) {
      return 0;
   }
   ParserContext_beginTree(c,-3);
   if (_set4[ParserContext_prefetch(c)]) {
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
   if (!e86(c)) {
      c->pos = pos;
      ParserContext_backTree(c,left3);
      ParserContext_backLog(c,log);
   }
   ParserContext_endTree(c,0,_TIf9,NULL, 0);
   return 1;
}
// '8' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If8 }
static inline int e94(ParserContext * c) {
   if (ParserContext_read(c) != 56) {
      return 0;
   }
   ParserContext_beginTree(c,-3);
   if (_set4[ParserContext_prefetch(c)]) {
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
   if (!e86(c)) {
      c->pos = pos;
      ParserContext_backTree(c,left3);
      ParserContext_backLog(c,log);
   }
   ParserContext_endTree(c,0,_TIf8,NULL, 0);
   return 1;
}
// 'B' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #IfB }
static inline int e97(ParserContext * c) {
   if (ParserContext_read(c) != 66) {
      return 0;
   }
   ParserContext_beginTree(c,-3);
   if (_set4[ParserContext_prefetch(c)]) {
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
   if (!e86(c)) {
      c->pos = pos;
      ParserContext_backTree(c,left3);
      ParserContext_backLog(c,log);
   }
   ParserContext_endTree(c,0,_TIfB,NULL, 0);
   return 1;
}
// { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If }
static inline int e85(ParserContext * c) {
   ParserContext_beginTree(c,-2);
   if (_set4[ParserContext_prefetch(c)]) {
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
   if (!e86(c)) {
      c->pos = pos;
      ParserContext_backTree(c,left3);
      ParserContext_backLog(c,log);
   }
   ParserContext_endTree(c,0,_TIf,NULL, 0);
   return 1;
}
// 'A' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #IfA }
static inline int e96(ParserContext * c) {
   if (ParserContext_read(c) != 65) {
      return 0;
   }
   ParserContext_beginTree(c,-3);
   if (_set4[ParserContext_prefetch(c)]) {
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
   if (!e86(c)) {
      c->pos = pos;
      ParserContext_backTree(c,left3);
      ParserContext_backLog(c,log);
   }
   ParserContext_endTree(c,0,_TIfA,NULL, 0);
   return 1;
}
// '2' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If2 }
static inline int e88(ParserContext * c) {
   if (ParserContext_read(c) != 50) {
      return 0;
   }
   ParserContext_beginTree(c,-3);
   if (_set4[ParserContext_prefetch(c)]) {
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
   if (!e86(c)) {
      c->pos = pos;
      ParserContext_backTree(c,left3);
      ParserContext_backLog(c,log);
   }
   ParserContext_endTree(c,0,_TIf2,NULL, 0);
   return 1;
}
// '1' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If1 }
static inline int e87(ParserContext * c) {
   if (ParserContext_read(c) != 49) {
      return 0;
   }
   ParserContext_beginTree(c,-3);
   if (_set4[ParserContext_prefetch(c)]) {
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
   if (!e86(c)) {
      c->pos = pos;
      ParserContext_backTree(c,left3);
      ParserContext_backLog(c,log);
   }
   ParserContext_endTree(c,0,_TIf1,NULL, 0);
   return 1;
}
// '3' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If3 }
static inline int e89(ParserContext * c) {
   if (ParserContext_read(c) != 51) {
      return 0;
   }
   ParserContext_beginTree(c,-3);
   if (_set4[ParserContext_prefetch(c)]) {
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
   if (!e86(c)) {
      c->pos = pos;
      ParserContext_backTree(c,left3);
      ParserContext_backLog(c,log);
   }
   ParserContext_endTree(c,0,_TIf3,NULL, 0);
   return 1;
}
// '5' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If5 }
static inline int e91(ParserContext * c) {
   if (ParserContext_read(c) != 53) {
      return 0;
   }
   ParserContext_beginTree(c,-3);
   if (_set4[ParserContext_prefetch(c)]) {
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
   if (!e86(c)) {
      c->pos = pos;
      ParserContext_backTree(c,left3);
      ParserContext_backLog(c,log);
   }
   ParserContext_endTree(c,0,_TIf5,NULL, 0);
   return 1;
}
// '4' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If4 }
static inline int e90(ParserContext * c) {
   if (ParserContext_read(c) != 52) {
      return 0;
   }
   ParserContext_beginTree(c,-3);
   if (_set4[ParserContext_prefetch(c)]) {
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
   if (!e86(c)) {
      c->pos = pos;
      ParserContext_backTree(c,left3);
      ParserContext_backLog(c,log);
   }
   ParserContext_endTree(c,0,_TIf4,NULL, 0);
   return 1;
}
// '7' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If7 }
static inline int e93(ParserContext * c) {
   if (ParserContext_read(c) != 55) {
      return 0;
   }
   ParserContext_beginTree(c,-3);
   if (_set4[ParserContext_prefetch(c)]) {
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
   if (!e86(c)) {
      c->pos = pos;
      ParserContext_backTree(c,left3);
      ParserContext_backLog(c,log);
   }
   ParserContext_endTree(c,0,_TIf7,NULL, 0);
   return 1;
}
// '6' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If6 }
static inline int e92(ParserContext * c) {
   if (ParserContext_read(c) != 54) {
      return 0;
   }
   ParserContext_beginTree(c,-3);
   if (_set4[ParserContext_prefetch(c)]) {
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
   if (!e86(c)) {
      c->pos = pos;
      ParserContext_backTree(c,left3);
      ParserContext_backLog(c,log);
   }
   ParserContext_endTree(c,0,_TIf6,NULL, 0);
   return 1;
}
// "if" ({ ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If } / '1' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If1 } / '2' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If2 } / '3' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If3 } / '4' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If4 } / '5' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If5 } / '6' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If6 } / '7' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If7 } / '8' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If8 } / '9' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If9 } / 'A' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #IfA } / 'B' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #IfB })
static inline int e84(ParserContext * c) {
   if (!ParserContext_match2(c,105,102)) {
      return 0;
   }
   {
      int temp = 1;
      if (temp) {
         const unsigned char * pos = c->pos;
         size_t left = ParserContext_saveTree(c);
         size_t log = ParserContext_saveLog(c);
         // { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If }
         if (e85(c)) {
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
         // '1' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If1 }
         if (e87(c)) {
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
         // '2' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If2 }
         if (e88(c)) {
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
         // '3' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If3 }
         if (e89(c)) {
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
         // '4' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If4 }
         if (e90(c)) {
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
         // '5' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If5 }
         if (e91(c)) {
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
         // '6' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If6 }
         if (e92(c)) {
            temp = 0;
         } else {
            c->pos = pos19;
            ParserContext_backTree(c,left20);
            ParserContext_backLog(c,log21);
         }
      }
      if (temp) {
         const unsigned char * pos22 = c->pos;
         size_t left23 = ParserContext_saveTree(c);
         size_t log24 = ParserContext_saveLog(c);
         // '7' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If7 }
         if (e93(c)) {
            temp = 0;
         } else {
            c->pos = pos22;
            ParserContext_backTree(c,left23);
            ParserContext_backLog(c,log24);
         }
      }
      if (temp) {
         const unsigned char * pos25 = c->pos;
         size_t left26 = ParserContext_saveTree(c);
         size_t log27 = ParserContext_saveLog(c);
         // '8' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If8 }
         if (e94(c)) {
            temp = 0;
         } else {
            c->pos = pos25;
            ParserContext_backTree(c,left26);
            ParserContext_backLog(c,log27);
         }
      }
      if (temp) {
         const unsigned char * pos28 = c->pos;
         size_t left29 = ParserContext_saveTree(c);
         size_t log30 = ParserContext_saveLog(c);
         // '9' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If9 }
         if (e95(c)) {
            temp = 0;
         } else {
            c->pos = pos28;
            ParserContext_backTree(c,left29);
            ParserContext_backLog(c,log30);
         }
      }
      if (temp) {
         const unsigned char * pos31 = c->pos;
         size_t left32 = ParserContext_saveTree(c);
         size_t log33 = ParserContext_saveLog(c);
         // 'A' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #IfA }
         if (e96(c)) {
            temp = 0;
         } else {
            c->pos = pos31;
            ParserContext_backTree(c,left32);
            ParserContext_backLog(c,log33);
         }
      }
      if (temp) {
         const unsigned char * pos34 = c->pos;
         size_t left35 = ParserContext_saveTree(c);
         size_t log36 = ParserContext_saveLog(c);
         // 'B' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #IfB }
         if (e97(c)) {
            temp = 0;
         } else {
            c->pos = pos34;
            ParserContext_backTree(c,left35);
            ParserContext_backLog(c,log36);
         }
      }
      if (temp) {
         return 0;
      }
   }
   return 1;
}
// { $expr(Expression) ';' ~_ #ExpressionStatement }
static inline int e26(ParserContext * c) {
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
// "if" ({ ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If } / '1' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If1 } / '2' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If2 } / '3' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If3 } / '4' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If4 } / '5' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If5 } / '6' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If6 } / '7' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If7 } / '8' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If8 } / '9' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If9 } / 'A' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #IfA } / 'B' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #IfB }) / { $expr(Expression) ';' ~_ #ExpressionStatement }
static inline int e83(ParserContext * c) {
   int temp = 1;
   if (temp) {
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      // "if" ({ ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If } / '1' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If1 } / '2' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If2 } / '3' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If3 } / '4' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If4 } / '5' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If5 } / '6' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If6 } / '7' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If7 } / '8' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If8 } / '9' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If9 } / 'A' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #IfA } / 'B' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #IfB })
      if (e84(c)) {
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
      if (e26(c)) {
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
static inline int e101(ParserContext * c) {
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
      if (!e22(c)) {
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
// $expr(Expression)
static inline int e100(ParserContext * c) {
   size_t left = ParserContext_saveTree(c);
   if (!pExpression(c)) {
      return 0;
   }
   ParserContext_linkTree(c,_Lexpr);
   ParserContext_backTree(c,left);
   return 1;
}
// "return" { ![$0-9A-Z_a-z] ~_ ($expr(Expression))? ';' ~_ #Return }
static inline int e99(ParserContext * c) {
   if (!ParserContext_match6(c,114,101,116,117,114,110)) {
      return 0;
   }
   ParserContext_beginTree(c,-6);
   if (_set4[ParserContext_prefetch(c)]) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   const unsigned char * pos = c->pos;
   size_t left = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   // $expr(Expression)
   if (!e100(c)) {
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
static inline int e98(ParserContext * c) {
   int temp = 1;
   if (temp) {
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      // "return" { ![$0-9A-Z_a-z] ~_ ($expr(Expression))? ';' ~_ #Return }
      if (e99(c)) {
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
      if (e26(c)) {
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
// { #Empty } ';' ~_
static inline int e82(ParserContext * c) {
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
// { $expr(Expression) ';' ~_ #ExpressionStatement } / { #Empty } ';' ~_ / ("if" ({ ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If } / '1' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If1 } / '2' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If2 } / '3' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If3 } / '4' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If4 } / '5' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If5 } / '6' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If6 } / '7' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If7 } / '8' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If8 } / '9' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If9 } / 'A' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #IfA } / 'B' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #IfB }) / { $expr(Expression) ';' ~_ #ExpressionStatement }) / ("return" { ![$0-9A-Z_a-z] ~_ ($expr(Expression))? ';' ~_ #Return } / { $expr(Expression) ';' ~_ #ExpressionStatement }) / { '{' ~_ (([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))* / ($(Statement) / ([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))*) / ($(Statement) / $(Declaration) / ([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))*))* '}' ~_ #Block #Block }
static inline int e25(ParserContext * c) {
   int temp = 1;
   switch(_index7[ParserContext_prefetch(c)]) {
      case 0: 
      return 0;
      case 1: 
      // { $expr(Expression) ';' ~_ #ExpressionStatement }
      temp = e26(c);
      break;
      case 2: 
      // { #Empty } ';' ~_
      temp = e82(c);
      break;
      case 3: 
      // "if" ({ ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If } / '1' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If1 } / '2' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If2 } / '3' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If3 } / '4' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If4 } / '5' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If5 } / '6' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If6 } / '7' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If7 } / '8' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If8 } / '9' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If9 } / 'A' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #IfA } / 'B' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #IfB }) / { $expr(Expression) ';' ~_ #ExpressionStatement }
      temp = e83(c);
      break;
      case 4: 
      // "return" { ![$0-9A-Z_a-z] ~_ ($expr(Expression))? ';' ~_ #Return } / { $expr(Expression) ';' ~_ #ExpressionStatement }
      temp = e98(c);
      break;
      case 5: 
      // { '{' ~_ (([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))* / ($(Statement) / ([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))*) / ($(Statement) / $(Declaration) / ([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))*))* '}' ~_ #Block #Block }
      temp = e101(c);
      break;
   }
   if (!temp) {
      return 0;
   }
   return 1;
}
// { $expr(Expression) ';' ~_ #ExpressionStatement } / { #Empty } ';' ~_ / ("if" ({ ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If } / '1' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If1 } / '2' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If2 } / '3' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If3 } / '4' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If4 } / '5' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If5 } / '6' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If6 } / '7' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If7 } / '8' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If8 } / '9' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #If9 } / 'A' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #IfA } / 'B' { ![$0-9A-Z_a-z] ~_ '(' ~_ $cond(Expression) ')' ~_ $then(Block) ("else" ![$0-9A-Z_a-z] ~_ $else(Block))? #IfB }) / { $expr(Expression) ';' ~_ #ExpressionStatement }) / ("return" { ![$0-9A-Z_a-z] ~_ ($expr(Expression))? ';' ~_ #Return } / { $expr(Expression) ';' ~_ #ExpressionStatement }) / { '{' ~_ (([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))* / ($(Statement) / ([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))*) / ($(Statement) / $(Declaration) / ([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))*))* '}' ~_ #Block #Block }
static inline int pStatement(ParserContext * c) {
   int memo = ParserContext_memoLookupTree(c,7);
   if (memo == 0) {
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e25(c)) {
         ParserContext_memoTreeSucc(c,7,pos);
         return 1;
      } else {
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         ParserContext_memoFail(c,7);
         return 0;
      }
   }
   return memo == 1;
}
// $(Statement)
static inline int e24(ParserContext * c) {
   size_t left = ParserContext_saveTree(c);
   if (!pStatement(c)) {
      return 0;
   }
   ParserContext_linkTree(c,_L);
   ParserContext_backTree(c,left);
   return 1;
}
// $(Statement) / $(Declaration) / ([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))*
static inline int e102(ParserContext * c) {
   int temp = 1;
   if (temp) {
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      // $(Statement)
      if (e24(c)) {
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
      if (e103(c)) {
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
// $(Statement) / ([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))*
static inline int e23(ParserContext * c) {
   int temp = 1;
   if (temp) {
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      // $(Statement)
      if (e24(c)) {
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
// ([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))* / ($(Statement) / ([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))*) / ($(Statement) / $(Declaration) / ([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))*)
int e22(ParserContext * c) {
   int temp = 1;
   switch(_index6[ParserContext_prefetch(c)]) {
      case 0: 
      return 0;
      case 1: 
      // ([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))*
      temp = e0(c);
      break;
      case 2: 
      // $(Statement) / ([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))*
      temp = e23(c);
      break;
      case 3: 
      // $(Statement) / $(Declaration) / ([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))*
      temp = e102(c);
      break;
   }
   if (!temp) {
      return 0;
   }
   return 1;
}
// '{' { ~_ (([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))* / ($(Statement) / ([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))*) / ($(Statement) / $(Declaration) / ([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))*))* '}' ~_ #Block }
static inline int e21(ParserContext * c) {
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
      if (!e22(c)) {
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
// '{' { ~_ (([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))* / ($(Statement) / ([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))*) / ($(Statement) / $(Declaration) / ([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))*))* '}' ~_ #Block }
int pBlock(ParserContext * c) {
   int memo = ParserContext_memoLookupTree(c,1);
   if (memo == 0) {
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e21(c)) {
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
// "function" { ![$0-9A-Z_a-z] ~_ ($name(Identifier) ~_)? '(' ~_ $list(FunctionParamList) ')' ~_ $body(Block) "::" ~_ #FunctionExpr }
static inline int e42(ParserContext * c) {
   if (!ParserContext_match8(c,102,117,110,99,116,105,111,110)) {
      return 0;
   }
   ParserContext_beginTree(c,-8);
   if (_set4[ParserContext_prefetch(c)]) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   const unsigned char * pos = c->pos;
   size_t left = ParserContext_saveTree(c);
   size_t log = ParserContext_saveLog(c);
   // $name(Identifier) ~_
   if (!e43(c)) {
      c->pos = pos;
      ParserContext_backTree(c,left);
      ParserContext_backLog(c,log);
   }
   if (ParserContext_read(c) != 40) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   {
      size_t left3 = ParserContext_saveTree(c);
      if (!pFunctionParamList(c)) {
         return 0;
      }
      ParserContext_linkTree(c,_Llist);
      ParserContext_backTree(c,left3);
   }
   if (ParserContext_read(c) != 41) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   {
      size_t left4 = ParserContext_saveTree(c);
      if (!pBlock(c)) {
         return 0;
      }
      ParserContext_linkTree(c,_Lbody);
      ParserContext_backTree(c,left4);
   }
   if (!ParserContext_match2(c,58,58)) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   ParserContext_endTree(c,0,_TFunctionExpr,NULL, 0);
   return 1;
}
// ',' ~_ $(AssignmentExpression)
static inline int e45(ParserContext * c) {
   if (ParserContext_read(c) != 44) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   {
      size_t left = ParserContext_saveTree(c);
      if (!pAssignmentExpression(c)) {
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
   return 1;
}
// $(AssignmentExpression) (',' ~_ $(AssignmentExpression))*
static inline int p_ArgumentExpressionList(ParserContext * c) {
   {
      size_t left = ParserContext_saveTree(c);
      if (!pAssignmentExpression(c)) {
         return 0;
      }
      ParserContext_linkTree(c,_L);
      ParserContext_backTree(c,left);
   }
   while (1) {
      const unsigned char * pos = c->pos;
      size_t left2 = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      // ',' ~_ $(AssignmentExpression)
      if (!e45(c)) {
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
static inline int e44(ParserContext * c) {
   ParserContext_foldTree(c,0,_Lrecv);
   if (!p_FunctionCall(c)) {
      return 0;
   }
   ParserContext_endTree(c,0,_T,NULL, 0);
   return 1;
}
// '"' { [\x01-\t\x0b-!#-[\]-\xff]* #String } '"' ~_
static inline int e39(ParserContext * c) {
   if (ParserContext_read(c) != 34) {
      return 0;
   }
   ParserContext_beginTree(c,0);
   while (_set10[ParserContext_prefetch(c)]) {
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
static inline int e38(ParserContext * c) {
   if (!ParserContext_match5(c,102,97,108,115,101)) {
      return 0;
   }
   if (_set4[ParserContext_prefetch(c)]) {
      return 0;
   }
   ParserContext_beginTree(c,0);
   ParserContext_endTree(c,0,_TFalse,NULL, 0);
   if (!p__(c)) {
      return 0;
   }
   return 1;
}
// { ([1-9] [0-9]* / '0') #Integer } ~_ / "true" ![$0-9A-Z_a-z] { #True } ~_ / "false" ![$0-9A-Z_a-z] { #False } ~_ / '"' { [\x01-\t\x0b-!#-[\]-\xff]* #String } '"' ~_ / "null" ![$0-9A-Z_a-z] { #Null } ~_ / '(' ~_ Expression ')' ~_ / "function" { ![$0-9A-Z_a-z] ~_ ($name(Identifier) ~_)? '(' ~_ $list(FunctionParamList) ')' ~_ $body(Block) "::" ~_ #FunctionExpr } / { ![0-9] !(("else" ![$0-9A-Z_a-z] / "function" ![$0-9A-Z_a-z] / "if" ![$0-9A-Z_a-z] / "return" ![$0-9A-Z_a-z] / "var" ![$0-9A-Z_a-z]) ![$0-9A-Z_a-z]) [$0-9A-Z_a-z]+ #Name } ~_
static inline int e33(ParserContext * c) {
   int temp = 1;
   if (temp) {
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      // { ([1-9] [0-9]* / '0') #Integer } ~_
      if (e34(c)) {
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
      if (e37(c)) {
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
      if (e38(c)) {
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
      if (e39(c)) {
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
      if (e40(c)) {
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
      if (e41(c)) {
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
      // "function" { ![$0-9A-Z_a-z] ~_ ($name(Identifier) ~_)? '(' ~_ $list(FunctionParamList) ')' ~_ $body(Block) "::" ~_ #FunctionExpr }
      if (e42(c)) {
         temp = 0;
      } else {
         c->pos = pos19;
         ParserContext_backTree(c,left20);
         ParserContext_backLog(c,log21);
      }
   }
   if (temp) {
      const unsigned char * pos22 = c->pos;
      size_t left23 = ParserContext_saveTree(c);
      size_t log24 = ParserContext_saveLog(c);
      // { ![0-9] !(("else" ![$0-9A-Z_a-z] / "function" ![$0-9A-Z_a-z] / "if" ![$0-9A-Z_a-z] / "return" ![$0-9A-Z_a-z] / "var" ![$0-9A-Z_a-z]) ![$0-9A-Z_a-z]) [$0-9A-Z_a-z]+ #Name } ~_
      if (e10(c)) {
         temp = 0;
      } else {
         c->pos = pos22;
         ParserContext_backTree(c,left23);
         ParserContext_backLog(c,log24);
      }
   }
   if (temp) {
      return 0;
   }
   return 1;
}
// { ([1-9] [0-9]* / '0') #Integer } ~_ / "true" ![$0-9A-Z_a-z] { #True } ~_ / "false" ![$0-9A-Z_a-z] { #False } ~_ / '"' { [\x01-\t\x0b-!#-[\]-\xff]* #String } '"' ~_ / "null" ![$0-9A-Z_a-z] { #Null } ~_ / '(' ~_ Expression ')' ~_ / "function" { ![$0-9A-Z_a-z] ~_ ($name(Identifier) ~_)? '(' ~_ $list(FunctionParamList) ')' ~_ $body(Block) "::" ~_ #FunctionExpr } / { ![0-9] !(("else" ![$0-9A-Z_a-z] / "function" ![$0-9A-Z_a-z] / "if" ![$0-9A-Z_a-z] / "return" ![$0-9A-Z_a-z] / "var" ![$0-9A-Z_a-z]) ![$0-9A-Z_a-z]) [$0-9A-Z_a-z]+ #Name } ~_
static inline int pPrimaryExpression(ParserContext * c) {
   int memo = ParserContext_memoLookupTree(c,3);
   if (memo == 0) {
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      if (e33(c)) {
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
// PrimaryExpression ({$recv _FunctionCall })+
int e32(ParserContext * c) {
   if (!pPrimaryExpression(c)) {
      return 0;
   }
   if (!e44(c)) {
      return 0;
   }
   while (1) {
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      // {$recv _FunctionCall }
      if (!e44(c)) {
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         break;
      }
   }
   return 1;
}
// PrimaryExpression ({$recv _FunctionCall })+ / '"' { [\x01-\t\x0b-!#-[\]-\xff]* #String } '"' ~_
int e31(ParserContext * c) {
   int temp = 1;
   if (temp) {
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      // PrimaryExpression ({$recv _FunctionCall })+
      if (e32(c)) {
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
      // '"' { [\x01-\t\x0b-!#-[\]-\xff]* #String } '"' ~_
      if (e39(c)) {
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
// '!' { !'=' ~_ $expr(UnaryExpression) #Not } / (PrimaryExpression ({$recv _FunctionCall })+ / '"' { [\x01-\t\x0b-!#-[\]-\xff]* #String } '"' ~_) / (PrimaryExpression ({$recv _FunctionCall })+ / { ![0-9] !(("else" ![$0-9A-Z_a-z] / "function" ![$0-9A-Z_a-z] / "if" ![$0-9A-Z_a-z] / "return" ![$0-9A-Z_a-z] / "var" ![$0-9A-Z_a-z]) ![$0-9A-Z_a-z]) [$0-9A-Z_a-z]+ #Name } ~_) / (PrimaryExpression ({$recv _FunctionCall })+ / '(' ~_ Expression ')' ~_) / (PrimaryExpression ({$recv _FunctionCall })+ / { ([1-9] [0-9]* / '0') #Integer } ~_) / (PrimaryExpression ({$recv _FunctionCall })+ / 'f' ("alse" ![$0-9A-Z_a-z] { #False } ~_ / "unction" { ![$0-9A-Z_a-z] ~_ ($name(Identifier) ~_)? '(' ~_ $list(FunctionParamList) ')' ~_ $body(Block) "::" ~_ #FunctionExpr }) / { ![0-9] !(("else" ![$0-9A-Z_a-z] / "function" ![$0-9A-Z_a-z] / "if" ![$0-9A-Z_a-z] / "return" ![$0-9A-Z_a-z] / "var" ![$0-9A-Z_a-z]) ![$0-9A-Z_a-z]) [$0-9A-Z_a-z]+ #Name } ~_) / (PrimaryExpression ({$recv _FunctionCall })+ / "null" ![$0-9A-Z_a-z] { #Null } ~_ / { ![0-9] !(("else" ![$0-9A-Z_a-z] / "function" ![$0-9A-Z_a-z] / "if" ![$0-9A-Z_a-z] / "return" ![$0-9A-Z_a-z] / "var" ![$0-9A-Z_a-z]) ![$0-9A-Z_a-z]) [$0-9A-Z_a-z]+ #Name } ~_) / (PrimaryExpression ({$recv _FunctionCall })+ / "true" ![$0-9A-Z_a-z] { #True } ~_ / { ![0-9] !(("else" ![$0-9A-Z_a-z] / "function" ![$0-9A-Z_a-z] / "if" ![$0-9A-Z_a-z] / "return" ![$0-9A-Z_a-z] / "var" ![$0-9A-Z_a-z]) ![$0-9A-Z_a-z]) [$0-9A-Z_a-z]+ #Name } ~_)
int pUnaryExpression(ParserContext * c) {
   int temp = 1;
   switch(_index9[ParserContext_prefetch(c)]) {
      case 0: 
      return 0;
      case 1: 
      // '!' { !'=' ~_ $expr(UnaryExpression) #Not }
      temp = e30(c);
      break;
      case 2: 
      // PrimaryExpression ({$recv _FunctionCall })+ / '"' { [\x01-\t\x0b-!#-[\]-\xff]* #String } '"' ~_
      temp = e31(c);
      break;
      case 3: 
      // PrimaryExpression ({$recv _FunctionCall })+ / { ![0-9] !(("else" ![$0-9A-Z_a-z] / "function" ![$0-9A-Z_a-z] / "if" ![$0-9A-Z_a-z] / "return" ![$0-9A-Z_a-z] / "var" ![$0-9A-Z_a-z]) ![$0-9A-Z_a-z]) [$0-9A-Z_a-z]+ #Name } ~_
      temp = e46(c);
      break;
      case 4: 
      // PrimaryExpression ({$recv _FunctionCall })+ / '(' ~_ Expression ')' ~_
      temp = e47(c);
      break;
      case 5: 
      // PrimaryExpression ({$recv _FunctionCall })+ / { ([1-9] [0-9]* / '0') #Integer } ~_
      temp = e48(c);
      break;
      case 6: 
      // PrimaryExpression ({$recv _FunctionCall })+ / 'f' ("alse" ![$0-9A-Z_a-z] { #False } ~_ / "unction" { ![$0-9A-Z_a-z] ~_ ($name(Identifier) ~_)? '(' ~_ $list(FunctionParamList) ')' ~_ $body(Block) "::" ~_ #FunctionExpr }) / { ![0-9] !(("else" ![$0-9A-Z_a-z] / "function" ![$0-9A-Z_a-z] / "if" ![$0-9A-Z_a-z] / "return" ![$0-9A-Z_a-z] / "var" ![$0-9A-Z_a-z]) ![$0-9A-Z_a-z]) [$0-9A-Z_a-z]+ #Name } ~_
      temp = e49(c);
      break;
      case 7: 
      // PrimaryExpression ({$recv _FunctionCall })+ / "null" ![$0-9A-Z_a-z] { #Null } ~_ / { ![0-9] !(("else" ![$0-9A-Z_a-z] / "function" ![$0-9A-Z_a-z] / "if" ![$0-9A-Z_a-z] / "return" ![$0-9A-Z_a-z] / "var" ![$0-9A-Z_a-z]) ![$0-9A-Z_a-z]) [$0-9A-Z_a-z]+ #Name } ~_
      temp = e53(c);
      break;
      case 8: 
      // PrimaryExpression ({$recv _FunctionCall })+ / "true" ![$0-9A-Z_a-z] { #True } ~_ / { ![0-9] !(("else" ![$0-9A-Z_a-z] / "function" ![$0-9A-Z_a-z] / "if" ![$0-9A-Z_a-z] / "return" ![$0-9A-Z_a-z] / "var" ![$0-9A-Z_a-z]) ![$0-9A-Z_a-z]) [$0-9A-Z_a-z]+ #Name } ~_
      temp = e54(c);
      break;
   }
   if (!temp) {
      return 0;
   }
   return 1;
}
// '=' !'=' ~_ #LessThanEquals
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
   ParserContext_tagTree(c,_TLessThanEquals);
   return 1;
}
// !'=' ~_ #LessThan
static inline int e70(ParserContext * c) {
   if (ParserContext_prefetch(c) == 61) {
      return 0;
   }
   if (!p__(c)) {
      return 0;
   }
   ParserContext_tagTree(c,_TLessThan);
   return 1;
}
// '<' (!'=' ~_ #LessThan / '=' !'=' ~_ #LessThanEquals)
static inline int e69(ParserContext * c) {
   if (ParserContext_read(c) != 60) {
      return 0;
   }
   int temp = 1;
   switch(_index12[ParserContext_prefetch(c)]) {
      case 0: 
      return 0;
      case 1: 
      // !'=' ~_ #LessThan
      temp = e70(c);
      break;
      case 2: 
      // '=' !'=' ~_ #LessThanEquals
      temp = e71(c);
      break;
   }
   if (!temp) {
      return 0;
   }
   return 1;
}
// {$left ('<' (!'=' ~_ #LessThan / '=' !'=' ~_ #LessThanEquals) / '>' (!'=' ~_ #GreaterThan / '=' !'=' ~_ #GreaterThanEquals)) $right(UnaryExpression) }
int e68(ParserContext * c) {
   ParserContext_foldTree(c,0,_Lleft);
   int temp = 1;
   switch(_index11[ParserContext_prefetch(c)]) {
      case 0: 
      return 0;
      case 1: 
      // '<' (!'=' ~_ #LessThan / '=' !'=' ~_ #LessThanEquals)
      temp = e69(c);
      break;
      case 2: 
      // '>' (!'=' ~_ #GreaterThan / '=' !'=' ~_ #GreaterThanEquals)
      temp = e72(c);
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
// UnaryExpression ({$left ('<' (!'=' ~_ #LessThan / '=' !'=' ~_ #LessThanEquals) / '>' (!'=' ~_ #GreaterThan / '=' !'=' ~_ #GreaterThanEquals)) $right(UnaryExpression) })*
int pRelationalExpression(ParserContext * c) {
   if (!pUnaryExpression(c)) {
      return 0;
   }
   while (1) {
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      // {$left ('<' (!'=' ~_ #LessThan / '=' !'=' ~_ #LessThanEquals) / '>' (!'=' ~_ #GreaterThan / '=' !'=' ~_ #GreaterThanEquals)) $right(UnaryExpression) }
      if (!e68(c)) {
         c->pos = pos;
         ParserContext_backTree(c,left);
         ParserContext_backLog(c,log);
         break;
      }
   }
   return 1;
}
// {$left ("!=" ~_ #NotEquals / "==" ~_ #Equals) $right(RelationalExpression) }
int e75(ParserContext * c) {
   ParserContext_foldTree(c,0,_Lleft);
   int temp = 1;
   switch(_index13[ParserContext_prefetch(c)]) {
      case 0: 
      return 0;
      case 1: 
      // "!=" ~_ #NotEquals
      temp = e76(c);
      break;
      case 2: 
      // "==" ~_ #Equals
      temp = e77(c);
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
// $(("function" { ![$0-9A-Z_a-z] ~_ $name(Identifier) ~_ '(' ~_ $list(FunctionParamList) ')' ~_ $body(Block) #FunctionDecl } / "var" { ![$0-9A-Z_a-z] ~_ $list(VariableList) ';' ~_ #Declaration } / { #Empty } ';' ~_))
static inline int e8(ParserContext * c) {
   size_t left = ParserContext_saveTree(c);
   {
      int temp = 1;
      if (temp) {
         const unsigned char * pos = c->pos;
         size_t left3 = ParserContext_saveTree(c);
         size_t log = ParserContext_saveLog(c);
         // "function" { ![$0-9A-Z_a-z] ~_ $name(Identifier) ~_ '(' ~_ $list(FunctionParamList) ')' ~_ $body(Block) #FunctionDecl }
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
         // "var" { ![$0-9A-Z_a-z] ~_ $list(VariableList) ';' ~_ #Declaration }
         if (e104(c)) {
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
         if (e82(c)) {
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
// $(("function" { ![$0-9A-Z_a-z] ~_ $name(Identifier) ~_ '(' ~_ $list(FunctionParamList) ')' ~_ $body(Block) #FunctionDecl } / "var" { ![$0-9A-Z_a-z] ~_ $list(VariableList) ';' ~_ #Declaration } / { #Empty } ';' ~_)) / ([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))*
static inline int e7(ParserContext * c) {
   int temp = 1;
   if (temp) {
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      // $(("function" { ![$0-9A-Z_a-z] ~_ $name(Identifier) ~_ '(' ~_ $list(FunctionParamList) ')' ~_ $body(Block) #FunctionDecl } / "var" { ![$0-9A-Z_a-z] ~_ $list(VariableList) ';' ~_ #Declaration } / { #Empty } ';' ~_))
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
// ~_ { ($(("function" { ![$0-9A-Z_a-z] ~_ $name(Identifier) ~_ '(' ~_ $list(FunctionParamList) ')' ~_ $body(Block) #FunctionDecl } / "var" { ![$0-9A-Z_a-z] ~_ $list(VariableList) ';' ~_ #Declaration } / { #Empty } ';' ~_)) / ([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))*)* #Source } !.
static inline int pFile(ParserContext * c) {
   if (!p__(c)) {
      return 0;
   }
   ParserContext_beginTree(c,0);
   while (1) {
      const unsigned char * pos = c->pos;
      size_t left = ParserContext_saveTree(c);
      size_t log = ParserContext_saveLog(c);
      // $(("function" { ![$0-9A-Z_a-z] ~_ $name(Identifier) ~_ '(' ~_ $list(FunctionParamList) ')' ~_ $body(Block) #FunctionDecl } / "var" { ![$0-9A-Z_a-z] ~_ $list(VariableList) ';' ~_ #Declaration } / { #Empty } ';' ~_)) / ([\t-\r ] / '/' ('*' (!"*/" .)* "*/" / '/' [\x01-\t\x0b-\xff]*))*
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
void* epsilon_parse(const char *text, size_t len, void *thunk, void* (*fnew)(symbol_t, const unsigned char *, size_t, size_t, void *), void  (*fset)(void *, size_t, symbol_t, void *, void *), void  (*fgc)(void *, int, void *)) {
   void* result = NULL;
   ParserContext * c = ParserContext_new((const unsigned char*)text, len);
   ParserContext_initTreeFunc(c,thunk,fnew,fset,fgc);
   ParserContext_initMemo(c,64,8);
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
   return epsilon_parse(text, len, NULL, NULL, NULL, NULL);
}
long epsilon_match(const char *text, size_t len) {
   long result = -1;
   ParserContext * c = ParserContext_new((const unsigned char*)text, len);
   ParserContext_initNoTreeFunc(c);
   ParserContext_initMemo(c,64,8);
   if (pFile(c)) {
      result = c->pos-c->inputs;
   }
   ParserContext_free(c);
   return result;
}
const char* epsilon_tag(symbol_t n) {
   return _tags[n];
}
const char* epsilon_label(symbol_t n) {
   return _labels[n];
}
#ifndef UNUSE_MAIN
int main(int ac, const char **argv) {
   return cnez_main(ac, argv, cnez_parse);
}
#endif/*MAIN*/
// End of File
