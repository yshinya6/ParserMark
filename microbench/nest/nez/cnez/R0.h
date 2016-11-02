
typedef unsigned long int symbol_t;
#define MAXTAG 1
#define MAXLABEL 1
void* R0_parse(const char *text, size_t len, void *, void* (*fnew)(symbol_t, const char *, size_t, size_t, void *), void  (*fset)(void *, size_t, symbol_t, void *, void *), void  (*fgc)(void *, int, void *));
long R0_match(const char *text, size_t len);
const char* R0_tag(symbol_t n);
const char* R0_label(symbol_t n);
