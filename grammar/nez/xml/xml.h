
typedef unsigned long int symbol_t;
#define _Name ((symbol_t)1)
#define _Value ((symbol_t)2)
#define _Attr ((symbol_t)3)
#define _Text ((symbol_t)4)
#define _Element ((symbol_t)5)
#define _CDATA ((symbol_t)6)
#define MAXTAG 7
#define _key ((symbol_t)1)
#define _value ((symbol_t)2)
#define MAXLABEL 3
void* xml_parse(const char *text, size_t len, void *, void* (*fnew)(symbol_t, const char *, size_t, size_t, void *), void  (*fset)(void *, size_t, symbol_t, void *, void *), void  (*fgc)(void *, int, void *));
long xml_match(const char *text, size_t len);
const char* xml_tag(symbol_t n);
const char* xml_label(symbol_t n);
