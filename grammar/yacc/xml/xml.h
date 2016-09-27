#ifndef __XML_H__
#define __XML_H__

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

enum {
	XML_VALUE_TYPE_ELEMENT = 0,
	XML_VALUE_TYPE_STRING,
	XML_VALUE_TYPE_NAME,
	XML_VALUE_TYPE_ATTRIBUTE,
	XML_VALUE_TYPE_CDATA,
  XML_VALUE_TYPE_PROLOG,
  XML_VALUE_TYPE_DTD,
  XML_VALUE_TYPE_TEXT,
	ALL_XML_VALUE_TYPE_NUM,
};

#define XML_VALUE_NAME_ELEMENT    "ELEMENT"
#define XML_VALUE_NAME_STRING     "STRING"
#define XML_VALUE_NAME_NAME       "NAME"
#define XML_VALUE_NAME_ATTRIBUTE  "ATTRIBUTE"
#define XML_VALUE_NAME_CDATA      "CDATA"

#define IS_HEX_CHAR(c)     (isdigit(c) || (('a' <= c && 'f' >= c) || ('A' <= c && 'F' >= c)))
#define HEX_CHAR2INT(c)    (isdigit(c) ? c - '0' : ('a' <= c && 'f' >= c) || ('A' <= c && 'F' >= c) ? tolower(c) - 'a' + 10 : -1)

/* Debug */
#define XML_DEBUG_MODE         0
#define XML_DEBUG_OUT          stderr
#if XML_DEBUG_MODE
#define XML_DEBUG(str)    fprintf(XML_DEBUG_OUT, "%s(%d) : " str "\n", __FILE__, __LINE__)
#else
#define XML_DEBUG(str)
#endif
#define XML_INDENT_STR        "  "

/*static char XML_value_name[ALL_XML_VALUE_TYPE_NUM][8] = {
 XML_VALUE_NAME_INTEGER,
 XML_VALUE_NAME_DOUBLE,
 XML_VALUE_NAME_String,
 XML_VALUE_NAME_Object,
 XML_VALUE_NAME_Array,
 XML_VALUE_NAME_TRUE,
 XML_VALUE_NAME_FALSE,
 XML_VALUE_NAME_NULL,
 };*/

typedef struct XML XML;
typedef struct XMLElement XMLElement;
typedef struct XMLValue XMLValue;
typedef struct XMLAttr XMLAttr;
typedef struct XMLContent XMLContent;
typedef struct XMLProlog XMLProlog;
typedef struct XMLDtd XMLDtd;

typedef struct XMLToken XMLToken;

struct XML {
  XML *prolog;
  XML *dtd;
  XML *element;
  XML *name;
  XML *attr;
  XML *content;
  XML *value;
  XML *next;
  int type;
	union {
		char       *s_value;
		XML *e_value;
	};
};

struct XMLElement {
  XMLValue *name;
	XMLAttr *attr;
  XMLContent *content;
};

struct XMLProlog {
  XML *value;
};

struct XMLDtd {
  XML *value;
};

struct XMLAttr {
	XML *name;
	XML *value;
	XMLAttr *next;
};

struct XMLContent {
  XMLValue *value;
  XMLContent *next;
};

struct XMLValue {
	int type;
	union {
		char       *s_value;
		XMLElement *e_value;
	};
};

struct XMLToken {
	char *str;
	char *next;
	int  token;
	XMLValue *value;
};

int yylex();
int yyparse();

XML *xml_parser(FILE *fp);

XML *new_XML();
XML *xml_add_values(XML *xml, XML *prolog, XML *dtd, XML *element);
void xml_free_xml(XML *xml);
void xml_print_xml(FILE *fp, XML *xml, int depth);

XML *xml_new_value();
XML *xml_new_string(char *str);
XML *xml_new_nstring(char *str, int size);
XML *xml_new_name(char *str);
XML *xml_new_text(char *str);
XML *xml_new_cdata(char *str);
XML *xml_new_prolog_text(char *str);
XML *xml_new_dtd_text(char *str);
XML *xml_new_element_value(XML *element);
XML *xml_new_add_element_value(XML *value, XML *element);
XML *xml_new_attr_value(XML *attr);
void xml_free_value(XML *value);
void xml_print_value(FILE *fp, XML *value, int depth);

XML *xml_new_prolog(XML *value);
void xml_free_prolog(XML *prolog);
void xml_print_prolog(FILE *fp, XML *prolog, int depth);

XML *xml_new_dtd(XML *value);
void xml_free_dtd(XML *dtd);
void xml_print_dtd(FILE *fp, XML *dtd, int depth);

XML *xml_new_element(XML *name, XML *attr, XML *content);
XML *xml_new_element_not_content(XML *name, XML *attr);
void xml_free_element(XML *element);
void xml_print_element(FILE *fp, XML *element, int depth);

XML *xml_new_content(XML *value);
XML *xml_value_add_content(XML *content, XML *value);
void xml_free_content(XML *content);
void xml_print_content(FILE *fp, XML *content, int depth);

XML *xml_new_attr(XML *name, XML *value);
void xml_free_attr(XML *attr);
XML *xml_value_add_attr(XML *attr, XML *name, XML *value);
XML *xml_attr_add_attr(XML *attr, XML *value);
void xml_print_attr(FILE *fp, XML *attr, int depth);

void xml_print(FILE *fp, XML *xml);
void xml_print_indent(FILE *fp, int depth);

#endif
