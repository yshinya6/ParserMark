#include "xml.h"

XML *xml_parser(FILE *fp) {
	extern FILE *yyin;
	extern XML *xml;
  xml = malloc(sizeof(struct XML));

	yyin = fp;

	if (yyparse()) {
		return NULL;
	}

	return xml;
}

XML *xml_new_value() {
	XML *value = (XML *)malloc(sizeof(XML));
	return value;
}

XML *xml_new_string(char *str) {
	XML *value = xml_new_nstring(str, strlen(str));
  value->type = XML_VALUE_TYPE_STRING;
  return value;
}

XML *xml_new_nstring(char *str, int size) {
	XML *value = xml_new_value();

	value->s_value = (char *)malloc(size + 1);
	strncpy(value->s_value, str, size);
	value->s_value[size] = '\0';

	return value;
}

XML *xml_new_name(char *str) {
  XML *value = xml_new_nstring(str, strlen(str));
  value->type = XML_VALUE_TYPE_NAME;
  return value;
}

XML *xml_new_text(char *str) {
  XML *value = xml_new_nstring(str, strlen(str));
  value->type = XML_VALUE_TYPE_TEXT;
  return value;
}

XML *xml_new_cdata(char *str) {
  XML *value = xml_new_nstring(str, strlen(str));
  value->type = XML_VALUE_TYPE_CDATA;
  return value;
}

XML *xml_new_prolog_text(char *str) {
  XML *value = xml_new_nstring(str, strlen(str));
  value->type = XML_VALUE_TYPE_PROLOG;
  return value;
}

XML *xml_new_dtd_text(char *str) {
  XML *value = xml_new_nstring(str, strlen(str));
  value->type = XML_VALUE_TYPE_DTD;
  return value;
}

XML *new_XML() {
  XML *xml = (XML *)malloc(sizeof(XML));
  return xml;
}

XML *xml_add_values(XML *xml, XML *prolog, XML *dtd, XML *element) {
  if (!xml) {
    return NULL;
  }

  xml->prolog = prolog;
  xml->dtd = dtd;
  xml->element = element;
  return xml;
}

void xml_free_xml(XML *xml) {
  if (!xml) {
    return;
  }
  if (xml->prolog) {
    xml_free_prolog(xml->prolog);
  }
  if (xml->dtd) {
    xml_free_dtd(xml->dtd);
  }
  if (xml->element) {
    xml_free_element(xml->element);
  }
  free(xml);
}

void xml_print_xml(FILE *fp, XML *xml, int depth) {
  if (!xml) {
    return;
  }
  xml_print_indent(fp, depth);
  fprintf(fp, "{#XML\n");
  xml_print_prolog(fp, xml->prolog, depth+1);
  xml_print_dtd(fp, xml->dtd, depth+1);
  xml_print_element(fp, xml->element, depth+1);
  xml_print_indent(fp, depth);
  fprintf(fp, "}\n");
}

XML *xml_new_element_value(XML *element) {
	XML *value;

	if (!element) {
		return NULL;
	}

	value = xml_new_value();
	value->type    = XML_VALUE_TYPE_ELEMENT;
	value->e_value = element;

	return value;
}

void xml_free_value(XML *value) {
	if (!value) {
		return;
	}

	if (value->type == XML_VALUE_TYPE_STRING) {
		free(value->s_value);
	} else if (value->type == XML_VALUE_TYPE_ELEMENT) {
		xml_free_element(value->e_value);
	}
	free(value);
}

void xml_print_value(FILE *fp, XML *value, int depth) {
  if (!value) {
    return;
  }
  xml_print_indent(fp, depth);
  fprintf(fp, "{#Value ");
  switch (value->type) {
    case XML_VALUE_TYPE_STRING:
      fprintf(fp, "\"%s\"", value->s_value);
      free(value->s_value);
      break;
    case XML_VALUE_TYPE_ELEMENT:
      fprintf(fp, "\n");
      xml_print_element(fp, value->e_value, depth+1);
      xml_print_indent(fp, depth);
      break;
    case XML_VALUE_TYPE_TEXT:
      fprintf(fp, "\n");
      xml_print_indent(fp, depth+1);
      fprintf(fp, "{#Text %s }\n", value->s_value);
      xml_print_indent(fp, depth);
      break;
    case XML_VALUE_TYPE_NAME:
      fprintf(fp, "\n");
      xml_print_indent(fp, depth+1);
      fprintf(fp, "{#NAME %s }\n", value->s_value);
      xml_print_indent(fp, depth);
      break;
    case XML_VALUE_TYPE_CDATA:
      fprintf(fp, "\n");
      xml_print_indent(fp, depth+1);
      fprintf(fp, "{#CDATA %s }\n", value->s_value);
      xml_print_indent(fp, depth);
      break;
    case XML_VALUE_TYPE_PROLOG:
      fprintf(fp, "\n");
      xml_print_indent(fp, depth+1);
      fprintf(fp, "{#PROLOG %s }\n", value->s_value);
      xml_print_indent(fp, depth);
      break;
    case XML_VALUE_TYPE_DTD:
      fprintf(fp, "\n");
      xml_print_indent(fp, depth+1);
      fprintf(fp, "{#DTD %s }\n", value->s_value);
      xml_print_indent(fp, depth);
      break;
  }
  fprintf(fp, "}\n");
}

XML *xml_new_prolog(XML *value) {
  XML *prolog = (XML *)malloc(sizeof(XML));
  prolog->value = value;
  return prolog;
}

void xml_free_prolog(XML *prolog) {
  if (!prolog) {
    return;
  }

  xml_free_value(prolog->value);
  free(prolog);
}

void xml_print_prolog(FILE *fp, XML *prolog, int depth) {
  if (!prolog) {
    return;
  }
  xml_print_indent(fp, depth);
  fprintf(fp, "{#Prolog\n");
  xml_print_value(fp, prolog->value, depth+1);
  xml_print_indent(fp, depth);
  fprintf(fp, "}\n");
}

XML *xml_new_dtd(XML *value) {
  XML *dtd = (XML *)malloc(sizeof(XML));
  dtd->value = value;
  return dtd;
}

void xml_free_dtd(XML *dtd) {
  if (!dtd) {
    return;
  }

  xml_free_value(dtd->value);
  free(dtd);
}

void xml_print_dtd(FILE *fp, XML *dtd, int depth) {
  if (!dtd) {
    return;
  }
  xml_print_indent(fp, depth);
  fprintf(fp, "{#DTD\n");
  xml_print_value(fp, dtd->value, depth+1);
  xml_print_indent(fp, depth);
  fprintf(fp, "}\n");
}

XML *xml_new_element(XML *name, XML *attr, XML *content) {
	XML *element = (XML *)malloc(sizeof(XML));

	element->name = name;
	element->attr = attr;
  element->content = content;

	return element;
}

XML *xml_new_element_not_content(XML *name, XML *attr) {
  XML *element = (XML *)malloc(sizeof(XML));

  element->name = name;
  element->attr = attr;
  element->content = NULL;

  return element;
}

void xml_free_element(XML *element) {
	if (!element) {
		return;
	}

  xml_free_value(element->name);
	xml_free_attr(element->attr);
  if (element->content) {
    xml_free_content(element->content);
  }
	free(element);
}

void xml_print_element(FILE *fp, XML *element, int depth) {
  if (!element) {
    return;
  }
  xml_print_indent(fp, depth);
  fprintf(fp, "{#Element \n");
  xml_print_value(fp, element->name, depth+1);
  xml_print_attr(fp, element->attr, depth+1);
  xml_print_content(fp, element->content, depth+1);
  xml_print_indent(fp, depth);
  fprintf(fp, "}\n");
}

XML *xml_new_content(XML *value) {
  XML *content = (XML *)malloc(sizeof(XML));

  content->value = value;
  return content;
}

XML *xml_value_add_content(XML *content, XML *value) {
  XML *c;

  if (!content) {
    return xml_new_content(value);
  }

  //for (c = content; c->next; c = c->next) {}
  c = content;
  if (c) {
    return xml_new_content(value);
  }
  while (c->next) {
    c = c->next;
  }
	c->next = xml_new_content(value);

  return content;
}
void xml_free_content(XML *content) {
  if (!content) {
    return;
  }

  xml_free_value(content->value);
  xml_free_content(content->next);
  free(content);
}
void xml_print_content(FILE *fp, XML *content, int depth) {
  if (!content) {
    return;
  }
  xml_print_indent(fp, depth);
  fprintf(fp, "{#Content \n");
  xml_print_value(fp, content->value, depth+1);
  xml_print_indent(fp, depth);
  fprintf(fp, "}\n");
  xml_print_content(fp, content->next, depth);
}

XML *xml_new_attr(XML *name, XML *value) {
	XML *attr = (XML *)malloc(sizeof(XML));
	attr->name = name;
  attr->value = value;

	return attr;
}

void xml_free_attr(XML *attr) {
	if (!attr) {
		return;
	}
  if (attr->value) {
    xml_free_value(attr->name);
  }
  if (attr->value) {
    xml_free_value(attr->value);
  }
	xml_free_attr(attr->next);
	free(attr);
}

void xml_print_attr(FILE *fp, XML *attr, int depth) {
  if (!attr) {
    return;
  }
	xml_print_indent(fp, depth); fprintf(fp, "{#Attr\n");
  xml_print_value(fp, attr->name, depth+1);
  xml_print_value(fp, attr->value, depth+1);
	xml_print_indent(fp, depth); fprintf(fp, "}\n");
  xml_print_attr(fp, attr->next, depth);
}

XML *xml_value_add_attr(XML *attr, XML *name, XML *value) {
  XML *a;

  if (!attr) {
    return xml_new_attr(name, value);
  }

  for (a = attr; a->next; a = a->next) {}
	a->next = xml_new_attr(name, value);

  return attr;
}

void xml_print(FILE *fp, XML *xml) {
	xml_print_xml(fp, xml, 0);
	fprintf(fp, "\n");
}

void xml_print_indent(FILE *fp, int depth) {
	int i;

	for (i = 0; i < depth; i++) {
		fprintf(fp, XML_INDENT_STR);
	}
}
