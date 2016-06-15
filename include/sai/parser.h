#ifndef SAI_PARSER_H
#define SAI_PARSER_H

#include "sai/state.h"
#include "sai/node.h"
#include "sai/string.h"

struct sai_parser {
  sai_state *state;
  sai_node *ast;
  sai_string source;
  sai_number start;
  sai_number end;
};

int yyparse(sai_parser *p);
int yylex(void *lval, sai_parser *p);
void yyerror(sai_parser *p, const char *s);
void yywarn(sai_parser *p, const char *s);
sai_parser *sai_parser_new(void);

#endif
