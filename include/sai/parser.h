#ifndef SAI_PARSER_H
#define SAI_PARSER_H

#include "sai/node.h"

typedef struct saiParserContext {

} saiParserContext;

int yyparse(saiParserContext *c);
int yylex(void *lval, saiParserContext *c);
void yyerror(saiParserContext *c, const char *s);
void yywarn(saiParserContext *c, const char *s);

#endif
