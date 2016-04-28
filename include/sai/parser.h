#ifndef SAI_PARSER_H
#define SAI_PARSER_H

#include "sai/node.h"

typedef struct SaiParserContext {

} SaiParserContext;

int yyparse(SaiParserContext *c);
int yylex(void *lval, SaiParserContext *c);
void yyerror(SaiParserContext *c, const char *s);
void yywarn(SaiParserContext *c, const char *s);

#endif
