#include "sai/parser.h"

sai_parser *
sai_parser_new(void)
{
  sai_parser *p = malloc(sizeof(struct sai_parser));
  p->ast = NULL;
  return p;
}

