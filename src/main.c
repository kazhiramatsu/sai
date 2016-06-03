
#include "sai/sai.h"
#include "sai/parser.h"

int
main(int argc, char **argv)
{
  saiParserContext c;
  yyparse(&c);

  return 0;
}

