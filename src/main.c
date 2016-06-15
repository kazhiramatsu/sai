
#include "sai/sai.h"
#include "sai/parser.h"

int
main(int argc, char **argv)
{
  sai_parser p;
  yyparse(&p);

  return 0;
}

