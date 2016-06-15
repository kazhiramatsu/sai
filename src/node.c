
#include "sai/parser.h"

sai_node *
sai_node_program_new(sai_parser *p, sai_string type, sai_source_location loc,
                     sai_string source_type, sai_node *body)
{
  sai_node_program *n = malloc(sizeof(sai_node_program));
  n->base.loc = loc;
  n->source_type = source_type;
  return (sai_node *)n;
}
