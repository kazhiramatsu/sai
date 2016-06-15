#ifndef SAI_NODE_H
#define SAI_NODE_H

#include "sai/string.h"
#include "sai/number.h"

typedef struct sai_parser sai_parser;

typedef struct sai_position  {
  sai_number line;
  sai_number column;
} sai_position;

typedef struct sai_source_location  {
  sai_string source;
  sai_position start;
  sai_position end;
} sai_source_location;

typedef struct sai_node {
  sai_string type;
  sai_source_location loc;
} sai_node;

enum sai_source_type {
  SAI_SOURCE_TYPE_SCRIPT,
  SAI_SOURCE_TYPE_MODULE,
};

typedef struct sai_node_program {
  sai_node base;
  sai_string source_type;
  sai_node *body;
} sai_node_program;

typedef struct sai_node_function {
  sai_node base;
} sai_node_function;

sai_node *sai_node_program_new(sai_parser *p, sai_string type, sai_source_location loc,
                               sai_string source_type, sai_node *body);

#endif
