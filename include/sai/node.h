#ifndef SAI_NODE_H
#define SAI_NODE_H

#include "sai/string.h"
#include "sai/number.h"

typedef struct saiPosition  {
  saiNumber line;
  saiNumber column;
} saiPosition;

typedef struct saiSourceLocation  {
  saiString source;
  saiPosition start;
  saiPosition end;
} saiSourceLocation;

typedef struct saiNode {
  saiString type;
  saiSourceLocation loc;
} saiNode;

enum saiSourceType {
  SAI_SOURCE_TYPE_SCRIPT,
  SAI_SOURCE_TYPE_MODULE,
};

typedef struct saiProgramNode {
  saiNode base;
  enum saiSourceType sourceType;
  saiString *body;
} saiProgramNode;

typedef struct saiFunctionNode {
  saiNode base;
} saiFunctionNode;

#endif
