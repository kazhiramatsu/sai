#ifndef SAI_NODE_H
#define SAI_NODE_H

#include "sai/string.h"
#include "sai/number.h"

typedef struct SaiPosition  {
  SaiNumber line;
  SaiNumber column;
} SaiPosition;

typedef struct SaiSourceLocation  {
  SaiString source;
  SaiPosition start;
  SaiPosition end;
} SaiSourceLocation;

typedef struct SaiNode {
  SaiString type;
  SaiSourceLocation loc;
} SaiNode;

enum SaiSourceType {
  SAI_SOURCE_TYPE_SCRIPT,
  SAI_SOURCE_TYPE_MODULE,
};

typedef struct SaiProgramNode {
  SaiNode base;
  enum SaiSourceType sourceType;
  SaiString *body;
} SaiProgramNode;

typedef struct SaiFunctionNode {
  SaiNode base;
} SaiFunctionNode;

#endif
