#ifndef SAI_STRING_H
#define SAI_STRING_H

#include "sai/sai.h"

typedef uint64_t saiString;

struct saiString {
  const char *buffer;
  size_t length;
};

#endif
