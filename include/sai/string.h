#ifndef SAI_STRING_H
#define SAI_STRING_H

#include "sai/sai.h"

typedef uint64_t sai_string;

struct sai_string {
  const char *str;
  size_t fill;
  size_t len;
  size_t max;
  int hash;
};

sai_string sai_string_new(sai_state *state, const char *s, size_t len);
sai_string sai_string_set_tag(struct sai_string *str);

#endif
