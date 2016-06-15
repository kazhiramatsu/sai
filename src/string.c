
#include "sai/string.h"

sai_string
sai_string_set_tag(struct sai_string *str)
{
  return (uint64_t)str | SAI_TAG_NAN | SAI_TAG_STRING;
}

sai_string
sai_string_new(sai_state *state, const char *s, size_t len)
{
  struct sai_string *str;

  char *buf = malloc(len+1);
  memcpy(buf, s, len);
  buf[len] = '\0';

  str = malloc(sizeof(struct sai_string));
  str->str = buf;
  str->fill = len;
  str->max = len+1;
  str->hash = 0;
  
  return sai_string_set_tag(str);
}

