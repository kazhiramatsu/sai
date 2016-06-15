#ifndef SAI_VALUE_H
#define SAI_VALUE_H

#include "sai/sai.h"

typedef uint64_t sai_value;

#define SAI_TAG_NAN         UINT64_C(0xFFF0000000000000)
#define SAI_VALUE_MASK      UINT64_C(0x0000FFFFFFFFFFFF)
#define SAI_TAG_STRING      UINT64_C(0x0001000000000000)
#define SAI_TAG_ARRAY       UINT64_C(0x0002000000000000)
#define SAI_TAG_FUNCTION    UINT64_C(0x0003000000000000)
#define SAI_TAG_OBJECT      UINT64_C(0x0004000000000000)
#define SAI_TAG_SYMBOL      UINT64_C(0x0005000000000000)
#define SAI_TAG_NULL        UINT64_C(0x0006000000000000)
#define SAI_TAG_UNDEFINED   UINT64_C(0x0007000000000000)
#define SAI_TYPE_MASK       UINT64_C(0x0008000000000000)

#define sai_type(data) (((uint64_t)(data) & SAI_TAG_NAN) == SAI_TAG_NAN) * (((uint64_t)data & SAI_TYPE_MASK) >> 48)

#define sai_check_type(o, T) do {					\
		if (sai_type(o) != T) {						\
			fprintf(stderr, "type = [%llu]\n", sai_type(o));	\
			assert(0 && "Type Error!\n");		\
		}										\
  } while (0)

enum sai_zone {
  SAI_ZONE_YOUNG,
};

enum sai_type {
  SAI_TYPE_NUMBER,
  SAI_TYPE_STRING,
  SAI_TYPE_ARRAY,
  SAI_TYPE_FUNCTION,
  SAI_TYPE_OBJECT,
  SAI_TYPE_SYMBOL,
  SAI_TYPE_NULL,
  SAI_TYPE_UNDEFINED,
};

typedef struct sai_object_header sai_object_header;

struct sai_object_header {
  unsigned int type:8;
  unsigned int zone:2;
  unsigned int age:4;
  unsigned int forwarded:1;
  unsigned int remembered:1;
  unsigned int marked:2;
  unsigned int require_cleanup:1;
  unsigned int refs_are_weak:1;
  sai_object_header *next;
};

typedef struct sai_object {
  sai_object_header header;
} sai_object;

typedef uint32_t sai_instruction;

#endif
