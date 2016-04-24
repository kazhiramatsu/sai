#ifndef JS_NODE_H
#define JS_NODE_H

#include "js/string.h"
#include "js/number.h"

typedef struct JsPosition  {
  JsNumber line;
  JsNumber column;
} JsPosition;

typedef struct JsSourceLocation  {
  JsString source;
  JsPosition start;
  JsPosition end;
} JsSourceLocation;

typedef struct JsNode {
  JsString type;
  JsSourceLocation loc;
} JsNode;

typedef struct JsProgramNode {
  JsNode base;
  enum JsSourceType sourceType;
  JsNode *body;
} JsProgramNode;

typedef struct JsFunctionNode {
  JsNode base;
} JsFunctionNode;

#endif
