#pragma once

#include "cdk/ast/fwd.h"

#include "yyparse.h"

static struct merge
{
  merge &operator () (int i) { return *this; }

  cdk::ast::generic_node *operator () (YYSTYPE const &x0, YYSTYPE const &x1);
} merge;
