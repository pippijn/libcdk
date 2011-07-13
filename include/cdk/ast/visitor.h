#pragma once

#include "common.h"

#include "src/ast/ast_fwd.h"

BEGIN_AST_NAMESPACE

struct VISIBLE visitor
{
  virtual ~visitor ();

#include "src/ast/node_visit.h"

  virtual void visit (ast::generic_node &n);
  virtual void visit (ast::generic_token &n);

  virtual void visit (ast::merge_node &n);
};

END_AST_NAMESPACE
