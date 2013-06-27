#pragma once

#include "cdk/ast/generic_node.h"

BEGIN_AST_NAMESPACE

struct VISIBLE merge_node
  : generic_node
{
  virtual void accept (visitor &v);

  merge_node (node_ptr n0, node_ptr n1);
  ~merge_node ();
};

END_AST_NAMESPACE
