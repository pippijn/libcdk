#pragma once

#include "cdk/ast/node.h"

BEGIN_AST_NAMESPACE

struct VISIBLE generic_node
  : node
{
  generic_node ();
  generic_node (location const &loc);
  ~generic_node ();

  PIMPL;
};

END_AST_NAMESPACE
