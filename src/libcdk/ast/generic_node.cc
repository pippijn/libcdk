#include "cdk/ast/generic_node.h"

BEGIN_AST_NAMESPACE

generic_node::generic_node (location const &loc, node_type type)
  : node (loc)
  , type (type)
{
}

generic_node::~generic_node ()
{
}

generic_node &
generic_node::add (node_ptr n)
{
  list.push_back (n);
  return *this;
}

END_AST_NAMESPACE
