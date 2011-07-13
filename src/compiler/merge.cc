#include "merge.h"

#include "cdk/ast/merge_node.h"

using cdk::ast::generic_node;

generic_node *
merge::operator () (YYSTYPE const &x0, YYSTYPE const &x1)
{
  assert (x0.any->is<generic_node> ());
  assert (x1.any->is<generic_node> ());
  return new cdk::ast::merge_node (x0.node, x1.node);
}
