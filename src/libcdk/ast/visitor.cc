#include "cdk/ast/visitor.h"

#include "cdk/ast/generic_node.h"
#include "cdk/ast/merge_node.h"

#include "src/ast/ast.h"

BEGIN_AST_NAMESPACE

visitor::~visitor ()
{
}

#include "src/ast/node_visit.cc"

void
visitor::visit (generic_token &n)
{
}

void
visitor::visit (generic_node &n)
{
  for (node_ptr const &p : n.list)
    p->accept (*this);
}


void
visitor::visit (merge_node &n)
{
  visit (static_cast<generic_node &> (n));
}

END_AST_NAMESPACE
