#include "cdk/ast/merge_node.h"

#include "cdk/ast/location.h"
#include "cdk/ast/visitor.h"

#include <QString>

BEGIN_AST_NAMESPACE

static QString const merge_file = "<merge>";
static location merge_location = { &merge_file, 0, 0, 0 };


void
merge_node::accept (visitor &v)
{
  v.visit (*this);
}


merge_node::merge_node (node_ptr n0, node_ptr n1)
  : generic_node (merge_location, node_type (-1))
{
  add (n0);
  add (n1);
}

merge_node::~merge_node ()
{
}

END_AST_NAMESPACE
