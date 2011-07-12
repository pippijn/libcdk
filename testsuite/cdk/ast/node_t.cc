#include "cdk/ast/node.h"

using namespace cdk::ast;

int
main ()
{
  node n;

  assert (node::audit_hash ());
  assert (node::node_count () == 1);
  assert (node::hash_size () == 1);
  assert (n.refcnt == 0);

  node_ptr p = new node;

  assert (node::audit_hash ());
  assert (node::node_count () == 2);
  assert (node::hash_size () == 2);
  assert (p->refcnt == 1);
}
