#include "cdk/ast/node.h"

#include <algorithm>

BEGIN_AST_NAMESPACE

void intrusive_ptr_release (node *n) { if (!n->refcnt--) delete n; }
void intrusive_ptr_add_ref (node *n) { ++n->refcnt; }


struct node::pimpl
{
  size_t index;
};


static std::vector<node *> nodes;

node::node ()
  : refcnt (0)
  , self (new pimpl)
{
  self->index = nodes.size ();
  nodes.push_back (this);
}

node::~node ()
{
  if (this == nodes.back ())
    nodes.pop_back ();
  else
    nodes[self->index] = 0;
}

void
node::compress_hash ()
{
  for (size_t i = 0; i < nodes.size (); i++)
    {
      if (!nodes[i])
        {
          while (!nodes.empty () && !nodes.back ())
            nodes.pop_back ();
          if (!nodes.empty ())
            {
              (nodes[i] = nodes.back ())->self->index = i;
              nodes.pop_back ();
            }
        }
    }

  assert (node_count () == hash_size ());
  assert (audit_hash ());
}

bool
node::audit_hash ()
{
  std::vector<node *>::const_reverse_iterator it = nodes.rbegin ();
  std::vector<node *>::const_reverse_iterator et = nodes.rend ();

  while (it != et)
    {
      if (*it && (*it)->self->index != et - it - 1)
        return false;
      ++it;
    }
  return true;
}

static bool
not_null (node *p)
{
  return p != NULL;
}

size_t
node::node_count ()
{
  return count_if (nodes.begin (), nodes.end (), not_null);
}

size_t
node::hash_size ()
{
  return nodes.size ();
}

END_AST_NAMESPACE
