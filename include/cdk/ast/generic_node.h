#pragma once

#include "cdk/ast/node.h"

#include <memory>

enum node_type : int;
template<typename T>
struct node_type_enum;

BEGIN_AST_NAMESPACE

struct VISIBLE generic_node
  : node
{
  typedef ast::location location;

  generic_node (location const &loc, node_type type);
  ~generic_node ();

  generic_node &add (node_ptr n);

  std::vector<node_ptr> list;
  node_type type;
};

template<typename T>
static inline void
fill_node (T &n)
{ }

template<typename First, typename... Rest>
static inline void
fill_node (generic_node &n, First const &first, Rest const &...rest)
{
  n.add (first);
  return fill_node (n, rest...);
}

template<typename T, typename... Nodes>
static inline T *
make_node (location const &loc, Nodes const &...children)
{
  std::auto_ptr<T> n (new T (loc));
  fill_node (*n, children...);
  return n.release ();
}

END_AST_NAMESPACE
