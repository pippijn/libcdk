#pragma once

#include "common.h"

BEGIN_AST_NAMESPACE

struct VISIBLE node
{
  node ();
  node (location const &loc);
  virtual ~node ();

  virtual void accept (visitor &v) = 0;

  template<typename T> T &as () { return dynamic_cast<T &> (*this); }
  template<typename T> T *is () { return dynamic_cast<T *> ( this); }

  static bool audit_hash ();
  static void compress_hash ();
  static size_t node_count ();
  static size_t hash_size ();

  int refcnt;

  PIMPL;
};

END_AST_NAMESPACE
