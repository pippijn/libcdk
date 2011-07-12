#pragma once

#include "common.h"

BEGIN_AST_NAMESPACE

struct VISIBLE node
{
  node ();
  node (location const &loc);
  ~node ();

  static bool audit_hash ();
  static void compress_hash ();
  static size_t node_count ();
  static size_t hash_size ();

  int refcnt;

  PIMPL;
};

END_AST_NAMESPACE
