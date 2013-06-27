#pragma once

#include "common.h"

#include "cdk/ast/fwd.h"
#include "cdk/basic/diagnostic_kind.h"

BEGIN_BASIC_NAMESPACE

struct VISIBLE diagnostic
{
  diagnostic (ast::node_ptr node, diagnostic_kind kind, std::string const &message = "", std::string const &note = "");
  ~diagnostic ();

  ast::node_ptr const &node () const;
  diagnostic_kind kind () const;
  std::string const &message () const;
  std::string const &note () const;

  PIMPL;
};

END_BASIC_NAMESPACE
