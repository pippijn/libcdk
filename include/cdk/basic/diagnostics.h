#pragma once

#include "common.h"

#include "cdk/ast/fwd.h"
#include "cdk/basic/diagnostic_kind.h"

BEGIN_BASIC_NAMESPACE

struct VISIBLE diagnostics
{
  diagnostics ();
  ~diagnostics ();

  void add (diagnostic *e);

  void add (ast::node_ptr const &node, diagnostic_kind kind, std::string const &message = "", std::string const &note = "");

  template<typename Diagnostic>
  void add (ast::node_ptr const &node, diagnostic_kind kind, std::string const &message = "", std::string const &note = "")
  {
    add (new Diagnostic (node, kind, message, note));
  }

  bool has_diagnostics () const;
  bool has_errors () const;
  size_t count () const;
  size_t error_count () const;

  PIMPL;
};

END_BASIC_NAMESPACE
