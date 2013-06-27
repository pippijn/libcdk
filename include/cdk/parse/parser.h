#pragma once

#include "common.h"

#include "cdk/ast/fwd.h"
#include "cdk/lex/fwd.h"

BEGIN_PARSE_NAMESPACE

struct VISIBLE parser
{
  parser (lex::lexer &lex);
  ~parser ();

  cdk::ast::node_ptr operator () ();

  lex::lexer &lex ();
  void error (ast::location const &lloc, char const *msg);

  ast::node_ptr doc;

  PIMPL;
};

END_PARSE_NAMESPACE
