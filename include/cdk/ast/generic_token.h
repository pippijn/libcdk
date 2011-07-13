#pragma once

#include "cdk/ast/node.h"

BEGIN_AST_NAMESPACE

struct VISIBLE generic_token
  : node
{
  virtual void accept (visitor &v);

  generic_token ();
  generic_token (location const &loc, short tok, char const *text, int leng);
  generic_token (location const &loc, short tok, std::string &&text);
  ~generic_token ();

  std::string const &text () const;

  PIMPL;
};

END_AST_NAMESPACE
