#pragma once

#include "cdk/ast/node.h"

BEGIN_AST_NAMESPACE

struct VISIBLE generic_token
  : node
{
  generic_token ();
  generic_token (location const &loc, short tok, char const *text, int leng);
  generic_token (location const &loc, short tok, std::string &&text);
  ~generic_token ();

  PIMPL;
};

END_AST_NAMESPACE
