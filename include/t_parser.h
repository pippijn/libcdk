#pragma once

#include "cdk/parse/parser.h"

struct t_parser
  : cdk::parse::parser
{
  t_parser (cdk::lex::lexer &lex)
    : parser (lex)
  {
  }

  cdk::ast::node_ptr operator () ()
  {
    extern int yyparse (t_parser *parse);
    yyparse (this);
    return doc;
  }

  cdk::ast::node_ptr doc;
};
