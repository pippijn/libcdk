#pragma once

#include "cdk/lex/lexer.h"

struct t_lexer
  : cdk::lex::lexer
{
  t_lexer (cdk::lex::input_manager &im);

  virtual int lex (YYSTYPE *yylval, YYLTYPE *yylloc);
  virtual int wrap ();

  virtual int state () const;
  virtual void push_state (int state);
  virtual void pop_state ();
};
