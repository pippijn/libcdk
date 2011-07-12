#pragma once

#include "common.h"

#include "cdk/parse/fwd.h"

class QString;

BEGIN_LEX_NAMESPACE

struct VISIBLE lexer
{
  lexer (yyvtable const &vtbl, input_manager &im);
  ~lexer ();

  int INIT (int init);
  void init (int init);

  virtual int lex (YYSTYPE *yylval, YYLTYPE *yylloc) = 0;
  int next (YYSTYPE *yylval, YYLTYPE *yylloc);
  void lloc (YYLTYPE *yylloc, int &lineno, int &column, char const *text, int leng);
  virtual int wrap ();

  static char const *STRSTATE (int state);
  static char const *strstate (int state);

  virtual int state () const = 0;
  virtual void push_state (int state) = 0;
  virtual void pop_state () = 0;


  yyscan_t yyscanner;
  yyvtable const &vtbl;
  input_manager &im;

  PIMPL;
};

END_LEX_NAMESPACE
