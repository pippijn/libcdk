#include "t_lexer.h"

#include "cdk/lex/lexer/pimpl.h"
#include "cdk/lex/yyvtable.h"
#include "cdk/parse/yyerror.h"

#include "yylex.h"
#include "yystate.h"

int yyget_column (yyscan_t yyscanner);
void yyset_column (int column_no, yyscan_t yyscanner);

static cdk::lex::yyvtable const yyvtbl = {
  yylex_init_extra,
  yylex_destroy,

  yyget_in,
  yyset_in,

  yyget_column,
  yyset_column,

  yyget_lineno,
  yyset_lineno,

  yy_scan_bytes,

  yytables_fload,
  yytables_destroy,
};


t_lexer::t_lexer (cdk::lex::input_manager &im)
  : lexer (yyvtbl, im)
{
}

int
t_lexer::wrap ()
{
  if (state () != yy::INITIAL)
    {
      std::string msg = "end of file ";
      msg += strstate (state ());
      yyerror (self->loc, 0, msg.c_str ());
    }

  return lexer::wrap ();
}
