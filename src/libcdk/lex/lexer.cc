#include "cdk/lex/lexer.h"

#include "cdk/ast/location.h"
#include "cdk/lex/input_manager.h"
#include "cdk/lex/yyvtable.h"

#include "src/compiler/rule_init.h"
#include "yyparse.h"
#include "yystate.h"

#include <cassert>
#include <climits>
#include <stdexcept>

#include "cdk/lex/lexer/pimpl.h"

BEGIN_LEX_NAMESPACE

char const *
lexer::STRSTATE (int state)
{
  switch (state)
    {
    case yy::INITIAL    : return "INITIAL";
    default             : return "<unknown>";
    }
}

char const *
lexer::strstate (int state)
{
  switch (state)
    {
    case yy::INITIAL    : return "in initial state";
    default             : return "<unknown>";
    }
}

lexer::lexer (yyvtable const &vtbl, input_manager &im)
  : vtbl (vtbl)
  , im (im)
  , self (new pimpl ())
{
  FILE *fh = fopen ("yyscanner.tables", "r");
  if (!fh)
    throw;

  if (vtbl.lex_init_extra (this, &yyscanner))
    throw;
  vtbl.set_in (NULL, yyscanner);

  if (vtbl.tables_fload (fh, yyscanner))
    throw;
  fclose (fh);

  if (wrap () == 1)
    throw std::invalid_argument ("input manager could not load data");
}

lexer::~lexer ()
{
  vtbl.tables_destroy (yyscanner);
  vtbl.lex_destroy (yyscanner);
}

void
lexer::init (int init)
{
  self->init = init;
}

int
lexer::INIT (int init)
{
  switch (init)
    {
    case r_document:
      push_state (yy::INITIAL);
      return R_DOCUMENT;
    }
  throw std::invalid_argument ("invalid rule initialisation");
}

int
lexer::next (YYSTYPE *yylval, YYLTYPE *yylloc)
{
  if (self->init)
    {
      int init = 0;
      std::swap (init, self->init);
      return INIT (init);
    }
  int tok = lex (yylval, yylloc);
#if LEXER_VERBOSE
  if (tok)
    printf ("%-16s: \"%s\"\n", tokname (tok), yylval->token->string.c_str ());
#endif

  return tok;
}

void
lexer::lloc (YYLTYPE *yylloc, int &lineno, int &column, char const *text, int leng)
{
  assert (yylloc != NULL);
  assert (lineno >= 1);
  assert (column >= 0);
  assert (leng >= 1);
  assert (UINT_MAX - column - leng > INT_MAX);

  if (column == 0)
    column = 1;

  yylloc->file = 0;
  yylloc->first_line = lineno;
  yylloc->first_column = column;

  for (char const *p = text; p != text + leng; ++p)
    if (*p == '\n')
      {
        lineno++;
        column = 1;
      }
    else
      column++;

  yylloc->last_line = lineno;
  yylloc->last_column = column;

  self->loc = yylloc;
}

int
lexer::wrap ()
{
  char const *data;
  size_t length;
  switch (im.load (data, length))
    {
    case 0:
      return 1;
    case 1:
      vtbl.scan_bytes (data, length, yyscanner);
      vtbl.set_lineno (1, yyscanner);
      vtbl.set_column (1, yyscanner);
      return 0;
    }

  throw std::invalid_argument ("invalid return from input_manager::load");
}

END_LEX_NAMESPACE
