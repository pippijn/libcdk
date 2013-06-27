#include "cdk/parse/parser.h"

#include "cdk/ast/location.h"

#include "ylcode.h"

#include <QString>

BEGIN_PARSE_NAMESPACE

struct parser::pimpl
{
  lex::lexer &lex;

  pimpl (lex::lexer &lex)
    : lex (lex)
  {
  }
};

parser::parser (lex::lexer &lex)
  : self (new pimpl (lex))
{
}

parser::~parser ()
{
}


lex::lexer &
parser::lex ()
{
  return self->lex;
}

void
parser::error (cdk::ast::location const &llocp, char const *msg)
{
  printf ("%s:%d:%d: error: \"%s\"\n",
          llocp.file ? llocp.file->toStdString ().c_str () : "<unknown>",
          llocp.first_line,
          llocp.first_column,
          msg);
  exit (EXIT_FAILURE);
}

END_PARSE_NAMESPACE


void
yyerror (cdk::ast::location const *llocp, cdk::parse::parser *parser, char const *msg)
{
  parser->error (*llocp, msg);
}
