#include "cdk/ast/generic_token.h"

#include "cdk/ast/location.h"
#include "cdk/ast/visitor.h"

BEGIN_AST_NAMESPACE

struct generic_token::pimpl
{
  location loc;
  std::string text;
  short tok;

  pimpl (location const &loc, short tok, std::string &&text)
    : loc (loc)
    , text (text)
    , tok (tok)
  {
  }
};

void
generic_token::accept (visitor &v)
{
  v.visit (*this);
}


generic_token::generic_token ()
{
}

generic_token::generic_token (location const &loc, short tok, char const *text, int leng)
  : self (new pimpl (loc, tok, std::string (text, leng)))
{
}

generic_token::generic_token (location const &loc, short tok, std::string &&text)
  : self (new pimpl (loc, tok, move (text)))
{
}

generic_token::~generic_token ()
{
}


std::string const &
generic_token::text () const
{
  return self->text;
}

END_AST_NAMESPACE
