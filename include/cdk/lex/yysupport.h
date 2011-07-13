#include "cdk/lex/lexer.h"
#include "cdk/lex/lexer/pimpl.h"
#include "cdk/ast/generic_token.h"

#define YY_USER_ACTION lloc (yylloc, yylineno, yycolumn, yytext, yyleng);

#define Return(TOK)					\
  return self->make_token<TOK> (yylval, yylloc, yytext, yyleng)

#define PUSH(STATE)	push_state (STATE)
#define POP()		pop_state ()

#define YY_DECL int t_lexer::lex (YYSTYPE *yylval_param, YYLTYPE *yylloc_param)

#define BACKTRACK(N) do {		\
  yycolumn -= yyleng - N;		\
  yyless (N);				\
} while (0)


template<short Tok>
int
cdk::lex::lexer::pimpl::make_token (YYSTYPE *lval, YYLTYPE const *lloc, char const *text, int leng)
{
  if (!this->text.empty ())
    lval->token = new cdk::ast::generic_token (*lloc, Tok, move (this->text));
  else
    lval->token = new cdk::ast::generic_token (*lloc, Tok, text, leng);
  return Tok;
}
