#include "cdk/ast/generic_node.h"
#include "cdk/ast/generic_token.h"
#include "cdk/ast/location.h"
#include "cdk/lex/lexer.h"

static int
yylex (YYSTYPE *yylval, YYLTYPE *yylloc, parser *self)
{
  return self->lex ().next (yylval, yylloc);
}

/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
 * If N is 0, then set CURRENT to the empty location which ends
 * the previous symbol: RHS[0] (always defined).  */
#define YYLLOC_DEFAULT(Current, Rhs, N)					\
    do {								\
      (Current) = YYRHSLOC (Rhs, 1);					\
      if (YYID (N))							\
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    } while (YYID (0))

/* YY_LOCATION_PRINT -- Print the location on the stream. */
#define YY_LOCATION_PRINT(File, Loc)			\
    fprintf (File, "%d.%d-%d.%d",			\
	     (Loc).first_line, (Loc).first_column,	\
	     (Loc).last_line,  (Loc).last_column)

using cdk::ast::make_node;
