#pragma once

#include <cerrno>
#include <cstddef>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <string>

#include <inttypes.h>

#include "cdk/ast/fwd.h"
#include "cdk/lex/fwd.h"
#include "cdk/parse/yyerror.h"

#include "yyparse.h"
#ifndef FLEX_SCANNER
#include "yylex.h"
#endif

struct t_parser;
typedef short yySymbol;

VISIBLE char const *tokname (yySymbol yytoken);
