#pragma once

#include "common.h"

#include "cdk/ast/fwd.h"

union YYSTYPE;
typedef cdk::ast::location YYLTYPE;
#define YYLTYPE_IS_DECLARED 1
#define YYLTYPE_IS_TRIVIAL 1

BEGIN_PARSE_NAMESPACE

struct parser;

END_PARSE_NAMESPACE
