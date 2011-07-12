#pragma once

#include "common.h"

#include "cdk/ast/fwd.h"

VISIBLE void yyerror (cdk::ast::location const *llocp, cdk::parse::parser *parse, char const *msg);
