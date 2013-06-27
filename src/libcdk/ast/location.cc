#include "cdk/ast/location.h"

#include <QString>

BEGIN_AST_NAMESPACE

static QString const generated_file = "<generated>";

location const location::generated = { &generated_file, 0, 0, 0, 0 };

END_AST_NAMESPACE
