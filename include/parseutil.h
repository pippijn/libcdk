#pragma once

#include "cdk/ast/fwd.h"

class QString;

cdk::ast::node_ptr parse_string (QString const &s, int init = 0);
cdk::ast::node_ptr parse_file (std::string const &s, int init = 0);
