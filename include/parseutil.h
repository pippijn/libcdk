#pragma once

#include "cdk/ast/fwd.h"

cdk::ast::node_ptr parse_string (std::string const &s, int init = 0);
cdk::ast::node_ptr parse_file (std::string const &s, int init = 0);
