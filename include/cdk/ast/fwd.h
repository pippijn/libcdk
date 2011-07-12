#pragma once

#include "common.h"

#include <vector>

#include <boost/intrusive_ptr.hpp>

BEGIN_AST_NAMESPACE

#define NODE(T) struct T; typedef boost::intrusive_ptr<T> T##_ptr; typedef std::vector<T##_ptr> T##_vec

NODE (node);
NODE (generic_node);
NODE (generic_token);

VISIBLE void intrusive_ptr_release (node *n);
VISIBLE void intrusive_ptr_add_ref (node *n);

struct location;

END_AST_NAMESPACE
