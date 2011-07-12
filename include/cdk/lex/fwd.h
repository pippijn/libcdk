#pragma once

#include "common.h"

typedef struct yy_buffer_state *YY_BUFFER_STATE;
typedef void *yyscan_t;

BEGIN_LEX_NAMESPACE

struct file_input_manager;
struct input_manager;
struct lexer;
struct string_input_manager;
struct yyvtable;

END_LEX_NAMESPACE

typedef cdk::lex::lexer *YY_EXTRA_TYPE;
#define YY_EXTRA_TYPE YY_EXTRA_TYPE
