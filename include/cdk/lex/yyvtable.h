#pragma once

#include "common.h"

BEGIN_LEX_NAMESPACE

struct yyvtable
{
  int (*lex_init_extra) (lexer *self, yyscan_t *yyscanner);
  int (*lex_destroy) (yyscan_t yyscanner);

  FILE *(*get_in) (yyscan_t yyscanner);
  void (*set_in) (FILE *in_str, yyscan_t yyscanner);

  int (*get_column) (yyscan_t yyscanner);
  void (*set_column) (int column_number, yyscan_t yyscanner);

  int (*get_lineno) (yyscan_t yyscanner);
  void (*set_lineno) (int line_number, yyscan_t yyscanner);

  YY_BUFFER_STATE (*scan_bytes) (char const *bytes, int len, yyscan_t yyscanner);

  int (*tables_fload) (FILE * fp ,yyscan_t yyscanner);
  int (*tables_destroy) (yyscan_t yyscanner);
};

END_LEX_NAMESPACE
