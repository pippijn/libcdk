#pragma once

#include "common.h"

class QString;

BEGIN_AST_NAMESPACE

struct location
{
  QString const *file;
  int first_line;
  int first_column;
  int last_line;
  int last_column;

  static location const generated;
};

END_AST_NAMESPACE
