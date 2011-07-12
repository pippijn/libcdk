#pragma once

#include "common.h"

class QString;

BEGIN_LEX_NAMESPACE

struct VISIBLE input_manager
{
  virtual bool load (char const *&data, size_t &length) = 0;
  virtual QString current_file () const = 0;
};

END_LEX_NAMESPACE
