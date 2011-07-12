#pragma once

#include "common.h"

BEGIN_BASIC_NAMESPACE

struct fileref
{
  friend struct source_manager;
private:
  fileref (size_t index)
    : index (index)
  { }

  size_t index;
};

END_BASIC_NAMESPACE
