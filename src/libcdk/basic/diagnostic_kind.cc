#include "cdk/basic/diagnostic_kind.h"

BEGIN_BASIC_NAMESPACE

diagnostic_class
classify (diagnostic_kind kind)
{
  return diagnostic_class (kind & DC_MASK);
}

bool
is_error (diagnostic_kind kind)
{
  return classify (kind) == DC_ERROR;
}

bool
is_warning (diagnostic_kind kind)
{
  return classify (kind) == DC_WARNING;
}

bool
is_note (diagnostic_kind kind)
{
  return classify (kind) == DC_NOTE;
}

END_BASIC_NAMESPACE
