#include "cdk/basic/diagnostic_kind.h"

#include <cassert>

using namespace cdk::basic;

enum compiler_diagnostic_kind
{
  err_test = DC_ERROR (100),
  warn_test = DC_WARNING (100),
  note_test = DC_NOTE (100),
};

XENUM (diagnostic_kind_base, compiler_diagnostic_kind)

int
main ()
{
  assert (is_error (err_test));
  assert (!is_warning (err_test));
  assert (!is_note (err_test));

  assert (!is_error (warn_test));
  assert (is_warning (warn_test));
  assert (!is_note (warn_test));

  assert (!is_error (note_test));
  assert (!is_warning (note_test));
  assert (is_note (note_test));

  return 0;
}
