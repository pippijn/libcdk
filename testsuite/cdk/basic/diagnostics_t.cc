#include "cdk/basic/diagnostics.h"

#include "cdk/basic/diagnostic.h"

using namespace cdk::basic;

enum compiler_diagnostic_kind
{
  err_type_not_in_scope = DC_ERROR (100),

  warn_test_warning = DC_WARNING (100),
};

XENUM (diagnostic_kind_base, compiler_diagnostic_kind)

int
main ()
{
  diagnostics diags;

  diags.add (0, err_file_not_found, "foo", "moo");
  diags.add<diagnostic> (0, err_file_not_found, "foo", "moo");
  diags.add (0, err_type_not_in_scope, "foo", "moo");
  diags.add (0, warn_test_warning, "foo", "moo");

  assert (diags.has_errors ());
  assert (diags.has_diagnostics ());
  assert (diags.count () == 4);
  assert (diags.error_count () == 3);

  return 0;
}
