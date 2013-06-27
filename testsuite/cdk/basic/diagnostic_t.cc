#include "cdk/basic/diagnostic.h"

using namespace cdk::basic;

int
main ()
{
  diagnostic d (0, err_file_not_found, "foo", "moo");

  assert (d.node () == 0);
  assert (d.kind () == err_file_not_found);
  assert (d.message () == "foo");
  assert (d.note () == "moo");

  return 0;
}
