#include "parseutil.h"

int
main (int argc, char *argv[])
{
  parse_string ("int main;");
  if (argv[1])
    parse_file (argv[1]);
  return 0;
}
