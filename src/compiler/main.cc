#include "parseutil.h"

#include <QString>

void print (cdk::ast::node_ptr n);

int
main (int argc, char *argv[])
{
  //extern int yydebug;
  //yydebug = 1;
  parse_string ("int main;");
  if (argv[1])
    {
      print (parse_file (argv[1]));
    }
  return 0;
}
