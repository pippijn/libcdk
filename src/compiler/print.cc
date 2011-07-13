#include "cdk/ast/visitor.h"

#include "cdk/ast/generic_token.h"
#include "cdk/ast/merge_node.h"
#include "src/ast/ast.h"

using cdk::ast::node_ptr;
using cdk::ast::generic_node;
using cdk::ast::generic_token;
using cdk::ast::merge_node;
using cdk::ast::visitor;

struct printer
  : visitor
{
  unsigned int indent;

  printer () : indent (0) { }

  virtual void visit (generic_node &n)
  {
    if (n.type != -1)
      {
        char buf[BUFSIZ];
        lines.emplace_back (buf, snprintf (buf, sizeof buf, "%*s%s", 2 * indent++, "", node_type_name[n.type]));
      }
    visitor::visit (n);
    --indent;
  }

  virtual void visit (merge_node &n)
  {
    char buf[BUFSIZ];
    n.list[0]->accept (*this);
    lines.emplace_back (buf, snprintf (buf, sizeof buf, "%*s%s", 2 * indent, "", "- or -"));
    n.list[1]->accept (*this);
  }

  virtual void visit (generic_token &n)
  {
    char buf[BUFSIZ];
    for (size_t i = lines.size (); i > 0; --i)
      {
        std::string &line = lines[i - 1];
        if (line.size () > 2 * indent && line[2 * indent] == ' ')
          line[2 * indent] = '|';
        else
          break;
      }
    lines.emplace_back (buf, snprintf (buf, sizeof buf, "%*s\" %s \"", 2 * indent, "", n.text ().c_str ()));
  }

  std::vector<std::string> lines;
};

void
print (node_ptr n)
{
  return;
  if (!n)
    return void (puts ("(null)"));
  printer v;
  n->accept (v);

  for (std::string const &s : v.lines)
    puts (s.c_str ());
}
