#include "parseutil.h"

#include "t_lexer.h"
#include "t_parser.h"

#include "cdk/basic/fileref.h"
#include "cdk/basic/source_manager.h"
#include "cdk/lex/file_input_manager.h"
#include "cdk/lex/string_input_manager.h"

#include <QFileInfo>
#include <QString>

static cdk::ast::node_ptr
parse (cdk::lex::lexer &lex)
{
  return t_parser (lex) ();
}


cdk::ast::node_ptr
parse_string (QString const &s, int init)
{
  cdk::lex::string_input_manager im (s);
  t_lexer lex (im);
  lex.init (init);
  return parse (lex);
}

static bool
is_c_file (QFileInfo const &info)
{
  QString const &ext = info.suffix ();
  return ext == "cc"
      || ext == "c"
      || ext == "ii"
      || ext == "i"
      ;;
}

cdk::ast::node_ptr
parse_file (std::string const &s, int init)
{
  cdk::basic::source_manager sm;
  if (!sm.add_recursive (QString::fromStdString (s)))
    return 0;
  cdk::basic::fileref_vec const &files = sm.find (is_c_file);
  if (files.empty ())
    return 0;
  printf ("parsing %lu files\n", files.size ());
  cdk::lex::file_input_manager im (sm, files);
  t_lexer lex (im);
  lex.init (init);
  return parse (lex);
}
